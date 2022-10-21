
make_plot_wrap <- function(dat, .scenario, french, scales = "fixed", ylim = NULL, ylab,
                           conf_int = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  if (is.null(conf_int)) {
    g <- mutate(dat, scenario = factor(scenario, levels = .scenario)) %>%
      ggplot(aes(year, value, group = iteration)) +
      geom_line(alpha = 0.05) +
      facet_wrap(vars(scenario), scales = scales) +
      theme_pbs() +
      labs(x = en2fr("Year", french), y = en2fr(ylab, french))
  } else {

    panel <- mutate(dat, scenario = factor(scenario, levels = .scenario)) %>%
      group_by(year, scenario) %>%
      summarise(lwr = quantile(value, probs = conf_int[1]),
                lwr2 = quantile(value, probs = conf_int[2]),
                mid = quantile(value, probs = conf_int[3]),
                upr2 = quantile(value, probs = conf_int[4]),
                upr = quantile(value, probs = conf_int[5]),)

    g <- panel %>%
      ggplot(aes(year, mid)) +
      geom_ribbon(fill = "grey90", colour = NA, inherit.aes = FALSE, aes(x = year, ymin = lwr, max = upr)) +
      geom_ribbon(fill = "grey70", colour = NA, inherit.aes = FALSE, aes(x = year, ymin = lwr2, max = upr2)) +
      geom_line() +
      facet_wrap(vars(scenario), scales = scales) +
      theme_pbs() +
      labs(x = en2fr("Year", french), y = en2fr(ylab, french))
  }

  if (!is.null(ylim)) g <- g + coord_cartesian(expand = FALSE, ylim = ylim)
  g
}

.rcm_SSB <- function(rcm, scenario, type = c("SSB", "depletion", "MSY"), get_medians = FALSE,
                     MPD = FALSE) {
  type <- match.arg(type)
  all_years <- seq(rcm@OM@CurrentYr - rcm@OM@nyears + 1, rcm@OM@CurrentYr)

  if (MPD) {
    if (!length(rcm@mean_fit)) stop("Can not make plot. Set MPD = FALSE", call. = FALSE)
    if (type == "depletion") out <- array(rcm@mean_fit$report$E / rcm@mean_fit$report$E0_SR, c(1, rcm@OM@nyears + 1))
    if (type == "SSB") out <- array(rcm@mean_fit$report$E, c(1, rcm@OM@nyears + 1))
    if (type == "MSY") {
      out <- array(rcm@mean_fit$report$E/.rcm_calc_MSY(rcm, MPD = MPD),
                   c(1, rcm@OM@nyears + 1)) # Calculate MSY using last historical year parameters
    }
  } else {
    if (type == "depletion") out <- rcm@SSB / sapply(rcm@Misc, getElement, "E0_SR")
    if (type == "SSB") out <- rcm@SSB
    if (type == "MSY") out <- rcm@SSB / .rcm_calc_MSY(rcm, MPD = MPD)
  }

  if (get_medians) {
    d1 <- apply(out[, -ncol(out), drop = FALSE], 2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      as.data.frame() %>%
      cbind(all_years) %>%
      mutate(scenario = scenario) %>%
      rename(lwr = 1, med = 2, upr = 3, year = all_years)

    d2 <- apply(out[, -ncol(out), drop = FALSE], 2, quantile, probs = c(0.25, 0.75)) %>%
      t() %>%
      as.data.frame() %>%
      cbind(all_years) %>%
      rename(lwr50 = 1, upr50 = 2, year = all_years)
    left_join(d1, d2, by = "year")
  } else {
    reshape2::melt(out[, -ncol(out), drop = FALSE]) %>%
      rename(iteration = Var1) %>%
      mutate(year = rep(all_years, each = max(iteration))) %>%
      mutate(scenario = scenario)
  }
}


.rcm_calc_MSY <- function(rcm, MPD = FALSE, type = "SSBMSY", y = rcm@OM@nyears) {
  type <- match.arg(type)
  nsim <- rcm@OM@nsim

  if (MPD) {
    rcm_report <- rcm@mean_fit$report

    MSYs <- sapply(1, MSEtool::optMSY_eq,
                   M_ageArray = apply(rcm@OM@cpars$M_ageArray, 2:3, mean) %>%
                     array(c(1, rcm@OM@maxage + 1, rcm@OM@nyears + rcm@OM@proyears)),
                   Wt_age = apply(rcm@OM@cpars$Wt_age, 2:3, mean) %>%
                     array(c(1, rcm@OM@maxage + 1, rcm@OM@nyears + rcm@OM@proyears)),
                   Mat_age = apply(rcm@OM@cpars$Mat_age, 2:3, mean) %>%
                     array(c(1, rcm@OM@maxage + 1, rcm@OM@nyears + rcm@OM@proyears)),
                   Fec_age = apply(rcm@OM@cpars$Wt_age * rcm@OM@cpars$Mat_age, 2:3, mean) %>%
                     array(c(1, rcm@OM@maxage + 1, rcm@OM@nyears + rcm@OM@proyears)),
                   V = array(rcm_report$F_at_age/apply(rcm_report$F_at_age, 1, max),
                             c(1, rcm@OM@nyears, rcm@OM@maxage + 1)) %>%
                     aperm(c(1, 3, 2)),
                   maxage = rcm@OM@maxage,
                   R0 = rcm_report[["R0"]],
                   SRrel = rcm@OM@SRrel,
                   hs = rcm_report[["h"]],
                   SSBpR = matrix(rcm_report[["EPR0_SR"]], 1, 1),
                   yr.ind = y,
                   plusgroup = 1)


  } else {
    phi0 <- sapply(rcm@Misc, getElement, "EPR0_SR")
    if(length(phi0) == 1) phi0 <- rep(phi0, nsim)

    MSYs <- sapply(1:nsim, MSEtool::optMSY_eq,
                   M_ageArray = rcm@OM@cpars$M_ageArray,
                   Wt_age = rcm@OM@cpars$Wt_age,
                   Mat_age = rcm@OM@cpars$Mat_age,
                   Fec_age = rcm@OM@cpars$Wt_age * rcm@OM@cpars$Mat_age,
                   V = rcm@OM@cpars$V,
                   maxage = rcm@OM@maxage,
                   R0 = rcm@OM@cpars$R0,
                   SRrel = rep(rcm@OM@SRrel, nsim),
                   hs = rcm@OM@cpars$hs,
                   SSBpR = matrix(phi0, nsim, 1),
                   yr.ind = y,
                   plusgroup = 1)
  }


  out <- switch(type,
                "SSBMSY" = MSYs["SB", ])

  return(out)
}

.rcm_F <- function(rcm, scenario, MPD) {
  if (MPD) {
    if (!length(rcm@mean_fit)) stop("Can not make plot. Set MPD = FALSE", call. = FALSE)
    .F1 <- rcm@mean_fit$report$F_at_age
    .F <- apply(.F1, 1, max) %>% matrix(1)

  } else {
    .F1 <- map(rcm@Misc, "F_at_age")
    .F <- sapply(.F1, apply, 1, max) %>% t()
  }
  row.names(.F) <- NULL

  all_years <- seq(rcm@OM@CurrentYr - rcm@OM@nyears + 1, rcm@OM@CurrentYr)

  reshape2::melt(.F) %>%
    rename(iteration = Var1) %>%
    mutate(year = rep(all_years, each = max(iteration))) %>%
    mutate(scenario = scenario)
}

.rcm_recdev <- function(x, scenario, proj = TRUE, logspace = FALSE, MPD) {
  max_age <- x@OM@maxage

  if (MPD) {
    if (!length(x@mean_fit)) stop("Can not make plot. Set MPD = FALSE", call. = FALSE)
    if (proj) message("No projection plotted with MPD = TRUE.")
    nyears <- x@OM@nyears
    all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)

    perr_y <- matrix(x@mean_fit$report$log_rec_dev, 1)

    if (logspace) {
      perr_y <- matrix(x@mean_fit$report$log_rec_dev, 1)
    } else {
      perr_y <- exp(x@mean_fit$report$log_rec_dev) *
        ifelse(x@mean_fit$obj$env$data$est_rec_dev, exp(-0.5 * x@mean_fit$report$tau^2), 1)
      perr_y <- matrix(perr_y, 1)
    }

  } else {
    if (proj) {
      nyears <- x@OM@nyears + x@OM@proyears
      all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr + x@OM@proyears)
    } else {
      nyears <- x@OM@nyears
      all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)
    }
    perr_y <- x@OM@cpars$Perr_y[, max_age + 1:nyears]

    if (logspace) { # Double-check for projected rec devs sampled with autocorrelation
      sigmaR <- vapply(x@Misc, function(xx) xx$tau, numeric(1))
      perr_y <- log(perr_y) + 0.5 * sigmaR^2
    }
  }

  reshape2::melt(perr_y) %>%
    rename(iteration = Var1) %>%
    mutate(year = rep(all_years, each = max(iteration))) %>%
    mutate(scenario = scenario)
}

.rcm_rec <- function(x, scenario, MPD) {
  if (MPD) {
    Rec <- x@mean_fit$report$R %>% matrix(x@OM@nyears + 1, 1)
  } else {
    Rec <- sapply(x@Misc, getElement, "R")
  }

  Rec %>%
    structure(dimnames = list(year = seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr + 1),
                              iteration = 1:ncol(Rec))) %>%
    reshape2::melt() %>%
    mutate(scenario = scenario)
}

.rcm_bio_sel <- function(rcm, scenario, bio_type, sel_type, sel_i, sel_name) {
  if (!length(rcm@mean_fit)) stop("Re-run RCMs with argument mean_fit = TRUE")
  nyears <- rcm@OM@nyears
  maxage <- rcm@OM@maxage
  if ("LAA" %in% bio_type) {
    LAA <- data.frame(
      Age = 0:maxage, Value = rcm@OM@cpars$Len_age[1, , nyears],
      Type = "Relative length"
    ) %>% mutate(Value = Value / max(Value))
  } else {
    LAA <- data.frame()
  }

  if ("mat" %in% bio_type) {
    mat <- data.frame(
      Age = 0:maxage, Value = rcm@OM@cpars$Mat_age[1, , nyears],
      Type = "Maturity"
    )
  } else {
    mat <- data.frame()
  }

  if (sel_type == "fleet") {
    sel_val <- rcm@mean_fit$report$vul[nyears, , sel_i]
  } else {
    sel_val <- rcm@mean_fit$report$ivul[nyears, , sel_i]
  }
  sel <- data.frame(Age = 0:maxage, Value = sel_val, Type = sel_name)
  dat <- rbind(LAA, mat, sel) %>% mutate(scenario = scenario)
  dat
}

.rcm_index <- function(rcm, .scenario, .index_names, i, MPD) {
  out2 <- purrr::map(1:length(i), function(ii) {
    if(MPD) {
      if (!length(rcm@mean_fit)) stop("Can not make plot. Set MPD = FALSE", call. = FALSE)
      y <- list(rcm@mean_fit$report)
    } else {
      y <- rcm@Misc
    }
    surveys <- purrr::map(y, ~ .$Ipred[, i[ii]])
    do.call(cbind, surveys) %>%
      reshape2::melt() %>%
      rename(year = Var1, iter = Var2) %>%
      mutate(scenario = .scenario, index_names = .index_names[ii])
  })
  bind_rows(out2)
}


.rcm_sel_multi <- function(rcm, .scenario, type = c("fleet", "index"), i, i_names) {
  type <- match.arg(type)
  nyears <- rcm@OM@nyears

  if (!length(rcm@mean_fit)) stop("Can not use plot_rcm_sel_multi. Try plot_rcm_sel", call. = FALSE)

  if (type == "fleet") {
    out <- rcm@mean_fit$report$vul[nyears, , ][, i, drop = FALSE]
  } else {
    out <- rcm@mean_fit$report$ivul[nyears, , ][, i, drop = FALSE]
  }
  structure(out, dimnames = list(NULL, i_names)) %>%
    reshape2::melt() %>%
    rename(Age = Var1, Type = Var2) %>%
    mutate(Age = Age - 1, scenario = .scenario)
}

.rcm_sel <- function(rcm, .scenario, type = c("fleet", "index"), i, MPD) {
  nyears <- rcm@OM@nyears
  if (MPD) {
    if (!length(rcm@mean_fit)) stop("Can not make plot. Set MPD = FALSE", call. = FALSE)
    y <- list(rcm@mean_fit$report)
  } else {
    y <- rcm@Misc
  }
  if (type == "fleet") {

    if (is.character(i) && i == "all") {
      sel <- sapply(1:length(y), function(x) y[[x]]$F_at_age[nyears, ]/max(y[[x]]$F_at_age[nyears, ]))
    } else {
      sel <- sapply(1:length(y), function(x) y[[x]]$vul[nyears, , i])
    }

  } else {
    if (is.character(i)) stop("i needs to be an integer for type = \"index\".")
    sel <- sapply(1:length(y), function(x) y[[x]]$ivul[nyears, , i])
  }

  sel %>%
    reshape2::melt() %>%
    rename(Age = Var1, iter = Var2) %>%
    mutate(Age = Age - 1, scenario = .scenario)
}

.rcm_mean_age <- function(rcm, .scenario = "", type = c("fleet", "index"), i, type2 = c("obs", "pred"),
                          MPD = FALSE) {
  type <- match.arg(type)
  type2 <- match.arg(type2)
  age <- 0:rcm@OM@maxage
  all_years <- seq(rcm@OM@CurrentYr - rcm@OM@nyears + 1, rcm@OM@CurrentYr)

  if(MPD) {
    if (!length(rcm@mean_fit)) stop("Can not make plot. Set MPD = FALSE", call. = FALSE)
    y <- list(rcm@mean_fit$report)
  } else {
    y <- rcm@Misc
  }

  if (type == "fleet") {
    if (type2 == "pred") {
      out <- purrr::map(1:length(y), function(ii) {
        data.frame(
          year = all_years,
          value = apply(y[[ii]]$CAApred[, , i], 1, function(x) weighted.mean(age, x)),
          iter = ii, scenario = .scenario
        )
      })
    } else {
      out <- data.frame(
        year = all_years,
        value = apply(rcm@data@CAA[, , i], 1, function(x) weighted.mean(age, x))
      )
    }
  } else {
    if (type2 == "pred") {
      out <- purrr::map(1:length(y), function(ii) {
        data.frame(
          year = all_years,
          value = apply(y[[ii]]$IAApred[, , i], 1, function(x) weighted.mean(age, x)),
          iter = ii, scenario = .scenario
        )
      })
    } else {
      out <- data.frame(
        year = all_years,
        value = apply(rcm@data@IAA[, , i], 1, function(x) weighted.mean(age, x))
      )
    }
  }
  if (is.list(out)) out <- bind_rows(out)
  return(out)
}


.rcm_mean_length <- function(rcm, .scenario = "", type = c("fleet", "index"), i, type2 = c("obs", "pred"),
                             MPD = FALSE) {
  type <- match.arg(type)
  type2 <- match.arg(type2)
  length_bin <- rcm@data@length_bin
  all_years <- seq(rcm@OM@CurrentYr - rcm@OM@nyears + 1, rcm@OM@CurrentYr)

  if(MPD) {
    if (!length(rcm@mean_fit)) stop("Can not make plot. Set MPD = FALSE", call. = FALSE)
    y <- list(rcm@mean_fit$report)
  } else {
    y <- rcm@Misc
  }

  if (type == "fleet") {
    if (type2 == "pred") {
      out <- purrr::map(1:length(y), function(ii) {
        data.frame(
          year = all_years,
          value = apply(y[[ii]]$CALpred[, , i], 1, function(x) weighted.mean(length_bin, x)),
          iter = ii, scenario = .scenario
        )
      })
    } else {
      out <- data.frame(
        year = all_years,
        value = apply(rcm@data@CAL[, , i], 1, function(x) weighted.mean(length_bin, x))
      )
    }
  } else {
    if (type2 == "pred") {
      out <- purrr::map(1:length(y), function(ii) {
        data.frame(
          year = all_years,
          value = apply(y[[ii]]$IALpred[, , i], 1, function(x) weighted.mean(length_bin, x)),
          iter = ii, scenario = .scenario
        )
      })
    } else {
      out <- data.frame(
        year = all_years,
        value = apply(rcm@data@IAL[, , i], 1, function(x) weighted.mean(length_bin, x))
      )
    }
  }
  if (is.list(out)) out <- bind_rows(out)
  return(out)
}
