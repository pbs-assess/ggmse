
make_plot_wrap <- function(dat, .scenario, FRENCH, scales = "fixed", ylim = NULL, ylab) {
  g <- mutate(dat, scenario = factor(scenario, levels = .scenario)) %>%
    ggplot(aes(year, value, group = iteration)) +
    geom_line(alpha = 0.05) +
    facet_wrap(vars(scenario), scales = scales) +
    theme_pbs() +
    labs(x = en2fr("Year", FRENCH), y = en2fr(ylab, FRENCH))
  if(!is.null(ylim)) g <- g + coord_cartesian(expand = FALSE, ylim = ylim)
  g
}

.rcm_SSB <- function(x, scenario, mse = NULL, type = c("SSB", "depletion", "MSY"), get_medians = FALSE) {
  type <- match.arg(type)
  all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)

  if(type == "depletion") out <- x@SSB / sapply(x@Misc, getElement, "E0_SR")
  if(type == "SSB") out <- x@SSB
  if(type == "MSY") out <- x@SSB / mse@RefPoint$SSBMSY[, 1, mse@nyears]

  if(get_medians) {
    d1 <- apply(out[, -ncol(out)], 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% t() %>%
      as.data.frame() %>%
      cbind(all_years) %>%
      mutate(scenario = scenario) %>%
      rename(lwr = 1, med = 2, upr = 3, year = all_years)

    d2 <- apply(out[, -ncol(out)], 2, quantile, probs = c(0.25, 0.75)) %>% t() %>%
      as.data.frame() %>%
      cbind(all_years) %>%
      rename(lwr50 = 1, upr50 = 2, year = all_years)
    left_join(d1, d2, by = "year")
  } else {
    reshape2::melt(out[, -ncol(out)]) %>%
      rename(iteration = Var1) %>%
      mutate(year = rep(all_years, each = max(iteration))) %>%
      mutate(scenario = scenario)
  }
}

.rcm_F <- function(x, scenario) {
  .F1 <- map(x@Misc, "F_at_age")
  .F <- map_dfc(.F1, ~tibble(.F = apply(.x, 1, max))) %>% as.matrix() %>% t()
  row.names(.F) <- NULL

  all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)

  reshape2::melt(.F) %>%
    rename(iteration = Var1) %>%
    mutate(year = rep(all_years, each = max(iteration))) %>%
    mutate(scenario = scenario)
}

.rcm_recdev <- function(x, scenario, proj = TRUE, logspace = FALSE) {
  max_age <- x@OM@maxage
  if(proj) {
    nyears <- x@OM@nyears+x@OM@proyears
    perr_y <- x@OM@cpars$Perr_y[, max_age + 1:nyears]
    all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr+x@OM@proyears)
  } else {
    nyears <- x@OM@nyears
    perr_y <- x@OM@cpars$Perr_y[, max_age + 1:nyears]
    all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)
  }
  if(logspace) { # Double-check for projected rec devs sampled with autocorrelation
    sigmaR <- vapply(x@Misc, function(xx) xx$tau, numeric(1))
    perr_y <- log(perr_y) + 0.5 * sigmaR^2
  }
  reshape2::melt(perr_y) %>%
    rename(iteration = Var1) %>%
    mutate(year = rep(all_years, each = max(iteration))) %>%
    mutate(scenario = scenario)
}

.rcm_bio_sel <- function(rcm, scenario, bio_type, sel_type, sel_i, sel_name) {

  if(!length(rcm@mean_fit)) stop("Re-run RCMs with argument mean_fit = TRUE")
  nyears <- rcm@OM@nyears
  maxage <- rcm@OM@maxage
  if("LAA" %in% bio_type) {
    LAA <- data.frame(Age = 0:maxage, Value = rcm@OM@cpars$Len_age[1, , nyears],
                      Type = "Relative length") %>% mutate(Value = Value/max(Value))
  } else {
    LAA <- data.frame()
  }

  if("mat" %in% bio_type) {
    mat <- data.frame(Age = 0:maxage, Value = rcm@OM@cpars$Mat_age[1, , nyears],
                      Type = "Maturity")
  } else {
    mat <- data.frame()
  }

  if(sel_type == "fleet") {
    sel_val <- rcm@mean_fit$report$vul[nyears, , sel_i]
  } else {
    sel_val <- rcm@mean_fit$report$ivul[nyears, , sel_i]
  }
  sel <- data.frame(Age = 0:maxage, Value = sel_val, Type = sel_name)
  dat <- rbind(LAA, mat, sel) %>% mutate(scenario = scenario)
  dat
}

.rcm_index <- function(x, .scenario, .index_names, i) {
  out2 <- purrr::map(1:length(i), function(ii) {
    surveys <- purrr::map(x@Misc, ~ .$Ipred[, i[ii]])
    do.call(cbind, surveys) %>% reshape2::melt() %>%
      rename(year = Var1, iter = Var2) %>%
      mutate(scenario = .scenario, index_names = .index_names[ii])
  })
  bind_rows(out2)
}


.rcm_sel_multi <- function(rcm, .scenario, type = c("fleet", "index"), i, i_names) {
  type <- match.arg(type)
  nyears <- rcm@OM@nyears

  if(!length(rcm@mean_fit)) stop("Can not use plot_rcm_sel_multi. Try plot_rcm_sel", call. = FALSE)

  if(type == "fleet") {
    out <- rcm@mean_fit$report$vul[nyears, , ][, i, drop = FALSE]
  } else {
    out <- rcm@mean_fit$report$ivul[nyears, , ][, i, drop = FALSE]
  }
  structure(out, dimnames = list(NULL, i_names)) %>%
    reshape2::melt() %>%
    rename(Age = Var1, Type = Var2) %>%
    mutate(Age = Age - 1, scenario = .scenario)
}

.rcm_sel <- function(rcm, .scenario, type = c("fleet", "index"), i) {
  nyears <- rcm@OM@nyears
  if(type == "fleet") {
    sel <- lapply(1:length(rcm@Misc), function(x) rcm@Misc[[x]]$vul[nyears, , i])
  } else {
    sel <- lapply(1:length(rcm@Misc), function(x) rcm@Misc[[x]]$ivul[nyears, , i])
  }

  do.call(cbind, sel) %>% reshape2::melt() %>% rename(Age = Var1, iter = Var2) %>%
    mutate(Age = Age - 1, scenario = .scenario)
}

.rcm_mean_age <- function(rcm, .scenario = "", type = c("fleet", "index"), i, type2 = c("obs", "pred")) {
  type <- match.arg(type)
  type2 <- match.arg(type2)
  age <- 0:rcm@OM@maxage
  all_years <- seq(rcm@OM@CurrentYr - rcm@OM@nyears + 1, rcm@OM@CurrentYr)

  if(type == "fleet") {

    if(type2 == "pred") {
      out <- purrr::map(1:length(rcm@Misc), function(ii) {
        data.frame(year = all_years,
                   value = apply(rcm@Misc[[ii]]$CAApred[,,i], 1, function(x) weighted.mean(age, x)),
                   iter = i, scenario = .scenario)
      })
    } else {
      out <- data.frame(year = all_years,
                        value = apply(rcm@data@CAA[,,i], 1, function(x) weighted.mean(age, x)))
    }


  } else {

    if(type2 == "pred") {
      out <- purrr::map(1:length(rcm@Misc), function(ii) {
        data.frame(year = all_years,
                   value = apply(rcm@Misc[[ii]]$IAApred[,,i], 1, function(x) weighted.mean(age, x)),
                   iter = i, scenario = .scenario)
      })
    } else {
      out <- data.frame(year = all_years,
                        value = apply(rcm@data@IAA[,,i], 1, function(x) weighted.mean(age, x)))
    }

  }
  if(is.list(out)) out <- bind_rows(out)
  return(out)
}


.rcm_mean_length <- function(rcm, .scenario = "", type = c("fleet", "index"), i, type2 = c("obs", "pred")) {
  type <- match.arg(type)
  type2 <- match.arg(type2)
  length_bin <- rcm@data@length_bin
  all_years <- seq(rcm@OM@CurrentYr - rcm@OM@nyears + 1, rcm@OM@CurrentYr)

  if(type == "fleet") {

    if(type2 == "pred") {
      out <- purrr::map(1:length(rcm@Misc), function(ii) {
        data.frame(year = all_years,
                   value = apply(rcm@Misc[[ii]]$CALpred[,,i], 1, function(x) weighted.mean(length_bin, x)),
                   iter = i, scenario = .scenario)
      })
    } else {
      out <- data.frame(year = all_years,
                        value = apply(rcm@data@CAL[,,i], 1, function(x) weighted.mean(length_bin, x)))
    }


  } else {

    if(type2 == "pred") {
      out <- purrr::map(1:length(rcm@Misc), function(ii) {
        data.frame(year = all_years,
                   value = apply(rcm@Misc[[ii]]$IALpred[,,i], 1, function(x) weighted.mean(length_bin, x)),
                   iter = i, scenario = .scenario)
      })
    } else {
      out <- data.frame(year = all_years,
                        value = apply(rcm@data@IAL[,,i], 1, function(x) weighted.mean(length_bin, x)))
    }

  }
  if(is.list(out)) out <- bind_rows(out)
  return(out)
}

