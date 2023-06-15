
#' @name plot_rcm
#' @title Plotting functions from OM conditioning
#' @description A set of functions that plots output from a list of RCModel objects used to condition a set
#' of operating models.
NULL

#' @describeIn plot_rcm Plot historical SSB
#' @param rcm A list containing \linkS4class{RCModel} objects.
#' @param scenario A character vector of names corresponding to \code{rcm}.
#' @param french Logical, whether the axes are in French or not.
#' @param scales The scales argument to \link[ggplot2]{facet_wrap}.
#' @param ylim Optional y-axes limits to figures.
#' @param MPD Logical, whether to plot individual simulations (\code{FALSE}) or from the single fit (\code{TRUE}) in \code{RCModel@mean_fit}.
#' @export
plot_rcm_SSB <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE,
                         scales = "fixed", ylim = NULL, MPD = FALSE) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_SSB, type = "SSB", MPD = MPD)

  if (is.null(ylim)) ylim <- c(0, 1.1 * max(dat$value))
  make_plot_wrap(dat, scenario, french, scales, ylim = ylim) +
    labs(y = en2fr("Spawning biomass"))
}

#' @describeIn plot_rcm Plot SSB/SSBMSY
#' @param histogram Logical, whether to show a histogram (TRUE) for the terminal year only. Otherwise, a time series plot.
#' @param bins The `bins` argument to \link{ggplot2}[geom_histogram]
#' @importFrom ggplot2 geom_histogram
#' @export
plot_rcm_SSBMSY <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE,
                            scales = "fixed", histogram = FALSE, bins = NULL, MPD = FALSE) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_SSB, type = "MSY", MPD = MPD)

  if (histogram) {
    .scenario <- scenario
    dat_hist <- dat %>% dplyr::filter(year == max(year)) %>%
      mutate(scenario = factor(scenario, levels = .scenario))

    g <- ggplot(dat_hist, aes(value, y = ..density..)) +
      geom_histogram(bins = bins, colour = "black", fill = "grey") +
      facet_wrap(vars(scenario), scales = scales) +
      theme_pbs() +
      labs(x = parse(text = en2fr("B/B[MSY]", french)),
           y = en2fr("Frequency", french))

  } else {
    g <- make_plot_wrap(dat, scenario, french, scales,
                        ylim = c(0, 1.1 * max(dat$value))) +
      labs(y = parse(text = en2fr("B/B[MSY]", french)))
  }
  g
}


#' @describeIn plot_rcm Plot historical spawning depletion
#' @export
plot_rcm_depletion <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE,
                               scales = "fixed", histogram = FALSE, bins = NULL, MPD = FALSE) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_SSB, type = "depletion", MPD = MPD)

  if (histogram) {
    .scenario <- scenario
    dat_hist <- dat %>% dplyr::filter(year == max(year)) %>%
      mutate(scenario = factor(scenario, levels = .scenario))

    g <- ggplot(dat_hist, aes(value, y = ..density..)) +
      geom_histogram(bins = bins, colour = "black", fill = "grey") +
      facet_wrap(vars(scenario), scales = scales) +
      theme_pbs() +
      labs(x = parse(text = "B/B[0]"),
           y = en2fr("Frequency", french))
  } else {
    g <- make_plot_wrap(dat, scenario, french = TRUE, scales,
                        ylim = c(0, 1.1 * max(dat$value))) +
      labs(y = parse(text = "B/B[0]"))
  }

  g
}

#' @describeIn plot_rcm Plot historical apical F
#' @export
plot_rcm_F <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE, scales = "fixed",
                       ylim = NULL, MPD = FALSE) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_F, MPD = MPD)

  if (is.null(ylim)) ylim <- c(0, 1.1 * max(dat$value))
  make_plot_wrap(dat, scenario, french, scales, ylim = ylim) +
    labs(y = en2fr("Fishing mortality", french))
}

#' @describeIn plot_rcm Plot historical F/FMSY
#' @export
plot_rcm_FMSY <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE, scales = "fixed",
                          ylim = NULL, MPD = FALSE) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_F, MPD = MPD, type = "FMSY")

  if (is.null(ylim)) ylim <- c(0, 1.1 * max(dat$value))
  make_plot_wrap(dat, scenario, french, scales, ylim = ylim) +
    labs(y = en2fr("F/F[MSY]", french))
}


#' @describeIn plot_rcm Plot recruitment deviates
#' @param proj Logical, whether to plot the recruitment deviates in the projection period
#' @param logspace Logical, whether to plot the recruitment deviates in logspace or normal space
#' @export
plot_rcm_recdev <- function(rcm, scenario, french = FALSE, proj = FALSE, logspace = FALSE,
                            scales = "fixed", ylim = NULL, MPD = FALSE) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_recdev, proj = proj, logspace = logspace, MPD = MPD)
  g <- make_plot_wrap(dat, scenario, french, scales = scales, ylim = ylim) +
    geom_hline(yintercept = ifelse(logspace, 0, 1), lty = 2, alpha = 0.6) +
    labs(y = paste(en2fr("Recruitment deviations", french), ifelse(logspace, "(log)", "")))
  g
}

#' @describeIn plot_rcm Plot recruitment
#' @importFrom ggplot2 expand_limits
#' @export
plot_rcm_rec <- function(rcm, scenario, french = FALSE, scales = "fixed", ylim = NULL, MPD = FALSE) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_rec, MPD = MPD)
  g <- make_plot_wrap(dat, scenario, french,
                      scales = scales, ylim = ylim) +
    expand_limits(y = 0) +
    labs(y = en2fr("Recruitment", french))
  g
}


#' @describeIn plot_rcm Compare maturity and length-at-age schedule vs. selectivity (of a single index or fleet)
#' @param bio_type Character vector to plot either length-at-age (\code{"LAA"}) or maturity-at-age (\code{"mat"})
#' @param sel_type Character to indicate whether to grab a fleet or index selectivity
#' @param sel_i The index of the fleet or index for the selectivity
#' @param sel_name The name for the selectivity
#' @export
plot_rcm_bio_sel <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE,
                             bio_type = c("LAA", "mat"), sel_type = c("fleet", "index"),
                             sel_i = 1, sel_name = "Selectivity") {
  bio_type <- match.arg(bio_type, several.ok = TRUE)
  sel_type <- match.arg(sel_type)

  if (missing(sel_name)) {
    if (requireNamespace("stringi", quietly = TRUE)) {
      sel_name <- stringi::stri_trans_totitle(sel_type) %>%
        paste(sel_i, en2fr("selectivity", french, case = "lower"))
    } else {
      sel_name <- en2fr("Selectivity", french)
    }
  }

  dat <- purrr::map2_dfr(rcm, scenario, .rcm_bio_sel,
    bio_type = bio_type, sel_type = sel_type,
    sel_i = sel_i, sel_name = sel_name
  )

  g <- ggplot(dat, aes(Age, Value, linetype = Type)) +
    geom_line() +
    geom_point(aes(shape = Type)) +
    facet_wrap(~scenario) +
    theme_pbs() +
    labs(x = en2fr("Age", french), y = en2fr("Selectivity", french)) +
    coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.01))
  g
}


#' @describeIn plot_rcm Plot selectivity of a single index or fleet (all simulations)
#' @param color Character vector of colors used for plotting.
#' @param type Whether the output is from a fishing fleet or index.
#' @param i The corresponding index from the fleet or index of abundance.
#' @export
plot_rcm_sel <- function(rcm, scenario, type = c("fleet", "index"), i = "all", color = "black", french = FALSE, MPD = FALSE) {
  type <- match.arg(type)
  sel <- purrr::map2_dfr(rcm, scenario, .rcm_sel, type = type, i = i, MPD = MPD)

  g <- ggplot(sel, aes(Age, value, group = paste(iter))) +
    geom_line(alpha = 0.05, color = color) +
    theme_pbs() +
    facet_wrap(~scenario) +
    xlab(en2fr("Age", french)) +
    ylab(en2fr("Selectivity", french)) +
    coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.1))

  g
}

#' @describeIn plot_rcm Plot selectivity of a multiple indices or fleets overlayed on top of each other
#' @param fleet_i Vectors of indices for the fleets, i.e., 1 is the first fleet, etc. Use NA if no fleets will be plotted.
#' @param index_i Vectors of indices for the indices of abundance. Use NA if no indices will be plotted.
#' @param fleet_names Character vector for the names of the fleets
#' @param index_names Character vector for the names of the indices
#' @export
plot_rcm_sel_multi <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), fleet_i = 1, index_i = 1,
                               fleet_names = paste("Fleet", fleet_i), index_names = paste("Index", index_i),
                               french = FALSE, color) {
  if (all(is.na(fleet_i))) {
    fsel <- data.frame()
  } else {
    fsel <- map2_dfr(rcm, scenario, .rcm_sel_multi, i = fleet_i, i_names = fleet_names)
  }

  if (all(is.na(index_i))) {
    isel <- data.frame()
  } else {
    isel <- map2_dfr(rcm, scenario, .rcm_sel_multi, i = index_i, i_names = index_names, type = "index")
  }
  out <- rbind(fsel, isel)

  if (missing(color)) {
    color <- RColorBrewer::brewer.pal(sum(!is.na(c(fleet_i, index_i))), "Set2") %>%
      structure(names = c(fleet_names, index_names))
  }

  ggplot(out, aes(Age, value, colour = Type)) +
    geom_line() +
    theme_pbs() +
    facet_wrap(~scenario) +
    ylab(en2fr("Selectivity", french)) +
    xlab(en2fr("Age", french)) +
    scale_color_manual(values = color) +
    coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.1))
}


#' @describeIn plot_rcm Plot histogram or scatter plot of operating model parameters
#' @param var Parameters to plot. Select up to two.
#' @param status Logical, whether to identify estimated status with each point in scatter plot.
#' Only used if two parameters are selected in \code{var}.
#' @param bins The number of bins for \link[ggplot]{geom_histogram}.
#' @param do_corr Whether to report the correlation between parameters.
#' Only used if two parameters are selected in \code{var}.
#' @export
plot_rcm_corr <- function(rcm, scenario, var = c("D", "M", "h", "R0"),
                          status = TRUE, bins = NULL, do_corr = FALSE,
                          french = FALSE) {
  var2 <- var <- match.arg(var, several.ok = TRUE)
  stopifnot(length(var) <= 2)

  if(length(var) == 1) var2 <- rep(var2, 2)

  var_names <- c("D" = en2fr("Spawning depletion", french),
                 "M" = en2fr("Natural mortality", french),
                 "h" = en2fr("Steepness", french),
                 "R0" = en2fr("Unfished recruitment", french))

  var2[var == "M"] <- "M_ageArray[,1,1]"

  vars <- purrr::map2_dfr(rcm, scenario, .rcm_par_status, var2, status = status)

  if(length(var) == 1) {

    g <- ggplot(vars, aes(Var1)) +
      geom_histogram(fill = "grey", colour = "black", bins = bins) +
      theme_pbs() +
      facet_wrap(vars(scenario), scales = "free") +
      labs(x = var_names[names(var_names) == var],
           y = en2fr("Frequency", french))

  } else {

    g <- ggplot(vars, aes(Var1, Var2)) +
      theme_pbs() +
      facet_wrap(vars(scenario)) +
      labs(x = var_names[names(var_names) == var[1]], y = var_names[names(var_names) == var[2]])

    if (status) {
      if (french) {
        values <- c("Saine" = "green", "Prudence" = "yellow", "Critique" = "red")
      } else {
        values <- c("Healthy" = "green", "Cautious" = "yellow", "Critical" = "red")
      }
      g <- g +
        geom_point(shape = 21, aes(fill = status)) +
        scale_fill_manual(ifelse(french, "Statut\nestimé", "Estimated\nstatus"), values = values)
    } else {
      g <- g + geom_point(shape = 21, bg = "grey")
    }

    if (do_corr) {
      corr <- vars %>%
        group_by(scenario) %>%
        summarise(correlation = cor(Var1, Var2) %>% round(2))
      g <- g +
        geom_label(data = corr,
                   x = Inf, y = Inf,
                   hjust = "inward", vjust = "inward",
                   inherit.aes = FALSE,
                   aes(label = correlation))
    }
  }

  g
}
