
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
#' @export
plot_rcm_SSB <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE,
                         scales = "fixed", ylim = NULL) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_SSB, type = "SSB")

  if (is.null(ylim)) ylim <- c(0, 1.1 * max(dat$value))
  make_plot_wrap(dat, scenario, french, scales, ylim = ylim, ylab = "Spawning biomass")
}


#' @describeIn plot_rcm Plot historical spawning depletion
#' @export
plot_rcm_depletion <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE,
                               scales = "fixed") {
  dat <- purrr::map2_df(rcm, scenario, .rcm_SSB, type = "depletion")
  make_plot_wrap(dat, scenario, french, scales, ylab = "Spawning depletion", ylim = c(0, 1.1 * max(dat$value)))
}

#' @describeIn plot_rcm Plot historical apical F
#' @export
plot_rcm_F <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE, scales = "fixed",
                       ylim = NULL) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_F)

  if (is.null(ylim)) ylim <- c(0, 1.1 * max(dat$value))
  make_plot_wrap(dat, scenario, french, scales, ylim = ylim, ylab = "Apical fishing mortality")
}


#' @describeIn plot_rcm Plot recruitment deviates
#' @param proj Logical, whether to plot the recruitment deviates in the projection period
#' @param logspace Logical, whether to plot the recruitment deviates in logspace or normal space
#' @export
plot_rcm_recdev <- function(rcm, scenario, french = FALSE, proj = FALSE, logspace = FALSE,
                            scales = "fixed", ylim = NULL) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_recdev, proj = proj, logspace = logspace)
  g <- make_plot_wrap(dat, scenario, french,
    scales = scales, ylim = ylim,
    ylab = paste0("Recruitment deviations", ifelse(logspace, " log space", ""))
  ) +
    geom_hline(yintercept = ifelse(logspace, 0, 1), lty = 2, alpha = 0.6)
  g
}

#' @describeIn plot_rcm Plot recruitment
#' @export
plot_rcm_rec <- function(rcm, scenario, french = FALSE, scales = "fixed", ylim = NULL) {
  dat <- purrr::map2_df(rcm, scenario, .rcm_rec)
  g <- make_plot_wrap(dat, scenario, french,
                      scales = scales, ylim = ylim,
                      ylab = "Recruitment") +
    expand_limits(y = 0)
  g
}

.rcm_rec <- function(x, scenario) {
  sapply(x@Misc, getElement, "R") %>%
    structure(dimnames = list(year = seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr + 1),
                              iteration = 1:length(x@Misc))) %>%
    reshape2::melt() %>%
    mutate(scenario = scenario)
}

#' @describeIn plot_rcm Compare maturity and length-at-age schedule vs. selectivity (of a single index or fleet)
#' @param bio_type Character vector to plot either length-at-age (\code{"LAA"}) or maturity-at-age (\code{"mat"})
#' @param sel_type Character to indicate whether to grab a fleet or index selectivity
#' @param sel_i The index of the fleet or index for the selectivity
#' @param sel_name The name for the selectivity
#' @export
plot_rcm_bio_sel <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE,
                             bio_type = c("LAA", "mat"), sel_type = c("fleet", "index"),
                             sel_i = 1, sel_name) {
  bio_type <- match.arg(bio_type, several.ok = TRUE)
  sel_type <- match.arg(sel_type)

  if (missing(sel_name)) {
    if (requireNamespace("stringi", quietly = TRUE)) {
      sel_name <- stringi::stri_trans_totitle(sel_type) %>% paste(sel_i, "selectivity")
    } else {
      sel_name <- "Selectivity"
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
    labs(x = en2fr("Age", french), y = en2fr("Value", french)) +
    coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.01))
  g
}
