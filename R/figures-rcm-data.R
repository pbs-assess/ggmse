
#' @name plot_rcm_data
#' @title Plotting functions of data used for OM conditioning
#'
#' @description A set of functions that plots fits to data from a list of RCModel objects.
NULL

#' @describeIn plot_rcm_data Plot fits to index
#' @param rcm A list containing \linkS4class{RCModel} objects.
#' @param scenario A character vector of names corresponding to \code{rcm}.
#' @param french Logical, whether the axes are in French or not.
#' @param type Whether the output is from a fishing fleet or index.
#' @param i The corresponding index from the fleet or index of abundance.
#' @param index_names Character vector of names for the indices of abundance.
#' @param color Character vector of colors used for plotting.
#' @param xlim Optional, x-axis limits for the figure.
#' @export
#' @importFrom stats weighted.mean
#' @importFrom ggplot2 ggtitle geom_label geom_col geom_pointrange
#' @importFrom purrr map2_dfr map_dfc
plot_rcm_index <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE, i, index_names,
                           color, xlim) {
  if (missing(i)) i <- 1:dim(rcm@Misc[[1]]$Ipred)[2]
  if (missing(index_names)) index_names <- paste("Index", i)
  if (missing(color)) {
    color <- RColorBrewer::brewer.pal(length(i), "Set2") %>%
      structure(names = index_names)
  }
  all_years <- seq(rcm[[1]]@OM@CurrentYr - rcm[[1]]@OM@nyears + 1, rcm[[1]]@OM@CurrentYr)

  .scenario <- scenario
  .index_names <- index_names
  Ipred <- purrr::map2_dfr(rcm, scenario, .rcm_index, .index_names = index_names, i = i) %>%
    mutate(
      scenario = factor(scenario, levels = .scenario),
      year = year + max(all_years) - rcm[[1]]@OM@nyears,
      index_names = factor(index_names, levels = .index_names)
    )

  I_sd <- rcm[[1]]@data@I_sd[, i, drop = FALSE] %>%
    structure(dimnames = list(all_years, index_names)) %>%
    as.data.frame() %>%
    cbind(data.frame(year = all_years)) %>%
    reshape2::melt(id.vars = "year", variable.name = "index_names", value.name = "SD")

  Index <- rcm[[1]]@data@Index[, i, drop = FALSE] %>%
    structure(dimnames = list(all_years, index_names)) %>%
    as.data.frame() %>%
    cbind(data.frame(year = all_years)) %>%
    reshape2::melt(id.vars = "year", variable.name = "index_names") %>%
    left_join(I_sd, by = c("year", "index_names")) %>%
    mutate(lower = exp(log(value) - 2 * SD), upper = exp(log(value) + 2 * SD))

  g <- ggplot(Ipred, aes(year, value, group = paste(iter, index_names), colour = index_names)) +
    geom_line(alpha = 0.05) +
    geom_pointrange(
      data = Index, aes(x = year, y = value, ymin = lower, ymax = upper, fill = index_names),
      inherit.aes = FALSE, pch = 21, colour = "grey40"
    ) +
    theme_pbs() +
    scale_color_manual(values = color) +
    scale_fill_manual(values = color) +
    ylab(en2fr("Index value", french)) +
    xlab(en2fr("Year", french)) +
    labs(colour = en2fr("Index", french), fill = en2fr("Index", french))
  if (length(i) > 1) {
    g <- g + facet_grid(index_names ~ scenario, scales = "free_y")
  } else {
    g <- g + facet_wrap(scenario ~ .)
  }
  if (!missing(xlim)) g <- g + coord_cartesian(xlim = xlim)
  g
}

#' @describeIn plot_rcm_data Plot age comps from a fleet or index (single RCM only)
#' @param RCModel An object of class \linkS4class{RCModel}
#' @export
plot_rcm_age_comps <- function(RCModel, scenario, french = FALSE, type = c("fleet", "index"), i = 1, color = "black") {
  type <- match.arg(type)
  all_years <- seq(RCModel@OM@CurrentYr - RCModel@OM@nyears + 1, RCModel@OM@CurrentYr)

  if (type == "fleet") {
    N <- RCModel@data@CAA_ESS[, i]
    obs <- RCModel@data@CAA[, , i] / rowSums(RCModel@data@CAA[, , i])
  } else {
    N <- RCModel@data@IAA_ESS[, i]
    obs <- RCModel@data@IAA[, , i] / rowSums(RCModel@data@IAA[, , i])
  }
  yr_ind <- N > 0
  yr_plot <- all_years[yr_ind]
  N_df <- data.frame(Year = yr_plot, N = paste("N =", N[yr_ind] %>% round(1)))

  obs <- structure(obs[yr_ind, ], dimnames = list(Year = yr_plot, Age = 0:RCModel@OM@maxage)) %>%
    reshape2::melt(value.name = "Frequency")

  pred <- lapply(1:length(RCModel@Misc), function(x) {
    if (type == "fleet") {
      p <- RCModel@Misc[[x]]$CAApred[yr_ind, , i] / rowSums(RCModel@Misc[[x]]$CAApred[yr_ind, , i])
    } else {
      p <- RCModel@Misc[[x]]$IAApred[yr_ind, , i] / rowSums(RCModel@Misc[[x]]$IAApred[yr_ind, , i])
    }
    p %>%
      structure(dimnames = list(Year = yr_plot, Age = 0:RCModel@OM@maxage)) %>%
      reshape2::melt(value.name = "Frequency") %>%
      mutate(Iter = x)
  }) %>% bind_rows()

  g <- ggplot(pred, aes(Age, Frequency, group = Iter)) +
    facet_wrap(~Year, scales = "free_y") +
    geom_col(data = obs, mapping = aes(x = Age, y = Frequency), fill = "grey40", inherit.aes = FALSE) +
    geom_line(alpha = 0.05, colour = color) +
    # geom_point(data = obs, mapping = aes(x = Age, y = Frequency), inherit.aes = FALSE) +
    geom_label(
      data = N_df, mapping = aes(label = N), x = Inf, y = Inf,
      hjust = "right", vjust = "top", inherit.aes = FALSE
    ) +
    theme_pbs() +
    ggtitle(scenario) +
    xlab(en2fr("Age", french)) +
    ylab(en2fr("Frequency", french))
  g
}

#' @describeIn plot_rcm_data Plot selectivity of a single index or fleet (all simulations)
#' @param name The name of the index or fleet
#' @export
plot_rcm_sel <- function(rcm, scenario, type = c("survey", "index"), i, name) {
  type <- match.arg(type)
  sel <- purrr::map2_dfr(rcm, scenario, .rcm_sel, type = type, i = i)

  g <- ggplot(sel, aes(Age, value, group = paste(iter))) +
    geom_line(alpha = 0.15) +
    theme_pbs() +
    facet_wrap(~scenario) +
    ylab(paste(name, "selectivity")) +
    coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.1))
  g
}

#' @describeIn plot_rcm_data Plot selectivity of a multiple indices or fleets overlayed on top of each other
#' @param fleet_i Vectors of indices for the fleets, i.e., 1 is the first fleet, etc. Use NA if no fleets will be plotted.
#' @param index_i Vectors of indices for the indices of abundance. Use NA if no indices will be plotted.
#' @param fleet_names Character vector for the names of the fleets
#' @param index_names Character vector for the names of the indices
#' @export
plot_rcm_sel_multi <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), fleet_i = 1, index_i = 1,
                               fleet_names = paste("Fleet", fleet_i), index_names = paste("Index", index_i),
                               color) {
  if (all(is.na(fleet_i))) {
    fsel <- map2_dfr(rcm, scenario, .rcm_sel_multi, i = fleet_i, i_names = fleet_names)
  } else {
    fsel <- data.frame()
  }

  if (all(is.na(index_i))) {
    isel <- map2_dfr(rcm, scenario, .rcm_sel_multi, i = index_i, i_names = index_names, type = "index")
  } else {
    isel <- data.frame()
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
    ylab("Selectivity") +
    xlab("Age") +
    scale_color_manual(values = color) +
    coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.1))
}


#' @describeIn plot_rcm_data Plot mean age for a fleet or index of abundance
#' @param scales The scales argument to \link[ggplot2]{facet_wrap}.
#' @param ylim Optional y-axes limits to figures.
#' @export
plot_rcm_mean_age <- function(rcm, scenario, type = c("fleet", "index"), i, name, scales = "fixed", ylim) {
  type <- match.arg(type)
  if (missing(i)) i <- 1
  if (missing(name)) {
    if (requireNamespace("stringi", quietly = TRUE)) {
      name <- stringi::stri_trans_totitle(type) %>% paste(i, "mean age")
    } else {
      name <- "Mean age"
    }
  }

  obs <- .rcm_mean_age(rcm = rcm[[1]], type = type, i = i, type2 = "obs")
  pred <- purrr::map2_dfr(rcm, scenario, .rcm_mean_age, type = type, i = i, type2 = "pred")

  g <- ggplot(pred, aes(year, value)) +
    geom_line(alpha = 0.6, aes(group = paste(iter))) +
    theme_pbs() +
    facet_wrap(~scenario, scales = scales) +
    labs(x = "Year", y = name) +
    geom_point(data = obs) +
    geom_line(data = obs, linetype = 3)
  if (!missing(ylim)) g <- g + coord_cartesian(expand = FALSE, ylim = ylim)
  g
}


#' @describeIn plot_rcm_data Plot length comps from a fleet or index (single RCM only)
#' @param RCModel An object of class \linkS4class{RCModel}
#' @export
plot_rcm_length_comps <- function(RCModel, scenario, french = FALSE, type = c("fleet", "index"), i = 1, color = "black") {
  type <- match.arg(type)
  all_years <- seq(RCModel@OM@CurrentYr - RCModel@OM@nyears + 1, RCModel@OM@CurrentYr)

  if (type == "fleet") {
    N <- RCModel@data@CAL_ESS[, i]
    obs <- RCModel@data@CAL[, , i] / rowSums(RCModel@data@CAL[, , i])
  } else {
    N <- RCModel@data@IAA_ESS[, i]
    obs <- RCModel@data@IAL[, , i] / rowSums(RCModel@data@IAL[, , i])
  }
  yr_ind <- N > 0
  yr_plot <- all_years[yr_ind]
  N_df <- data.frame(Year = yr_plot, N = paste("N =", N[yr_ind] %>% round(1)))

  obs <- structure(obs[yr_ind, ], dimnames = list(Year = yr_plot, Length = RCModel@data@length_bin)) %>%
    reshape2::melt(value.name = "Frequency")

  pred <- lapply(1:length(RCModel@Misc), function(x) {
    if (type == "fleet") {
      p <- RCModel@Misc[[x]]$CALpred[yr_ind, , i] / rowSums(RCModel@Misc[[x]]$CALpred[yr_ind, , i])
    } else {
      p <- RCModel@Misc[[x]]$IALpred[yr_ind, , i] / rowSums(RCModel@Misc[[x]]$IALpred[yr_ind, , i])
    }
    p %>%
      structure(dimnames = list(Year = yr_plot, Length = RCModel@data@length_bin)) %>%
      reshape2::melt(value.name = "Frequency") %>%
      mutate(Iter = x)
  }) %>% bind_rows()

  g <- ggplot(pred, aes(Length, Frequency, group = Iter)) +
    facet_wrap(~Year, scales = "free_y") +
    geom_col(data = obs, mapping = aes(x = Length, y = Frequency), fill = "grey40", inherit.aes = FALSE) +
    geom_line(alpha = 0.05, colour = color) +
    geom_label(
      data = N_df, mapping = aes(label = N), x = Inf, y = Inf,
      hjust = "right", vjust = "top", inherit.aes = FALSE
    ) +
    theme_pbs() +
    ggtitle(scenario) +
    xlab(en2fr("Length", french)) +
    ylab(en2fr("Frequency", french))
  g
}

#' @describeIn plot_rcm_data Plot mean length for a fleet or index of abundance using the length compositions
#' @param scales The scales argument to \link[ggplot2]{facet_wrap}.
#' @param ylim Optional y-axes limits to figures.
#' @export
plot_rcm_mean_length <- function(rcm, scenario, type = c("fleet", "index"), i, name, scales = "fixed", ylim) {
  type <- match.arg(type)
  if (missing(i)) i <- 1
  if (missing(name)) {
    if (requireNamespace("stringi", quietly = TRUE)) {
      name <- stringi::stri_trans_totitle(type) %>% paste(i, "mean length")
    } else {
      name <- "Mean length"
    }
  }

  obs <- .rcm_mean_length(rcm = rcm[[1]], type = type, i = i, type2 = "obs")
  pred <- purrr::map2_dfr(rcm, scenario, .rcm_mean_length, type = type, i = i, type2 = "pred")

  g <- ggplot(pred, aes(year, value)) +
    geom_line(alpha = 0.6, aes(group = paste(iter))) +
    theme_pbs() +
    facet_wrap(~scenario, scales = scales) +
    labs(x = "Year", y = name) +
    geom_point(data = obs) +
    geom_line(data = obs, linetype = 3)
  if (!missing(ylim)) g <- g + coord_cartesian(expand = FALSE, ylim = ylim)
  g
}

#' @describeIn plot_rcm_data Plots the proportion of biomass vs. age from the predicted age composition of a
#' fleet or index. Used as a diagnostic of whether a high residual in the plusgroup is problematic. Only plots
#' years for which there are observed age data.
#' @export
plot_rcm_biomass_age <- function(RCModel, scenario, type = c("fleet", "index"), i = 1, color = "black", french = FALSE) {
  type <- match.arg(type)
  all_years <- seq(RCModel@OM@CurrentYr - RCModel@OM@nyears + 1, RCModel@OM@CurrentYr)

  if (type == "fleet") {
    N <- RCModel@data@CAA_ESS[, i]
  } else {
    N <- RCModel@data@IAA_ESS[, i]
  }
  yr_ind <- N > 0
  yr_plot <- all_years[yr_ind]

  Wt_age <- RCModel@OM@cpars$Wt_age[, , 1:RCModel@OM@nyears]

  prop_bio <- lapply(1:length(RCModel@Misc), function(x) {
    if (type == "fleet") {
      age_comp <- RCModel@Misc[[x]]$CAApred[1:RCModel@OM@nyears, , i]
    } else {
      age_comp <- RCModel@Misc[[x]]$IAApred[1:RCModel@OM@nyears, , i]
    }
    reshape2::melt(age_comp * t(Wt_age[x, , ])) %>%
      rename(Year = Var1, Age = Var2) %>%
      mutate(Year = Year + RCModel@OM@CurrentYr - RCModel@OM@nyears, Age = Age - 1) %>%
      group_by(Year) %>%
      mutate(prop = value / sum(value), Iter = x)
  }) %>%
    bind_rows() %>%
    dplyr::filter(Year %in% yr_plot)

  g <- ggplot(prop_bio, aes(Age, prop, group = Iter)) +
    facet_wrap(~Year, scales = "free_y") +
    geom_line(alpha = 0.05, colour = color) +
    geom_point() +
    theme_pbs() +
    ggtitle(scenario) +
    xlab(en2fr("Age", french)) +
    ylab(en2fr("Proportion biomass", french))
  g
}
