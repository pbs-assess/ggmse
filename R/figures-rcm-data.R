
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
#' @param alpha The alpha parameter to control color transparency in the ggplots.
#' @param color Character vector of colors used for plotting.
#' @param MPD Logical, whether to plot individual simulations (\code{FALSE}) or from the single fit (\code{TRUE}) in
#' \code{RCModel@mean_fit}.
#' @export
#' @importFrom stats weighted.mean
#' @importFrom ggplot2 ggtitle geom_label geom_col geom_pointrange
#' @importFrom purrr map2_dfr map_dfc
plot_rcm_index <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), french = FALSE, i, index_names,
                           color, MPD = FALSE) {
  if (missing(i)) i <- 1:dim(rcm@Misc[[1]]$Ipred)[2]
  if (missing(index_names)) index_names <- paste("Index", i)
  if (missing(color)) {
    color <- RColorBrewer::brewer.pal(length(i), "Set2") %>%
      structure(names = index_names)
  }
  all_years <- seq(rcm[[1]]@OM@CurrentYr - rcm[[1]]@OM@nyears + 1, rcm[[1]]@OM@CurrentYr)

  .scenario <- scenario
  .index_names <- index_names
  Ipred <- purrr::map2_dfr(rcm, scenario, .rcm_index, .index_names = index_names, i = i, MPD = MPD) %>%
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
    ylab(en2fr("Index", french)) +
    xlab(en2fr("Year", french)) +
    labs(colour = en2fr("Index", french), fill = en2fr("Index", french))
  if (length(i) > 1) {
    g <- g + facet_grid(index_names ~ scenario, scales = "free_y")
  } else {
    g <- g + facet_wrap(scenario ~ .)
  }

  g
}

#' @describeIn plot_rcm_data Plot age comps from a fleet or index (single RCM only)
#' @param RCModel An object of class \linkS4class{RCModel}
#' @export
plot_rcm_age_comps <- function(RCModel, scenario, french = FALSE, type = c("fleet", "index"), i = 1,
                               color = "black", MPD = FALSE) {
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

  if (MPD) {
    y <- list(RCModel@mean_fit$report)
  } else {
    y <- RCModel@Misc
  }

  pred <- lapply(1:length(y), function(x) {
    if (type == "fleet") {
      p <- y[[x]]$CAApred[yr_ind, , i] / rowSums(y[[x]]$CAApred[yr_ind, , i])
    } else {
      p <- y[[x]]$IAApred[yr_ind, , i] / rowSums(y[[x]]$IAApred[yr_ind, , i])
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

#' @describeIn plot_rcm_data Plot mean age for a fleet or index of abundance
#' @param scales The scales argument to \link[ggplot2]{facet_wrap}.
#' @export
plot_rcm_mean_age <- function(rcm, scenario, type = c("fleet", "index"), i = 1,
                              scales = "fixed", color = "black", alpha = 0.6, french = FALSE,
                              MPD = FALSE) {
  type <- match.arg(type)

  obs <- .rcm_mean_age(rcm = rcm[[1]], type = type, i = i, type2 = "obs")
  pred <- purrr::map2_dfr(rcm, scenario, .rcm_mean_age, type = type, i = i, type2 = "pred", MPD = MPD)

  g <- ggplot(pred, aes(year, value)) +
    geom_line(alpha = alpha, colour = color, aes(group = paste(iter))) +
    theme_pbs() +
    facet_wrap(~scenario, scales = scales) +
    labs(x = en2fr("Year", french),
         y = en2fr("Mean age", french, allow_missing = TRUE)) +
    geom_point(data = obs, aes(year, value),
               inherit.aes = FALSE, fill = color, pch = 21, colour = "grey40") +
    geom_line(data = obs, linetype = 3) +
    scale_color_manual(values = color) +
    scale_fill_manual(values = color)

  g
}


#' @describeIn plot_rcm_data Plot length comps from a fleet or index (single RCM only)
#' @param RCModel An object of class \linkS4class{RCModel}
#' @export
plot_rcm_length_comps <- function(RCModel, scenario, french = FALSE, type = c("fleet", "index"),
                                  i = 1, color = "black", MPD = FALSE) {
  type <- match.arg(type)
  all_years <- seq(RCModel@OM@CurrentYr - RCModel@OM@nyears + 1, RCModel@OM@CurrentYr)

  if (type == "fleet") {
    N <- RCModel@data@CAL_ESS[, i]
    obs <- RCModel@data@CAL[, , i] / rowSums(RCModel@data@CAL[, , i])
  } else {
    N <- RCModel@data@IAL_ESS[, i]
    obs <- RCModel@data@IAL[, , i] / rowSums(RCModel@data@IAL[, , i])
  }
  yr_ind <- N > 0
  yr_plot <- all_years[yr_ind]
  N_df <- data.frame(Year = yr_plot, N = paste("N =", N[yr_ind] %>% round(1)))

  obs <- structure(obs[yr_ind, , drop = FALSE], dimnames = list(Year = yr_plot, Length = RCModel@data@length_bin)) %>%
    reshape2::melt(value.name = "Frequency")

  if (MPD) {
    y <- list(RCModel@mean_fit$report)
  } else {
    y <- RCModel@Misc
  }

  pred <- lapply(1:length(y), function(x) {
    if (type == "fleet") {
      p <- y[[x]]$CALpred[, , i][yr_ind, , drop = FALSE] / rowSums(y[[x]]$CALpred[, , i][yr_ind, , drop = FALSE])
    } else {
      p <- y[[x]]$IALpred[, , i][yr_ind, , drop = FALSE] / rowSums(y[[x]]$IALpred[, , i][yr_ind, , drop = FALSE])
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
#' @export
plot_rcm_mean_length <- function(rcm, scenario, type = c("fleet", "index"), i = 1,
                                 scales = "fixed", color = "black", alpha = 0.6,
                                 french = FALSE, MPD = FALSE) {
  type <- match.arg(type)

  obs <- .rcm_mean_length(rcm = rcm[[1]], type = type, i = i, type2 = "obs")
  pred <- purrr::map2_dfr(rcm, scenario, .rcm_mean_length, type = type, i = i, type2 = "pred", MPD = MPD)

  g <- ggplot(pred, aes(year, value)) +
    geom_line(alpha = alpha, colour = color, aes(group = paste(iter))) +
    theme_pbs() +
    facet_wrap(~scenario, scales = scales) +
    labs(x = en2fr("Year", french),
         y = en2fr("Mean length", french)) +
    geom_point(data = obs, aes(year, value),
               inherit.aes = FALSE, fill = color, pch = 21, colour = "grey40") +
    geom_line(data = obs, linetype = 3) +
    scale_color_manual(values = color) +
    scale_fill_manual(values = color)

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
    ylab(en2fr("Proportion biomass", french, allow_missing = TRUE))
  g
}
