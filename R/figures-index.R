#' Plot sampled survey index or catch projections
#'
#' @param object_list A list of MSEtool MSE objects representing different
#'   scenarios. The list should be named with the scenario names.
#' @param n_samples The number of timeseries samples to illustrate.
#' @param seed The seed to set before drawing samples.
#' @param type Which index to plot.
#' @param omit_index_fn A function that indexes years in the projection period
#'   to omit from the plot. E.g., for a biennial survey omitting even years
#'   starting in year 2: `function(x) seq(2, x, 2))`. `x` represents the total
#'   number of years.
#' @param quantiles Probability quantiles to show.
#' @param french French?
#' @param MP A character vector to subset the management procedures in the MSE object.
#'
#' @export
#' @importFrom ggplot2 facet_grid scale_colour_brewer
#' @return A ggplot object
#' @describeIn plot_index Plots observed and simulated indices, i.e., `MSE@PPD[[i]]@Ind` or `MSE@PPD[[i]]@AddInd`, gridded by
#' OM and MP
#' @examples
#' plot_index(mse_example)
#' mse_list <- list()
#' mse_list[[1]] <- mse_example
#' mse_list[[2]] <- mse_example
#' names(mse_list) <- c("Sc 1", "Sc 2")
#' plot_index(mse_list)
plot_index <- function(object_list, n_samples = 4, seed = 42,
                       type = c("Ind", "AddInd"), MP = NULL,
                       omit_index_fn = function(x) NULL,
                       quantiles = c(0.025, 0.975),
                       french = isTRUE(getOption("french"))) {

  if (!is.list(object_list)) {
    object_list <- list(object_list)
    names(object_list) <- "Scenario"
  }

  type <- match.arg(type)
  if (is.null(object_list[[1]]@OM$CurrentYr[[1]])) {
    warning(
      "Missing `object@OM$CurrentYr`.\n",
      "Please run the MSE with a newer GitHub MSEtool version\n",
      "or set `object@OM$CurrentYr` yourself.\n",
      "Setting CurrentYr = 0 for now.",
      call. = FALSE
    )
    this_year <- 0
  } else {
    this_year <- object_list[[1]]@OM$CurrentYr[[1]]
  }

  d <- purrr::map_dfr(object_list, get_index_ts,
    this_year = this_year, type = type, omit_index_fn = omit_index_fn,
    seed = seed, n_samples = n_samples, .id = "scenario"
  )
  d_all <- purrr::map_dfr(object_list, get_index_ts,
    this_year = this_year, type = type, omit_index_fn = omit_index_fn,
    seed = seed, n_samples = object_list[[1]]@nsim, .id = "scenario"
  )
  d_all <- group_by(d_all, scenario, real_year, mp_name) %>%
    summarise(
      lwr = quantile(value, probs = quantiles[[1]], na.rm = TRUE),
      upr = quantile(value, probs = quantiles[[2]], na.rm = TRUE)
    )

  if(!is.null(MP)) {
    d <- dplyr::filter(d, mp_name %in% MP) %>% mutate(mp_name = factor(mp_name, levels = MP))
    d_all <- dplyr::filter(d_all, mp_name %in% MP) %>% mutate(mp_name = factor(mp_name, levels = MP))
  }

  g <- ggplot(
    d[!is.na(d$value), , drop = FALSE],
    aes_string("real_year", "value", group = "as.factor(iter)")
  ) +
    geom_ribbon(
      data = d_all[!is.na(d_all$lwr), , drop = FALSE],
      aes_string(x = "real_year", ymin = "lwr", ymax = "upr"),
      alpha = 0.2, inherit.aes = FALSE
    ) +
    # geom_point(alpha = 0.9) +
    geom_path(alpha = 0.8) +
    facet_grid(mp_name ~ scenario) +
    geom_vline(xintercept = this_year, lty = 2, alpha = 0.5) +
    theme_pbs() +
    theme(panel.spacing = unit(0, "in")) +
    ylab(en2fr("Index value", french, allow_missing = TRUE)) +
    xlab(en2fr("Year", french, allow_missing = TRUE)) +
    guides(colour = "none")

  g
}

#' @describeIn plot_index Plots observed and simulated indices, i.e., `MSE@PPD[[i]]@Ind` or `MSE@PPD[[i]]@AddInd` by
#' MP (multiple OMs per panel)
#' @param palette A palette color as recognized by [ggplot2::scale_color_brewer()]
#' @export
plot_scenario_index <- function(object_list,
                                seed = 42,
                                type = c("Ind", "AddInd"),
                                MP = NULL,
                                omit_index_fn = function(x) NULL,
                                quantiles = c(0.025, 0.975),
                                palette = "Dark2",
                                french = isTRUE(getOption("french"))) {

  if (!is.list(object_list)) {
    object_list <- list(object_list)
    names(object_list) <- "Scenario"
  }

  type <- match.arg(type)
  if (is.null(object_list[[1]]@OM$CurrentYr[[1]])) {
    warning(
      "Missing `object@OM$CurrentYr`.\n",
      "Please run the MSE with a newer GitHub MSEtool version\n",
      "or set `object@OM$CurrentYr` yourself.\n",
      "Setting CurrentYr = 0 for now.",
      call. = FALSE
    )
    this_year <- 0
  } else {
    this_year <- object_list[[1]]@OM$CurrentYr[[1]]
  }

  d_all <- purrr::map_dfr(object_list, get_index_ts,
                          this_year = this_year, type = type, omit_index_fn = omit_index_fn,
                          seed = seed, n_samples = object_list[[1]]@nsim, .id = "scenario"
  )
  d_all <- group_by(d_all, scenario, real_year, mp_name) %>%
    summarise(
      lwr = quantile(value, probs = quantiles[[1]], na.rm = TRUE),
      med = quantile(value, probs = 0.5, na.rm = TRUE),
      upr = quantile(value, probs = quantiles[[2]], na.rm = TRUE)
    )

  if(!is.null(MP)) {
    d_all <- dplyr::filter(d_all, mp_name %in% MP) %>% mutate(mp_name = factor(mp_name, levels = MP))
  }

  d_hist <- dplyr::filter(d_all, .data$real_year <= this_year)

  g <- d_all %>%
    filter(!is.na(.data$med)) %>%
    ggplot(aes(.data$real_year)) +
    geom_ribbon(
      aes(ymin = .data$lwr, ymax = .data$upr, fill = .data$scenario),
      alpha = 0.2,
      colour = NA
    ) +
    geom_line(aes(y = .data$med, colour = .data$scenario)) +
    geom_point(data = d_hist, aes(y = .data$med)) +
    geom_line(data = d_hist, aes(y = .data$med)) +
    facet_wrap(vars(mp_name)) +
    geom_vline(xintercept = this_year, lty = 2, alpha = 0.5) +
    theme_pbs() +
    theme(panel.spacing = unit(0, "in")) +
    ylab(en2fr("Index", french, allow_missing = TRUE)) +
    xlab(en2fr("Year", french, allow_missing = TRUE)) +
    labs(colour = en2fr("OM", french), fill = en2fr("OM", french)) +
    scale_colour_brewer(palette = palette) +
    scale_fill_brewer(palette = palette)

  g
}


get_index_ts <- function(object, this_year, seed = 42, n_samples = 5,
                         type = c("Ind", "AddInd"), omit_index_fn = function(x) NULL) {
  type <- match.arg(type)
  x <- purrr::map(object@PPD, type)
  if (type == "AddInd") {
    x <- purrr::map(x, ~ .x[, 1L, , drop = TRUE])
  }
  ind <- omit_index_fn(ncol(x[[1]]))
  if (!is.null(ind)) {
    ind_proj_nas <- ind[ind > object@nyears]
    x <- purrr::map(x, ~ {
      .x[, ind_proj_nas] <- NA
      .x
    })
  }

  x <- reshape2::melt(x) %>%
    dplyr::rename(iter = .data$Var1, year = .data$Var2, mp = .data$L1) %>%
    dplyr::mutate(type = "projection")
  years <- seq(this_year - object@nyears + 1, this_year + object@proyears)
  years_df <- data.frame(
    year = seq_len(object@proyears + object@nyears), real_year = years
  )
  mps <- data.frame(
    mp = seq_along(object@MPs),
    mp_name = object@MPs, stringsAsFactors = FALSE
  )
  set.seed(seed)
  sampled_ids <- sample(unique(x$iter), size = n_samples)
  dplyr::filter(x, iter %in% sampled_ids) %>%
    dplyr::left_join(years_df, by = "year") %>%
    dplyr::left_join(mps, by = "mp") %>%
    tibble::as_tibble()
}


get_catch_ts <- function(object, this_year, seed = 42, n_samples = 5) {

  x <- purrr::map(object@PPD, "Cat")
  x <- reshape2::melt(x) %>%
    dplyr::rename(iter = .data$Var1, year = .data$Var2, mp = .data$L1) %>%
    dplyr::mutate(type = "projection")
  years <- seq(this_year - object@nyears + 1, this_year + object@proyears)
  years_df <- data.frame(
    year = seq_len(object@proyears + object@nyears), real_year = years
  )
  mps <- data.frame(
    mp = seq_along(object@MPs),
    mp_name = object@MPs, stringsAsFactors = FALSE
  )
  set.seed(seed)
  sampled_ids <- sample(unique(x$iter), size = n_samples)
  dplyr::filter(x, iter %in% sampled_ids) %>%
    dplyr::left_join(years_df, by = "year") %>%
    dplyr::left_join(mps, by = "mp") %>%
    tibble::as_tibble()
}

#' @describeIn plot_index Plots observed catches, i.e., `MSE@PPD[[i]]@Cat`
#' @aliases plot_catch
#' @export
plot_catch <- function(object_list, n_samples = 4, seed = 42, MP = NULL,
                       quantiles = c(0.025, 0.975),
                       french = isTRUE(getOption("french"))) {
  if (!is.list(object_list)) {
    object_list <- list(object_list)
    names(object_list) <- "Scenario"
  }

  if (is.null(object_list[[1]]@OM$CurrentYr[[1]])) {
    warning(
      "Missing `object@OM$CurrentYr`.\n",
      "Please run the MSE with a newer GitHub MSEtool version\n",
      "or set `object@OM$CurrentYr` yourself.\n",
      "Setting CurrentYr = 0 for now.",
      call. = FALSE
    )
    this_year <- 0
  } else {
    this_year <- object_list[[1]]@OM$CurrentYr[[1]]
  }

  d <- purrr::map_dfr(object_list, get_catch_ts,
                      this_year = this_year,
                      seed = seed, n_samples = n_samples, .id = "scenario"
  )
  d_all <- purrr::map_dfr(object_list, get_catch_ts,
                          this_year = this_year,
                          seed = seed, n_samples = object_list[[1]]@nsim, .id = "scenario"
  )
  d_all <- group_by(d_all, scenario, real_year, mp_name) %>%
    summarise(
      lwr = quantile(value, probs = quantiles[[1]], na.rm = TRUE),
      upr = quantile(value, probs = quantiles[[2]], na.rm = TRUE)
    )

  if(!is.null(MP)) {
    d <- dplyr::filter(d, mp_name %in% MP) %>% mutate(mp_name = factor(mp_name, levels = MP))
    d_all <- dplyr::filter(d_all, mp_name %in% MP) %>% mutate(mp_name = factor(mp_name, levels = MP))
  }

  g <- ggplot(
    d[!is.na(d$value), , drop = FALSE],
    aes_string("real_year", "value", group = "as.factor(iter)")
  ) +
    geom_ribbon(
      data = d_all[!is.na(d_all$lwr), , drop = FALSE],
      aes_string(x = "real_year", ymin = "lwr", ymax = "upr"),
      alpha = 0.2, inherit.aes = FALSE
    ) +
    # geom_point(alpha = 0.9) +
    geom_path(alpha = 0.8) +
    facet_grid(mp_name ~ scenario) +
    geom_vline(xintercept = this_year, lty = 2, alpha = 0.5) +
    theme_pbs() +
    theme(panel.spacing = unit(0, "in")) +
    ylab(en2fr("Catch", french, allow_missing = TRUE)) +
    xlab(en2fr("Year", french, allow_missing = TRUE)) +
    guides(colour = "none")

  g
}

