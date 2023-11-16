
#' @name plot_scenario
#' @title Plot a single time series for various scenarios and MPs
#' @description
#' Plot historical and projected time series for various scenarios (B, B/BMSY, catch, etc.) either
#' (1) separate panels by MP that overlap by OM scenario, or (2) as a full grid by OM scenario and MP.
#' See also [plot_scenario_index()].
#'
#' @param object_list A named list of MSE objects from MSEtool. Names become scenario names.
#' @param MP Optional subset of management procedures
#' @param quantiles Length 2 vector for the quantiles to plot as ribbons
#' @param facet Character to indicate a wrapped facet plot by MP or grid by OM and MP. See description.
#' @param palette The name of an RColorBrewer colour palette
#' @param n_samples The number of timeseries samples to illustrate. Only used if `facet = "grid"`.
#' @param seed The seed to set before drawing samples
#' @param french Logical to use French translation.
#' @aliases plot_scenario_BMSY
#'
#' @export
plot_scenario_BMSY <- function(object_list, MP = NULL, quantiles = c(0.025, 0.975),
                               facet = c("wrap", "grid"),
                               palette = "Dark2",
                               n_samples = 0,
                               seed = 42,
                               french = isTRUE(getOption("french"))) {

  g <- .plot_scenario_ts(object_list, MP, quantiles, n_samples, seed, facet, palette, var = "BMSY", french)
  g
}

#' @rdname plot_scenario
#' @aliases plot_scenario_SSB
#' @export
plot_scenario_SSB <- function(object_list, MP = NULL, quantiles = c(0.025, 0.975),
                              facet = c("wrap", "grid"),
                              palette = "Dark2",
                              n_samples = 2,
                              seed = 42,
                              french = isTRUE(getOption("french"))) {

  g <- .plot_scenario_ts(object_list, MP, quantiles, n_samples, seed, facet, palette, var = "B", french)
  g
}

#' @rdname plot_scenario
#' @aliases plot_scenario_F
#' @export
plot_scenario_F <- function(object_list, MP = NULL, quantiles = c(0.025, 0.975),
                            facet = c("wrap", "grid"),
                            palette = "Dark2",
                            n_samples = 2,
                            seed = 42,
                            french = isTRUE(getOption("french"))) {

  g <- .plot_scenario_ts(object_list, MP, quantiles, n_samples, seed, facet, palette, var = "F", french)
  g
}

#' @rdname plot_scenario
#' @aliases plot_scenario_FMSY
#' @export
plot_scenario_FMSY <- function(object_list, MP = NULL, quantiles = c(0.025, 0.975),
                               facet = c("wrap", "grid"),
                               palette = "Dark2",
                               n_samples = 2,
                               seed = 42,
                               french = isTRUE(getOption("french"))) {

  g <- .plot_scenario_ts(object_list, MP, quantiles, n_samples, seed, facet, palette, var = "FMSY", french)
  g
}

#' @rdname plot_scenario
#' @aliases plot_scenario_catch
#' @export
plot_scenario_catch <- function(object_list, MP = NULL, quantiles = c(0.025, 0.975),
                                facet = c("wrap", "grid"),
                                palette = "Dark2",
                                n_samples = 2,
                                seed = 42,
                                french = isTRUE(getOption("french"))) {

  g <- .plot_scenario_ts(object_list, MP, quantiles, n_samples, seed, facet, palette, var = "catch", french)
  g
}


#' @importFrom rlang .data
.plot_scenario_ts <- function(object_list, MP = NULL, quantiles = c(0.025, 0.975), n_samples = 2, seed = 1,
                              facet = c("wrap", "grid"),
                              palette = "Dark2",
                              var = c("BMSY", "B", "F", "FMSY", "catch"),
                              french = isTRUE(getOption("french"))) {
  var <- match.arg(var)
  facet <- match.arg(facet)

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

  d_all <- purrr::map_dfr(
    object_list, .get_ts,
    this_year = this_year,
    n_samples = object_list[[1]]@nsim,
    var = var,
    .id = "scenario"
  )
  d <- group_by(d_all, scenario, real_year, mp_name) %>%
    summarise(value = median(.data$value, na.rm = TRUE))

  d_all <- group_by(d_all, scenario, real_year, mp_name) %>%
    summarise(
      lwr = quantile(.data$value, probs = quantiles[[1]], na.rm = TRUE),
      upr = quantile(.data$value, probs = quantiles[[2]], na.rm = TRUE)
    )

  if(!is.null(MP)) {
    d <- dplyr::filter(d, .data$mp_name %in% MP) %>% mutate(mp_name = factor(.data$mp_name, levels = MP))
    d_all <- dplyr::filter(d_all, .data$mp_name %in% MP) %>% mutate(mp_name = factor(.data$mp_name, levels = MP))
  }

  ylab <- switch(
    var,
    "B" = "Spawning~biomass",
    "BMSY" = "B/B[MSY]",
    "F" = "Fishing~mortality",
    "FMSY" = "F/F[MSY]",
    "catch" = "Catch",
    NULL
  )

  if (facet == "grid") {

    d_samp <- purrr::map_dfr(
      object_list, .get_ts,
      this_year = this_year,
      n_samples = n_samples,
      var = var,
      seed = seed,
      .id = "scenario"
    ) %>%
      mutate(iter = as.factor(.data$iter))

    g <- ggplot(
      d[!is.na(d$value), , drop = FALSE],
      aes(.data$real_year, .data$value)
    ) +
      geom_ribbon(
        data = d_all[!is.na(d_all$lwr), , drop = FALSE],
        aes(x = .data$real_year, ymin = .data$lwr, ymax = .data$upr),
        alpha = 0.2, inherit.aes = FALSE
      ) +
      geom_path(data = d_samp, aes(group = .data$iter), alpha = 0.8) +
      geom_line(linetype = 3) +
      facet_grid(mp_name ~ scenario, scales = "free_y") +
      geom_vline(xintercept = this_year, lty = 2, alpha = 0.5) +
      theme_pbs() +
      theme(panel.spacing = unit(0, "in")) +
      ylab(parse(text = en2fr(ylab, french, allow_missing = TRUE))) +
      xlab(en2fr("Year", french, allow_missing = TRUE)) +
      guides(colour = "none")
  } else {
    g <- ggplot(
      d[!is.na(d$value), , drop = FALSE],
      aes(.data$real_year, .data$value, colour = scenario)
    ) +
      geom_ribbon(
        data = d_all[!is.na(d_all$lwr), , drop = FALSE],
        aes(.data$real_year, ymin = .data$lwr, ymax = .data$upr, fill = .data$scenario),
        alpha = 0.2, inherit.aes = FALSE
      ) +
      geom_line() +
      facet_wrap(vars(mp_name)) +
      geom_vline(xintercept = this_year, lty = 2, alpha = 0.5) +
      theme_pbs() +
      theme(panel.spacing = unit(0, "in")) +
      ylab(parse(text = en2fr(ylab, french, allow_missing = TRUE))) +
      xlab(en2fr("Year", french, allow_missing = TRUE)) +
      labs(colour = en2fr("Operating model", french),
           fill = en2fr("Operating model", french)) +
      scale_colour_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      theme(legend.position = "bottom")
  }

  g
}



.get_ts <- function(object, this_year, MP = NULL,
                    var = c("BMSY", "B", "F", "FMSY", "catch"),
                    n_samples = 5, seed = 42) {

  if(is.null(MP)) {
    MP_ind <- 1:object@nMPs
  } else {
    MP_ind <- match(MP, object@MPs)
  }

  hist_obj <- switch(
    var,
    "B" = object@SSB_hist,
    "BMSY" = object@SSB_hist/object@RefPoint$ByYear$SSBMSY[, object@nyears],
    "F" = object@FM_hist,
    "FMSY" = object@FM_hist/object@RefPoint$ByYear$FMSY[, object@nyears],
    "catch" = object@CB_hist,
    NULL
  )
  hist <- array(hist_obj, c(object@nsim, object@nyears, length(MP_ind))) %>%
    aperm(c(1, 3, 2)) %>%
    reshape2::melt() %>%
    dplyr::rename(iter = .data$Var1, mp = .data$Var2, year = .data$Var3)

  pro_obj <- switch(
    var,
    "B" = object@SSB[, MP_ind, ],
    "BMSY" = object@SB_SBMSY[, MP_ind, ],
    "F" = object@FM[, MP_ind, ],
    "FMSY" = object@F_FMSY[, MP_ind, ],
    "catch" = object@Catch[, MP_ind, ],
    NULL
  )
  pro <- reshape2::melt(pro_obj) %>%
    dplyr::rename(iter = .data$Var1, mp = .data$Var2, year = .data$Var3) %>%
    dplyr::mutate(year = object@nyears + year)

  x <- rbind(hist, pro)
  years <- seq(this_year - object@nyears + 1, this_year + object@proyears)
  years_df <- data.frame(
    year = seq_len(object@proyears + object@nyears), real_year = years
  )
  mps <- data.frame(
    mp = 1:length(MP_ind),
    mp_name = object@MPs[MP_ind], stringsAsFactors = FALSE
  )

  if (n_samples < object@nsim) {
    set.seed(seed)
    sampled_ids <- sample(unique(x$iter), size = n_samples)
    x <- dplyr::filter(x, .data$iter %in% sampled_ids)
  }

  x %>%
    dplyr::left_join(years_df, by = "year") %>%
    dplyr::left_join(mps, by = "mp") %>%
    tibble::as_tibble()
}
