#' Plot projection time series
#'
#' @param object A DLMtool object of class `mse` that was created by
#'  running [DLMtool::runMSE()].
#' @param type A character vector describing the element of `mse@` to
#'   plot. Each of these "types" will be included as a column in the
#'   final plot.
#' @param n_samples The number of timeseries samples to illustrate.
#' @param this_year The last year of the historical timeseries.
#' @param probs A numeric vector of the quantiles to plot as ribbons. Must be of length 2.
#' @param ribbon_colours A character vector of length three giving the colours for
#'   the outer uncertainty ribbon, inner uncertainty ribbon, and median.
#' @param bbmsy_zones A numeric vector of status zone lines to add to
#'   the B/Bmsy panel if it is present.
#' @param catch_reference The number of years over which to calculate the
#'   average catch and show as a horizontal line in a plot of catch.
#' @param clip_ylim An optional numeric value to set the upper ylim
#'   value. `clip_ylim` is multiplied by the highest median value
#'   across the panels. Useful if there are outlying timeseries that distort
#'   the y axis limits.
#' @param seed The seed to set before drawing samples.
#'
#' @return ggplot object
#' @importFrom rlang .data
#' @export
#' @examples
#' library(DLMtool)
#' om@nsim <- 10
#' x <- runMSE(om, MPs = c("AvC", "CC1"))
#' set.seed(1)
#' plot_projection_ts(x)
#' plot_projection_ts(x, type = "SSB")
plot_projection_ts <- function(object,
                               type = c("SSB", "FM"),
                               n_samples = 3,
                               this_year = 2018,
                               probs = c(0.1, 0.5),
                               ribbon_colours = RColorBrewer::brewer.pal(8, "Blues")[c(2, 4, 8)],
                               bbmsy_zones = c(0.4, 0.8),
                               catch_reference = 1,
                               clip_ylim = NULL,
                               seed = 42) {
  ts_data <- get_ts(object = object, type = type, this_year = this_year)
  quantiles <- get_ts_quantiles(ts_data, probs = probs)

  set.seed(seed)
  sampled_ids <- sample(unique(ts_data$iter), size = n_samples)
  d <- dplyr::filter(ts_data, .data$iter %in% sampled_ids)

  .type_labels <- gsub("_", "/", unique(d$Type))
  .type_labels <- gsub("MSY", "[MSY]", .type_labels)
  .type_labels <- gsub("B/B\\[MSY\\]", "SSB/SSB[MSY]", .type_labels)

  type_df <- data.frame(
    Type = unique(d$Type), type_labels = .type_labels,
    stringsAsFactors = FALSE
  )
  if (all(type == c("SSB", "FM"))) {
    type_df$type_labels <- factor(type_df$type_labels, levels = c("SSB/SSB[MSY]", "F/F[MSY]"))
  }

  d <- dplyr::left_join(d, type_df, by = "Type")
  quantiles <- dplyr::left_join(quantiles, type_df, by = "Type")

  # lines <- data.frame(value = bbmsy_zones, type_labels = "SSB/SSB[MSY]", stringsAsFactors = FALSE)
  # if (all(type == c("SSB", "FM"))) {
  #   browser()
  #   lines$type_labels <- factor(lines$type_labels, levels = c("SSB/SSB[MSY]", "F/F[MSY]"))
  # }

  g <- ggplot2::ggplot(d, ggplot2::aes_string("real_year", "value", group = "iter"))
  g <- g + ggplot2::geom_ribbon(
    data = quantiles,
    ggplot2::aes_string(x = "real_year", ymin = "ll", ymax = "uu"),
    colour = NA, fill = ribbon_colours[1], inherit.aes = FALSE
  )
  g <- g + ggplot2::geom_ribbon(
    data = quantiles,
    ggplot2::aes_string(x = "real_year", ymin = "l", ymax = "u"),
    colour = NA, fill = ribbon_colours[2], inherit.aes = FALSE
  )
  g <- g + ggplot2::geom_line(
    data = quantiles,
    ggplot2::aes_string(x = "real_year", y = "m"),
    colour = ribbon_colours[3], lwd = 1.1, inherit.aes = FALSE
  )

  if ("SSB" %in% type || "FM" %in% type) {
    g <- g + ggplot2::geom_hline(yintercept = 1, alpha = 0.2, lty = 2, lwd = 0.5)
  }
  # if ("SSB" %in% type) {
  #   g <- g + ggplot2::geom_hline(yintercept = 0.4, alpha = 0.2, lty = 2, lwd = 0.5)
  #   g <- g + ggplot2::geom_hline(yintercept = 0.8, alpha = 0.2, lty = 2, lwd = 0.5)
  # }

  if ("Catch" %in% type) {
    average_catch <- filter(d, Type == "Catch", real_year %in%
        .hist_years[(length(.hist_years)-(catch_reference-1)):length(.hist_years)]) %>%
      summarize(average_catch = mean(value)) %>% pull(average_catch)
    g <- g + ggplot2::geom_hline(yintercept = average_catch, alpha = 0.2, lty = 2, lwd = 0.5)
  }
  g <- g + ggplot2::geom_line(alpha = 0.3, lwd = 0.4) + # sampled lines
    ggplot2::ylab("Value") +
    ggplot2::xlab("Year") +
    gfplot::theme_pbs() +
    ggplot2::facet_grid(mp_name ~ type_labels, labeller = ggplot2::label_parsed) +
    ggplot2::coord_cartesian(expand = FALSE,
      ylim = if (is.null(clip_ylim)) NULL else c(0, clip_ylim * max(quantiles$m))) +
    ggplot2::theme(panel.spacing = grid::unit(-0.1, "lines")) +
    geom_vline(xintercept = this_year, lty = 2, alpha = 0.3)
  g

}


get_ts <- function(object,
  type = c("SSB", "FM"),
  this_year = 2018) {
  if (!class(object) != "mse") {
    stop(
      "`object` must be a DLMtool object of class `mse`",
      "that was created by running `DLMtool::runMSE()`."
    )
  }
  .proj_years <- seq(this_year + 1, this_year + object@proyears)
  years_df <- data.frame(
    year = seq_len(object@proyears), real_year = .proj_years,
    stringsAsFactors = FALSE
  )

  mps <- data.frame(
    mp = seq_along(object@MPs),
    mp_name = object@MPs, stringsAsFactors = FALSE
  )

  ts_data <- list()
  for (i in seq_along(type)) {
    ts_data[[i]] <- slot(object, type[i]) %>%
      reshape2::melt() %>%
      dplyr::rename(
        iter = .data$Var1, mp = .data$Var2,
        value = .data$value, year = .data$Var3
      ) %>%
      dplyr::left_join(mps, by = "mp") %>%
      dplyr::mutate(Type = type[i])
  }
  ts_data <- dplyr::bind_rows(ts_data)
  ts_data <- dplyr::left_join(ts_data, years_df, by = "year")
  iters <- dim(object@SSB)[1]
  ts_data$iter <- rep(seq_len(iters), length(unique(ts_data$Type))) # parallel messed this up

  # --------------
  # historical

  n_hist_years <- dim(object@SSB_hist)[3]
  .hist_years <- seq(this_year - n_hist_years + 1, this_year)
  years_df <- data.frame(
    year = seq_len(n_hist_years), real_year = .hist_years,
    stringsAsFactors = FALSE
  )
  hist_data <- list()

  for (i in seq_along(type)) {
    hist_data[[i]] <- slot(object, paste0(type[i],
      ifelse(type[i] == "C", "B_hist", "_hist"))) %>%
      apply(c(1, 3), if(type[i] == "FM") max else sum) %>%
      reshape2::melt() %>%
      dplyr::rename(
        iter = .data$Var1,
        value = .data$value, year = .data$Var2
      ) %>%
      dplyr::mutate(Type = type[i])
  }
  hist_data <- dplyr::bind_rows(hist_data)
  hist_data$iter <- rep(seq_len(iters), length(unique(hist_data$Type))) # parallel messed this up
  hist_data <- dplyr::left_join(hist_data, years_df, by = "year")
  hist_data2 <- do.call("rbind",
    replicate(length(mps$mp), hist_data, simplify = FALSE))
  hist_data2[["mp_name"]] <- rep(mps$mp_name, each = nrow(hist_data))

  ts_data <- bind_rows(ts_data, hist_data2)

  ref_ssb <- data.frame(ref = object@Misc$MSYRefs$Refs$SSBMSY, iter = seq_len(iters),
    Type = "SSB", stringsAsFactors = FALSE)
  ref_msy <- data.frame(ref = 1, iter = seq_len(iters), Type = "C", stringsAsFactors = FALSE)
  ref_f <- data.frame(ref = object@Misc$MSYRefs$Refs$FMSY, iter = seq_len(iters), Type = "FM", stringsAsFactors = FALSE)
  refs <- bind_rows(ref_ssb, ref_msy) %>%
    bind_rows(ref_f)

  ts_data <- left_join(ts_data, refs, by = c("iter", "Type")) %>%
    mutate(value = value / ref)

  type[type == "SSB"] <- "B_BMSY"
  type[type == "FM"] <- "F_FMSY"
  type[type == "C"] <- "Catch"
  ts_data$Type[ts_data$Type == "FM"] <- "F_FMSY"
  ts_data$Type[ts_data$Type == "SSB"] <- "B_BMSY"
  ts_data$Type[ts_data$Type == "C"] <- "Catch"
  ts_data
}

get_ts_quantiles <- function(x, probs = c(0.1, 0.5)) {
  x %>%
    dplyr::group_by(.data$mp_name, .data$real_year, .data$Type) %>%
    dplyr::summarize(
      median_value = median(.data$value),
      u = quantile(.data$value, probs = 1 - probs[2] / 2),
      l = quantile(.data$value, probs = probs[2] / 2),
      m = quantile(.data$value, probs = 0.50),
      uu = quantile(.data$value, probs = 1 - probs[1] / 2),
      ll = quantile(.data$value, probs = probs[1] / 2)
    ) %>%
    ungroup()
}