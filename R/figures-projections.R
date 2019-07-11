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
#'
#' @return ggplot object
#' @export
#' @examples
#' library(DLMtool)
#' om@nsim <- 10
#' x <- runMSE(om, MPs = c("AvC", "CC1"))
#' set.seed(1)
#' plot_projection_ts(x)
#' plot_projection_ts(x, type = "SSB")
plot_projection_ts <- function(object,
                               type = c("F_FMSY", "B_BMSY"),
                               n_samples = 3,
                               this_year = 2018,
                               probs = c(0.1, 0.5),
                               ribbon_colours = RColorBrewer::brewer.pal(8, "Blues")[c(2, 4, 8)],
                               bbmsy_zones = c(0.4, 0.8)) {
  if (!class(object) != "mse") {
    stop(
      "`object` must be aa DLMtool object of class `mse`",
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
      dplyr::rename(iter = Var1, mp = Var2, value = value, year = Var3) %>%
      dplyr::left_join(mps, by = "mp") %>%
      dplyr::mutate(Type = type[i])
  }
  ts_data <- dplyr::bind_rows(ts_data)
  ts_data <- dplyr::left_join(ts_data, years_df, by = "year")

  quantiles <- ts_data %>%
    dplyr::group_by(mp_name, real_year, Type) %>%
    dplyr::summarize(
      median_value = median(value),
      l = quantile(value, probs = 1 - probs[2] / 2),
      u = quantile(value, probs = probs[2] / 2),
      m = quantile(value, probs = 0.50),
      ll = quantile(value, probs = 1 - probs[1] / 2),
      uu = quantile(value, probs = probs[1] / 2)
    )

  sampled_ids <- sample(unique(ts_data$iter), size = n_samples)
  d <- dplyr::filter(ts_data, iter %in% sampled_ids)

  .type_labels <- gsub("_", "/", type)
  .type_labels <- gsub("MSY", "[MSY]", .type_labels)

  type_df <- data.frame(
    Type = type, type_labels = .type_labels,
    stringsAsFactors = FALSE
  )
  d <- dplyr::left_join(d, type_df, by = "Type")
  quantiles <- dplyr::left_join(quantiles, type_df, by = "Type")

  lines <- data.frame(value = bbmsy_zones, type_labels = "B/B[MSY]", stringsAsFactors = FALSE)

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
  g <- g + ggplot2::geom_hline(yintercept = 1, lty = 2, col = "grey30") +
    ggplot2::geom_hline(data = lines, aes_string(yintercept = "value"), lty = 2, col = "grey40")
  g <- g + ggplot2::geom_line(alpha = 0.5, lwd = 0.4) + # sampled lines
    ggplot2::ylab("Projected value") +
    ggplot2::xlab("Year") +
    gfplot::theme_pbs() +
    ggplot2::facet_grid(mp_name ~ type_labels, labeller = ggplot2::label_parsed) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme(panel.spacing = grid::unit(-0.1, "lines"))
  g
}
