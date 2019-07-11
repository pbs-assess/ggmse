#' Plot projection time series
#'
#' @param object A DLMtool object of class `mse` that was created by
#'  running [DLMtool::runMSE()].
#' @param type A character vector describing the element of `mse@` to
#'   plot. Each of these "types" will be included as a column in the
#'   final plot.
#' @param n_samples The number of timeseries samples to illustrate.
#' @param this_year The last year of the historical timeseries.
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
                               observed_ts = NULL,
                               legend_position = c(0.9, 0.85)) {
  if (!class(object) != "mse") {
    stop(
      "`object` must be aa DLMtool object of class `mse`",
      "that was created by running `DLMtool::runMSE()`."
    )
  }
  proj_years <- seq(this_year, this_year + object@proyears)

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

  quantiles <- ts_data %>%
    dplyr::group_by(mp_name, year, Type) %>%
    dplyr::summarize(
      median_value = median(value),
      l = quantile(value, probs = 0.75),
      u = quantile(value, probs = 0.25),
      m = quantile(value, probs = 0.50),
      ll = quantile(value, probs = 0.95),
      uu = quantile(value, probs = 0.05)
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

  lines <- data.frame(value = 0.4, type_labels = "B/B[MSY]", stringsAsFactors = FALSE)

  g <- ggplot2::ggplot(d, ggplot2::aes_string("year", "value", group = "iter"))
  g <- g + ggplot2::geom_ribbon(
    data = quantiles,
    ggplot2::aes_string(x = "year", ymin = "ll", ymax = "uu"),
    colour = NA, fill = "grey90", inherit.aes = FALSE
  )
  g <- g + ggplot2::geom_ribbon(
    data = quantiles,
    ggplot2::aes_string(x = "year", ymin = "l", ymax = "u"),
    colour = NA, fill = "grey70", inherit.aes = FALSE
  )
  g <- g + ggplot2::geom_line(
    data = quantiles,
    ggplot2::aes_string(x = "year", y = "m"),
    colour = "grey40", lwd = 1.1, inherit.aes = FALSE
  )
  g <- g + ggplot2::geom_hline(yintercept = 1, lty = 2, col = "grey30") +
    ggplot2::geom_hline(data = lines, aes_string(yintercept = "value"), lty = 2, col = "grey30")
  g <- g + ggplot2::geom_line(alpha = 0.5, lwd = 0.3, lty = 1) +
    ggplot2::ylab("Projected value") +
    ggplot2::xlab("Year") +
    gfplot::theme_pbs() +
    ggplot2::facet_grid(mp_name ~ type_labels, labeller = ggplot2::label_parsed) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme(panel.spacing = grid::unit(-0.1, "lines"))
  g
}
