#' Plot historical time series
#'
#' @param object A MSEtool object of class `Hist` that was created by
#'  running [MSEtool::runMSE()] with `Hist = TRUE`.
#' @param type A character object describing the element of `object@TSdata` to
#'   plot.
#' @param n_samples The number of timeseries samples to illustrate.
#' @param this_year The last year of the historical timeseries.
#' @param observed_ts An optional observed timeseries to add is a comparison.
#' @param scale Should the timeseries be scaled by their geometric mean?
#' @param legend_position The x and y coordinates of the legend.
#' @importFrom reshape2 melt
#' @importFrom dplyr transmute filter mutate group_by summarize bind_rows
#' @importFrom ggplot2 ggplot aes aes_string scale_colour_manual geom_ribbon
#'   xlab ylab scale_size_manual labs theme geom_line guides
#' @importFrom stats quantile
#' @importFrom methods .hasSlot
#' @importFrom rlang .data
#' @importFrom stats median
#'
#' @return ggplot object
#' @export
#' @examples
#' library(MSEtool)
#' historical_mse <- runMSE(om, Hist = TRUE)
#' plot_historical_ts(historical_mse, n_samples = 2)
#' plot_historical_ts(historical_mse, type = "SSB", n_samples = 2)
#' plot_historical_ts(historical_mse, type = "RecDev", n_samples = 2)
#' plot_historical_ts(historical_mse,
#'   n_samples = 2,
#'   observed_ts = rlnorm(50, 1, 0.3)
#' )
plot_historical_ts <- function(object,
                               type = c("Catch", "Number", "Biomass",
                                 "VBiomass", "SBiomass",
                                 "Removals", "Landings",
                                 "Discards", "Find", "RecDev", "Unfished_Equilibrium"),
                               n_samples = 50,
                               this_year = 2018,
                               observed_ts = NULL,
                               scale = if (!is.null(observed_ts)) TRUE else FALSE,
                               legend_position = c(0.9, 0.85)) {
  if (!.hasSlot(object, "Data") || class(object) != "Hist") {
    stop(
      "`object` must be a MSEtool object of class `Hist`",
      "that was created by running `MSEtool::runMSE()` with",
      "`Hist = TRUE`."
    )
  }
  all_years <- seq(this_year - object@Data@LHYear + 1, this_year)

  # used to be called Catch:
  type <- gsub("Catch", "Removals", type)
  x <- object@TSdata[[type[[1]]]] %>%
    reshape2::melt() %>%
    dplyr::transmute(
      sample_id = Var1, year = Var2 + min(all_years) - 1,
      value = value, type = "Simulated"
    )
  x <- x %>%
    group_by(sample_id, year, type) %>%
    summarize(value = sum(value)) %>% # over areas
    filter(!is.na(value))

  if (scale) x <- mutate(x, value = value / exp(mean(log(value))))


  outer_bounds <- x %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      lower = quantile(value, probs = 0),
      upper = quantile(value, probs = 1)
    )

  if (!is.null(observed_ts)) {
    real_x <- data.frame(
      year = unique(x$year), value = observed_ts,
      type = "Observed", stringsAsFactors = FALSE
    )
    real_x <- mutate(real_x, value = value / exp(mean(log(value))))
  }

  sampled_ids <- sample(unique(x$sample_id), size = n_samples)

  d <- dplyr::filter(x, sample_id %in% sampled_ids)

  if (!is.null(observed_ts)) d <- dplyr::bind_rows(d, real_x)

  g <- ggplot2::ggplot(d, ggplot2::aes_string("year", "value",
    colour = "type", group = "sample_id"
  ))
  g <- g + ggplot2::geom_ribbon(
    data = outer_bounds,
    ggplot2::aes_string(x = "year", ymin = "lower", ymax = "upper"),
    colour = NA, fill = "grey90", inherit.aes = FALSE
  )
  g <- g + ggplot2::geom_line(aes(size = type)) +
    ggplot2::ylab(paste0(
      "Historical ", type[[1]],
      if (scale) "\n(scaled by geometric mean)" else ""
    )) +
    ggplot2::xlab("Year") +
    theme_pbs() +
    ggplot2::scale_colour_manual(values = c("Simulated" = "#00000050", "Observed" = "red")) +
    ggplot2::scale_size_manual(values = c("Simulated" = 0.4, "Observed" = 0.9)) +
    ggplot2::labs(colour = "Type", size = "Type") +
    ggplot2::theme(legend.position = legend_position)

  if (is.null(observed_ts)) g <- g + guides(colour = FALSE, size = FALSE)
  g
}
