plot_historical_ts <- function(object,
  type = c("Catch", "SSB"),
  n_samples = 50,
  this_year = 2018,
  observed_ts = NULL,
  legend_position = c(0.9, 0.85)) {

  all_years <- seq(this_year - rex_historical@Data@LHYear + 1, this_year)

  x <- object@TSdata[[type[[1]]]] %>%
    reshape2::melt() %>%
    dplyr::transmute(
      sample_id = Var1, year = Var2 + min(all_years) - 1,
      value = value, type = "Simulated"
    )

  x <- x %>%
    filter(!is.na(value)) %>%
    # group_by(sample_id)
    mutate(value = value / exp(mean(log(value))))


  outer_bounds <- x %>%
    group_by(year) %>%
    summarize(lower = quantile(value, probs = 0),
      upper = quantile(value, probs = 1))


  if (!is.null(observed_ts)) {
    real_x <- data.frame(year = unique(x$year), value = observed_ts,
      type = "Observed", stringsAsFactors = FALSE) %>%
      mutate(value = value / exp(mean(log(value))))

  }

  sampled_ids <- sample(unique(x$sample_id), size = n_samples)

  d <- dplyr::filter(x, sample_id %in% sampled_ids)

  if (!is.null(observed_ts)) d <- plyr::bind_rows(d, real_x)

  g <- ggplot2::ggplot(d, ggplot2::aes_string("year", "value",
    colour = "type", group = "sample_id"))
  g <- g + ggplot2::geom_ribbon(data = outer_bounds,
    ggplot2::aes_string(x = "year", ymin = "lower", ymax = "upper"),
    colour = NA, fill = "grey90", inherit.aes = FALSE)
  g <- g + ggplot2::geom_line(aes(size = type)) +
    ggplot2::ylab("Historical catch\n(scaled by geometric mean)") +
    ggplot2::xlab("Year") +
    gfplot::theme_pbs() +
    ggplot2::scale_colour_manual(values = c("Simulated" = "#00000050", "Observed" = "red")) +
    ggplot2::scale_size_manual(values = c("Simulated" = 0.4, "Observed" = 0.9)) +
    ggplot2::labs(colour = "Type", size = "Type") +
    ggplot2::theme(legend.position = legend_position)

    if (is.null(observed_ts)) g <- g + guides(colour = FALSE, size = FALSE)
  g
}