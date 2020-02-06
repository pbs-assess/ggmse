#' Sensitivity trajectory plot
#'
#' Plot sensitivity of performance metrics to OM slots across iterations as
#' trajectories. First the iterations are divided into the upper and lower
#' thirds of the respective OM slot values. Then ribbon plots are made within
#' those groups across iterations through time.
#'
#' @param object A DLMtool MSE object.
#' @param type Whether to make a B/Bmsy or F/Fmsy projection plot.
#' @param mp A character vector of MPs to plot (must be included in the MSE
#'   object). Defaults to all.
#' @param slots A character vector of OM slots to plot. Will be plotted in this
#'   order. Set `slots = "all"` to plot all available OM and observation slots.
#' @param probs A numeric value corresponding to the tail probability for the ribbon.
#'   E.g., 0.5 corresponds to a ribbon at 0.25 and 0.75 quantiles.
#' @param this_year The last year of the historical timeseries.
#'
#' @return
#' A ggplot object
#' @export
#'
#' @examples
#' plot_sensitivity_trajectory(mse_example)
plot_sensitivity_trajectory <- function(object, type = c("B_BMSY", "F_FMSY"), mp = object@MPs,
                                        slots = c("D", "hs", "M", "ageM", "L50", "Linf", "K", "Isd"),
                                        probs = 0.3, this_year = 2018) {
  type <- match.arg(type)
  if (class(object) != "MSE") {
    stop("`object` must be class 'MSE'", call. = FALSE)
  }
  if (any(!slots %in% union(colnames(object@OM), colnames(object@Obs)))) {
    stop("All `slots` must be valid `object@OM` or `object@Obs` slot names.", call. = FALSE)
  }

  obs <- suppressMessages(reshape2::melt(object@Obs,
    variable.name = "om_slot", value.name = "om_value"
  )) %>%
    mutate(iter = rep(seq_len(dim(object@Obs)[1]), dim(object@Obs)[2])) %>%
    mutate(slot_type = "Obs")
  om <- suppressMessages(reshape2::melt(object@OM,
    id.vars = NULL, variable.name = "om_slot", value.name = "om_value"
  )) %>%
    mutate(iter = rep(seq_len(dim(object@OM)[1]), dim(object@OM)[2])) %>%
    mutate(slot_type = "OM")
  om <- suppressWarnings(bind_rows(obs, om))

  dat_ts <- get_ts(object)
  dat <- dplyr::inner_join(om, dat_ts, by = "iter")

  if (slots[[1]] == "all" && length(slots) == 1L) {
    slots <- union(colnames(object@OM), colnames(object@Obs))
  }

  dat <- dat %>%
    dplyr::filter(Type == type) %>%
    dplyr::filter(mp %in% mp, om_slot %in% slots) %>%
    group_by(om_slot) %>%
    mutate(om_value_upr = quantile(om_value, probs = 0.666, na.rm = TRUE)) %>%
    mutate(om_value_lwr = quantile(om_value, probs = 0.333, na.rm = TRUE)) %>%
    dplyr::filter(om_value < om_value_lwr | om_value > om_value_upr) %>%
    mutate(om_value_group = ifelse(om_value < om_value_lwr, "Lower", "Upper"))

  dat_summarized <- dat %>%
    dplyr::group_by(om_value_group, om_slot, mp_name, real_year, Type) %>%
    dplyr::summarize(
      l = quantile(.data$value, probs = 1 - probs[1] / 2),
      u = quantile(.data$value, probs = probs[1] / 2),
      m = quantile(.data$value, probs = 0.5)
    ) %>%
    ungroup()

  ylab <- if (type == "B_BMSY") expression(B / B[MSY]) else expression(F / F[MSY])
  pal <- RColorBrewer::brewer.pal(3, "Set2")[c(2, 3)]
  dat_summarized %>%
    mutate(om_slot = factor(om_slot, levels = slots)) %>%
    ggplot2::ggplot(aes(real_year, m)) +
    ggplot2::geom_ribbon(mapping = aes(ymin = l, ymax = u, fill = om_value_group), alpha = 0.5) +
    ggplot2::geom_line(mapping = aes(colour = om_value_group)) +
    ggplot2::facet_grid(mp_name ~ om_slot) +
    theme_pbs() +
    ggplot2::labs(
      x = "Year", y = ylab,
      colour = "OM value\nthird", fill = "OM value\nthird"
    ) +
    geom_vline(xintercept = this_year, lty = 2, alpha = 0.3) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.3) +
    ggplot2::scale_color_manual(values = c("Lower" = pal[1], "Upper" = pal[2])) +
    ggplot2::scale_fill_manual(values = c("Lower" = pal[1], "Upper" = pal[2]))
  # scale_x_continuous(breaks = seq(min(dat_summarized$real_year), max(dat_summarized$real_year), 10))
}
