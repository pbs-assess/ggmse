
#' Dot plot
#'
#' @param pm_df_list A named list of performance metric data frames from
#'   [get_probs()]. The names will be used as the plot labels.
#'   Can also be a single data frame from [get_probs()].
#' @param type The type of plot. Multipanel `"facet"` vs. single panel
#'   `"single"`. In the single panel version, a line segment represents the
#'   upper and lower values across the scenarios and the dot represents the
#'   mean.
#' @param custom_pal An optional custom color palette. Should be a named
#'   character vector
#' @param dodge The amount to separate or "dodge" the lollipop lines.
#' @param bar_alpha Background bar transparency. 0 will omit.
#'
#' @return A ggplot2 object
#' @export
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm <- list()
#' pm[[1]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm[[2]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' names(pm) <- c("Scenario 1", "Scenario 2")
#' plot_dots(pm)
#' plot_dots(pm, type = "facet")
plot_dots <- function(pm_df_list, type = c("single", "facet"),
  custom_pal = NULL, dodge = 0.6,
  bar_alpha = 0.2) {
  if (!is.data.frame(pm_df_list)) {
    df <- bind_rows(pm_df_list, .id = "scenario")
  } else {
    df <- pm_df_list
    df$scenario <- ""
  }

  type <- match.arg(type)
  if (type == "single") {
    pm_avg <- condense_func(df, mean, label = "prob")
    pm_min <- condense_func(df, min, label = "min")
    pm_max <- condense_func(df, max, label = "max")
    pm <- dplyr::left_join(pm_avg, pm_min, by = c("MP", "pm")) %>%
      dplyr::left_join(pm_max, by = c("MP", "pm"))
  } else {
    pm <- reshape2::melt(df,
      id.vars = c("MP", "scenario"),
      value.name = "prob",
      variable.name = "pm"
    )
  }
  pm$`Reference` <- ifelse(grepl("ref", pm$MP), "True", "False")

  g <- ggplot(pm, aes_string("pm", "prob", colour = "MP", group = "MP")) +
    theme_pbs() +
    theme(panel.border = element_blank()) +
    annotate(geom = "segment", y = Inf, yend = Inf, x = -Inf, xend = Inf, colour = "grey70") +
    annotate(geom = "segment", y = 0, yend = 0, x = -Inf, xend = Inf, colour = "grey70") +
    annotate(geom = "segment", y = -Inf, yend = Inf, x = Inf, xend = Inf, colour = "grey70") +
    annotate(geom = "segment", y = -Inf, yend = Inf, x = -Inf, xend = -Inf, colour = "grey70")

  if (type == "single") {
    g <- g + geom_linerange(aes_string(ymin = "min", ymax = "max"),
      position = position_dodge(width = dodge), alpha = 0.8, lwd = 0.5
    )
  }

  g <- g + geom_point(aes_string(shape = "`Reference`"),
    position = position_dodge(width = dodge),
  ) +
    ggplot2::scale_shape_manual(values = c(19, 21)) +
    ylab("Probability") + xlab("Performance metric") +
    guides(
      col = guide_legend(order = 1, override.aes = list(pch = 19)),
      shape = guide_legend(override.aes = list(colour = "grey50"))
    ) + theme(
      panel.grid.major.y = element_line(colour = "grey85"),
      panel.grid.minor.y = element_line(colour = "grey96")
    )

  if (type == "facet") {
    g <- g + facet_wrap(~scenario)
  }

  d <- ggplot2::ggplot_build(g)$data[[5]] %>% dplyr::filter(.data$PANEL == 1)
  a <- abs(sort(unique(round(diff(d$x), 9))))
  g <- g + annotate(
    geom = "rect", xmin = d$x - a[[1]] / 2, xmax = d$x + a[[1]] / 2,
    ymin = -Inf, ymax = Inf, fill = "grey75", alpha = bar_alpha
  )

  g <- g + coord_cartesian(
    expand = FALSE, ylim = c(0, 1),
    xlim = range(d$x) + c(-a[1], a[1]), clip = "off"
  )

  if (!is.null(custom_pal)) {
    g <- g + scale_color_manual(values = custom_pal)
  }

  g
}
