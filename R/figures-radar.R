#' Create a radar plot
#'
#' @param pm_df A performance metric data frame from [get_probs()].
#' @param palette A palette color as recognized by [ggplot2::scale_color_brewer()]
#' @param custom_pal A named character vector of custom colors to pass to
#'   [ggplot2::scale_color_manual()]. This argument is used in favour of
#'   `palette` if specified.
#' @param french French?
#' @param ... Other arguments to pass to [ggspider::spider_web()] or [plot_radar()].
#'
#' @return A ggplot object
#' @importFrom ggspider spider_web
#' @export
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' plot_radar(probs)
plot_radar <- function(pm_df,
                       palette = "Set2", custom_pal = NULL,
                       french = isTRUE(getOption("french")), ...) {
  x <- reshape2::melt(pm_df,
    id.vars = "MP",
    value.name = "prob",
    variable.name = "pm"
  )
  g <- ggspider::spider_web(x,
    "MP",
    "pm",
    "prob",
    leg_main_title = en2fr("MP", french),
    leg_lty_title = if (french) "Type de PG" else "MP type",
    palette = palette,
    ...
  )
  if ("ggplot" %in% class(g)) {
    g <- g + ggplot2::labs(color = en2fr("MP", french))
  }
  if (!is.null(custom_pal)) {
    suppressMessages({
      g <- g + ggplot2::scale_color_manual(values = custom_pal)
    })
  }
  g
}

#' @param pm_df_list A named list of performance metric data frames from
#'   [get_probs()]. The names will be used as the plot labels.
#' @param ncol An optional number of columns in the grid.
#' @param nrow An optional number of rows in the grid.
#' @param french French?
#'
#' @export
#' @rdname plot_radar
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm <- list()
#' pm[[1]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm[[2]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' names(pm) <- c("Scenario 1", "Scenario 2")
#' plot_radar_facet(pm)
plot_radar_facet <- function(pm_df_list, custom_pal = NULL,
                             ncol = NULL, nrow = NULL,
                             french = isTRUE(getOption("french")), ...) {
  if (!is.list(pm_df_list)) {
    stop("`pm_df_list` must be a list of data frames from `get_probs()`.",
      call. = FALSE
    )
  }

  gdat <- purrr::map(pm_df_list, plot_radar,
    return_data = TRUE, ...
  )
  g1 <- gdat[[1]]
  spider_data <- purrr::map_df(gdat, "spider_data", .id = "scenario")
  ref_lines_data <- purrr::map_df(gdat, "ref_lines_data", .id = "scenario")
  spokes <- purrr::map_df(gdat, "spokes", .id = "scenario")
  label_data <- purrr::map_df(gdat, "label_data", .id = "scenario")
  spider_data$lty <- ifelse(spider_data$lty == "ref", "True", "False")

  g <- ggplot2::ggplot(spider_data, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_segment(
      data = spokes,
      aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
      color = g1$spoke_color,
      lty = g1$spoke_lty
    ) +
    ggplot2::geom_path(
      data = ref_lines_data,
      aes_string("x", "y", group = "line_num"),
      color = ref_lines_data$color,
      lty = ref_lines_data$lty,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_path(ggplot2::aes_string(
      color = "as.factor(group)",
      linetype = "as.factor(lty)"
    ), lwd = 1) +
    ggplot2::facet_wrap(ggplot2::vars(scenario)) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::geom_text(
      data = spokes,
      aes_string(x = "xend * 1.1", y = "yend * 1.1", label = "spk_nms"),
      colour = "grey30"
    ) +
    ggplot2::scale_color_brewer(
      name = g1$leg_main_title, palette = g1$palette, guide = "legend"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1)
    ) +
    theme_pbs() +
    ggplot2::theme(
      panel.spacing.y = grid::unit(1, "lines"),
      panel.spacing.x = grid::unit(2, "lines")
    ) +
    ggplot2::theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    )

  if (length(g1$ref_lines_to_label)) {
    g <- g + ggplot2::geom_text(
      data = label_data,
      ggplot2::aes_string(x = "x", y = "y", label = "label"),
      color = label_data$color,
      nudge_y = 0.04, hjust = 0, nudge_x = 0.01, inherit.aes = FALSE
    )
  }

  g <- g + ggplot2::labs(colour = en2fr("MP", french), lty = en2fr("Reference", french, allow_missing = TRUE))
  g <- g + ggplot2::theme(strip.text = element_text(size = 11, face = "bold"))

  if (!is.null(custom_pal)) {
    suppressMessages({
      g <- g + ggplot2::scale_color_manual(values = custom_pal)
    })
  }

  g
}
