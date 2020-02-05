#' Create a radar plot
#'
#' @param pm_df A performance metric data frame from [get_probs()].
#' @param palette A palette color as recognized by [ggplot2::scale_color_brewer()]
#' @param custom_pal A named character vector of custom colors to pass to
#'   [ggplot2::scale_color_manual()]. This argument is used in favour of
#'   `palette` if specified.
#' @param ... Other arguments to pass to [ggspider::spider_web()].
#'
#' @return A ggplot object
#' @importFrom ggspider spider_web
#' @export
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' plot_radar(probs)
plot_radar <- function(pm_df,
                       palette = "Set2", custom_pal = NULL, ...) {
  x <- reshape2::melt(pm_df,
    id.vars = "MP",
    value.name = "prob",
    variable.name = "pm"
  )
  g <- ggspider::spider_web(x,
    "MP",
    "pm",
    "prob",
    leg_main_title = "MP",
    leg_lty_title = "MP type",
    palette = palette,
    ...
  )
  if ("ggplot" %in% class(g)) {
    g <- g + ggplot2::labs(color = "MP")
  }
  if (!is.null(custom_pal)) {
    suppressMessages({
      g <- g + ggplot2::scale_color_manual(values = custom_pal)
    })
  }
  g
}

#' Plot a grid of ggplots
#'
#' This is a slightly customized wrapper for [cowplot::plot_grid()].
#'
#' @param plotlist A list of plots
#' @param align Alignment character value
#' @param label_fontface Font face
#' @param label_size Label size
#' @param hjust Horizontal adjustment value
#' @param radar_margins Logical for margins that work well with [plot_radar()]
#' @param ... Other arguments to pass to [cowplot::plot_grid()]. In particular,
#'   you will probably want to use the `labels` argument.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' g <- list()
#' g[[1]] <- plot_radar(probs)
#' g[[2]] <- plot_radar(probs)
#' plot_grid_pbs(g, labels = c("First plot", "Second plot"), radar_margins = TRUE)
plot_grid_pbs <- function(plotlist, align = "hv",
                          label_fontface = "bold", label_size = 12,
                          hjust = 0, radar_margins = FALSE, ...) {
  out <- cowplot::plot_grid(
    plotlist = plotlist, align = align,
    label_fontface = label_fontface, hjust = hjust,
    label_size = label_size, ...
  )
  if (radar_margins) {
    out <- out +
      ggplot2::theme(plot.margin = grid::unit(c(0.2, 0.2, -0.7, 1.0), "lines"))
  }
  out
}

#' Make a set of radar plots
#'
#' @param pm_df_list A named list of performance metric data frames from [get_probs()]. The names will be used as the plot labels.
#' @param custom_pal An optional custom color palette. Will be fed to [ggplot2::scale_color_manual()].
#' @param ncol An optional number of columns in the grid.
#' @param nrow An optional number of rows in the grid.
#' @param label_size Label size for the plots.
#' @param ... Other arguments to pass to [plot_radar()].
#'
#' @return
#' A ggplot2 object
#' @export
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm <- list()
#' pm[[1]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm[[2]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' names(pm) <- c("Scenario 1", "Scenario 2")
#' plot_radar_facet(pm)
plot_radar_facet <- function(pm_df_list, custom_pal = NULL,
                             ncol = NULL, nrow = NULL, label_size = 12, ...) {
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

  g <- g + ggplot2::labs(colour = "MP", lty = "Reference MP")
  g <- g + ggplot2::theme(strip.text = element_text(size = 12, face = "bold"))

  if (!is.null(custom_pal)) {
    suppressMessages({
      g <- g + ggplot2::scale_color_manual(values = custom_pal)
    })
  }

  g
}
