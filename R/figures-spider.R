#' Create a spider plot
#'
#' @param pm_df A performance metric data frame from [get_probs()].
#' @param palette A palette color as recognized by [ggplot2::scale_color_brewer()]
#' @param ... Other arguments to pass to [ggspider::spider_web()].
#'
#' @return A ggplot object
#' @importFrom ggspider spider_web
#' @export
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' plot_spider(probs)
plot_spider <- function(pm_df,
                        palette = "Set2", ...) {
  x <- reshape2::melt(pm_df,
    id.vars = "MP",
    value.name = "prob",
    variable.name = "pm"
  )
  ggspider::spider_web(x,
    "MP",
    "pm",
    "prob",
    leg_main_title = "MP",
    leg_lty_title = "MP type",
    palette = palette,
    ...
  ) + ggplot2::labs(color = "MP")
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
#' @param spider_margins Logical for margins that work well with [plot_spider()]
#' @param ... Other arguments to pass to [cowplot::plot_grid()]. In particular,
#'   you will probably want to use the `labels` argument.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' g <- list()
#' g[[1]] <- plot_spider(probs)
#' g[[2]] <- plot_spider(probs)
#' plot_grid_pbs(g, labels = c("First plot", "Second plot"), spider_margins = TRUE)
plot_grid_pbs <- function(plotlist, align = "hv",
                          label_fontface = "bold", label_size = 12,
                          hjust = 0, spider_margins = FALSE, ...) {
  out <- cowplot::plot_grid(
    plotlist = plotlist, align = align,
    label_fontface = label_fontface, hjust = hjust,
    label_size = label_size, ...
  )
  if (spider_margins) {
    out <- out +
      ggplot2::theme(plot.margin = grid::unit(c(0.2, 0.2, -0.7, 1.0), "lines"))
  }
  out
}

#' Make a set of spider plots
#'
#' @param pm_df_list A named list of performance metric data frames from [get_probs()]. The names will be used as the plot labels.
#' @param custom_pal An optional custom color palette. Will be fed to [ggplot2::scale_color_manual()].
#' @param ncol An optional number of columns in the grid.
#' @param nrow An optional number of rows in the grid.
#' @param label_size Label size for the plots.
#' @param ... Other arguments to pass to [plot_spider()].
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
#' plot_spider_facet(pm)
plot_spider_facet <- function(pm_df_list, custom_pal = NULL,
  ncol = NULL, nrow = NULL, label_size = 12, ...) {
  if (!is.list(pm_df_list))
    stop("`pm_df_list` must be a list of data frames from `get_probs()`.",
      call. = FALSE)
  g <- purrr::map(pm_df_list, plot_spider, ...)
  gg <- plot_grid_pbs(g, labels = names(pm_df_list), ncol = ncol,
    nrow = nrow, label_size = label_size)
  if (!is.null(custom_pal)) {
    gg <- gg + ggplot2::scale_color_manual(values = custom_pal)
  }
  gg
}