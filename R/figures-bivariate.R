#' Make a trade-off scatterplot
#'
#' Creates a trade-off scatterplot of 2 performance metrics across multiple
#' scenarios
#'
#' @param pm_df_list A named list of performance metric data frames from
#'   [get_probs()]. The names will be used as the plot labels.
#' @param xvar The performance metric for the x axis (as character).
#' @param yvar The performance metric for the y axis (as character).
#' @param custom_pal An optional custom color palette. Should be a named
#'   character victor.
#' @param mp An optional character vector of MPs to include. By default includes
#'   all.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' probs <- list(get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY"))
#' names(probs) <- "Scenario 1"
#' plot_tradeoff(probs, "P40", "LTY")

plot_tradeoff <- function(pm_df_list, xvar, yvar, custom_pal = NULL, mp = NULL) {

  df <- purrr::map_df(
    names(pm_df_list),
    ~ dplyr::bind_cols(pm_df_list[[.x]],
      scenario = rep(.x, nrow(pm_df_list[[.x]]))
    )
  )

  if (!is.null(mp)) {
    df <- dplyr::filter(df, MP %in% mp)
  }
  df_long <- reshape2::melt(df,
    id.vars = c("MP", "scenario"),
    value.name = "prob",
    variable.name = "pm"
  )
  df_wide <- df_long %>%
    reshape2::dcast(MP + scenario ~ pm, value.var = "prob") %>%
    dplyr::mutate(`Reference MP` = ifelse(grepl("ref", MP), "True", "False"))

  xmin <- pull(df_wide, !!xvar) %>% min()
  ymin <- pull(df_wide, !!yvar) %>% min()
  xvar <- paste0("`", xvar, "`")
  yvar <- paste0("`", yvar, "`")

  g <- ggplot2::ggplot(df_wide,
    ggplot2::aes_string(xvar, yvar, colour = "MP", pch = "`Reference MP`")) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~scenario, nrow = 2) +
    theme_pbs() +
    ggplot2::coord_equal(
      xlim = c(xmin * 0.99, 1.005),
      ylim = c(ymin * 0.99, 1.005), expand = FALSE
    ) +
    ggplot2::scale_shape_manual(values = c(19, 21)) +
    ggplot2::guides(col = ggplot2::guide_legend(order = 1))

  if (!is.null(custom_pal)) {
    g <- g + ggplot2::scale_color_manual(values = custom_pal)
  }

  g <- g + ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(colour = "grey85"),
    panel.grid.major.x = ggplot2::element_line(colour = "grey85")
  )

  g
}
