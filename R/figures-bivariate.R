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
#'   character vector.
#' @param mp An optional character vector of MPs to include. By default includes
#'   all.
#' @param nudge_x How far to nudge the labels left/right from the x-value for \link[ggplot2]{geom_text}
#' @param nudge_y How far to nudge the labels up/down from the y-value for \link[ggplot2]{geom_text}
#' @param french French
#'
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' probs <- list(get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY"))
#' names(probs) <- "Scenario 1"
#' plot_tradeoff(probs, "P40", "LTY")
plot_tradeoff <- function(pm_df_list, xvar, yvar, custom_pal = NULL, mp = NULL,
                          nudge_x = 0, nudge_y = 0.05,
                          french = FALSE) {
  #df <- purrr::map_df(
  #  names(pm_df_list),
  #  ~ dplyr::bind_cols(pm_df_list[[.x]],
  #    scenario = rep(.x, nrow(pm_df_list[[.x]]))
  #  )
  #)

  df <- lapply(names(pm_df_list), function(x) {
    pm_df_list[[x]] %>%
      mutate(scenario = x, MP_label = 1:nrow(.))
  }) %>%
    bind_rows()

  if (!is.null(mp)) {
    df <- dplyr::filter(df, MP %in% mp) %>% mutate(MP = factor(MP, levels = mp))
  }
  df_long <- reshape2::melt(df,
    id.vars = c("MP", "scenario", "MP_label"),
    value.name = "prob",
    variable.name = "pm"
  )
  df_wide <- df_long %>%
    reshape2::dcast(MP + MP_label + scenario ~ pm, value.var = "prob") %>%
    dplyr::mutate(`Reference` = ifelse(grepl("ref", MP), "True", "False"))

  xmin <- pull(df_wide, !!xvar) %>% min()
  ymin <- pull(df_wide, !!yvar) %>% min()
  xvar <- paste0("`", xvar, "`")
  yvar <- paste0("`", yvar, "`")

  n_mp <- length(unique(df_wide$MP))
  ref_or_not <- dplyr::select(df_wide, .data$MP, .data$Reference) %>% dplyr::distinct()
  mp_shapes <- vector(mode = "numeric", length = n_mp)
  mp_shapes <- ifelse(ref_or_not$Reference == "True", 21, 19)
  names(mp_shapes) <- ref_or_not$MP
  mp_label <- filter(df_wide, scenario == names(pm_df_list)[[1]]) %>% pull("MP_label")

  g <- ggplot2::ggplot(
    df_wide,
    ggplot2::aes_string(xvar, yvar, colour = "MP", pch = "MP")
  ) +
    ggplot2::geom_text(aes_string(label = "MP_label"), nudge_x = nudge_x, nudge_y = nudge_y) +
    ggplot2::geom_point(show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = mp_shapes) +
    guides(colour = guide_legend(override.aes = list(label = mp_label))) +
    ggplot2::facet_wrap(vars(scenario), nrow = 2) +
    theme_pbs() +
    ggplot2::coord_equal(
      xlim = c(xmin * 0.99, 1.005),
      ylim = c(ymin * 0.99, 1.005), expand = FALSE
    )

  if (!is.null(custom_pal)) {
    g <- g + ggplot2::scale_color_manual(values = custom_pal)
  }

  g <- g + ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(colour = "grey85"),
    panel.grid.major.x = ggplot2::element_line(colour = "grey85")
  ) +
    labs(shape = en2fr("MP", french), colour = en2fr("MP", french), fill = en2fr("MP", french))
  g
}
