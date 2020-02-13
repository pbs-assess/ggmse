#' Make lollipop plot
#'
#' @param pm_df_list A named list of performance metric data frames from
#'   [get_probs()]. The names will be used as the plot labels.
#'   Can also be a single data frame from [get_probs()].
#' @param custom_pal An optional custom color palette. Should be a named
#'   character vector
#' @param dodge The amount to separate or "dodge" the lollipop lines.
#' @param pt_size Point size.
#' @return A ggplot2 object
#' @importFrom ggplot2 geom_linerange coord_flip position_dodge annotate
#' @export
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm <- list()
#' pm[[1]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm[[2]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' names(pm) <- c("Scenario 1", "Scenario 2")
#' plot_lollipop(pm)
#' plot_lollipop(pm[1]) + ggplot2::scale_colour_brewer(palette = "Set2")
plot_lollipop <- function(pm_df_list, custom_pal = NULL, dodge = 0.6, pt_size = 2.25) {
  if (!is.data.frame(pm_df_list)) {
    df <- purrr::map_df(
      names(pm_df_list),
      ~ dplyr::bind_cols(pm_df_list[[.x]],
        scenario = rep(.x, nrow(pm_df_list[[.x]]))
      )
    )
  } else {
    df <- pm_df_list
    df$scenario <- ""
  }

  df_long <- reshape2::melt(df,
    id.vars = c("MP", "scenario"),
    value.name = "prob",
    variable.name = "pm"
  )
  df_long$`Reference` <- ifelse(grepl("ref", df_long$MP), "True", "False")
  df_long$MP <- as.factor(df_long$MP)

  npm <- length(unique(df_long$pm))
  g <- ggplot(df_long, aes_string("pm", "prob", colour = "MP", group = "MP")) +
    theme_pbs() + theme(panel.spacing.x = grid::unit(1.3, "lines")) +
    theme(panel.border = element_blank()) +
    annotate(geom = "segment", y = Inf, yend = Inf, x = -Inf, xend = Inf, colour = "grey70") +
    annotate(geom = "segment", y = 0, yend = 0, x = -Inf, xend = Inf, colour = "grey70") +
    annotate(geom = "segment", y = -Inf, yend = Inf, x = Inf, xend = Inf, colour = "grey70") +
    annotate(geom = "segment", y = -Inf, yend = Inf, x = -Inf, xend = -Inf, colour = "grey70") +
    geom_linerange(aes_string(ymin = "0", ymax = "prob"),
      position = position_dodge(width = dodge), alpha = 0.8, lwd = 0.5
    ) +
    geom_point(aes_string(shape = "`Reference`"),
      position = position_dodge(width = dodge), size = pt_size,
    ) +
    ggplot2::scale_x_discrete(limits = rev(levels(df_long$pm))) +
    coord_flip(
      expand = FALSE, ylim = c(0, 1),
      xlim = c(1 - dodge / 2 - 0.2, npm + dodge / 2 + 0.2), clip = FALSE
    ) +
    facet_wrap(~scenario) +
    ggplot2::scale_shape_manual(values = c(19, 21))

  df_temp <- df_long
  df_temp$prob[df_temp$`Reference` != "True"] <- NA

  g <- g + geom_point(
    data = df_temp,
    position = position_dodge(width = dodge),
    size = pt_size, pch = 21,
    fill = "white", na.rm = TRUE
  )
  # g <- g

  if (!is.null(custom_pal)) {
    g <- g + scale_color_manual(values = custom_pal) +
      scale_fill_manual(values = custom_pal)
  }
  g <- g + theme(
    panel.grid.major.x = element_line(colour = "grey85")
  ) +
    xlab("Performance metric") + ylab("Probability") +
    guides(
      col = guide_legend(order = 1, override.aes = list(pch = 19)),
      shape = guide_legend(override.aes = list(colour = "grey50"))
    )

  g
}
