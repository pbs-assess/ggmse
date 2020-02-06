#' Make parallel coordinates plot of performance metrics
#'
#' @param pm_df_list A named list of performance metric data frames from
#'   [get_probs()]. The names will be used as the plot labels.
#' @param type The type of plot. Multipanel `"facet"` vs. single panel
#'   `"single"`. In the single panel version, a shaded ribbon represents the
#'   upper and lower values across the scenarios.
#' @param custom_pal An optional custom color palette. Should be a named
#'   character vector
#' @param mp An optional character vector of management procedures to filter to
#'   include.
#' @param rotate_labels Logical: rotate the performance metric labels 90
#'   degrees?
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm <- list()
#' pm[[1]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm[[2]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' names(pm) <- c("Scenario 1", "Scenario 2")
#' plot_parallel_coords(pm)
#' plot_parallel_coords(pm, "single")
plot_parallel_coords <- function(pm_df_list, type = c("facet", "single"),
                                 custom_pal = NULL, mp = NULL,
                                 rotate_labels = type == "facet") {
  type <- match.arg(type)

  df <- purrr::map_df(
    names(pm_df_list),
    ~ dplyr::bind_cols(pm_df_list[[.x]],
      scenario = rep(.x, nrow(pm_df_list[[.x]]))
    )
  )
  if (!is.null(mp)) {
    df <- dplyr::filter(df, MP %in% mp)
  }

  if (type == "facet") {
    df_long <- reshape2::melt(df,
      id.vars = c("MP", "scenario"),
      value.name = "prob",
      variable.name = "pm"
    )
    df_long$`Reference MP` <- ifelse(grepl("ref", df_long$MP), "True", "False")

    nmp <- length(unique(df_long$MP))
    g <- ggplot(df_long, aes_string("pm", "prob", group = "MP", colour = "MP")) +
      ggplot2::geom_line(lwd = 0.7, mapping = ggplot2::aes_string(lty = "`Reference MP`")) +
      ggplot2::coord_cartesian(
        expand = FALSE, ylim = c(min(df_long$prob), 1.0),
        xlim = c(1 - nmp * .05, nmp + nmp * 0.05)
      ) +
      ggplot2::facet_wrap(~scenario)
  } else {
    condense_func <- function(dat, f, label = "prob") {
      dplyr::group_by(dat, MP) %>%
        dplyr::summarise_if(is.numeric, f, na.rm = TRUE) %>%
        reshape2::melt(
          id.vars = "MP",
          value.name = label,
          variable.name = "pm"
        )
    }
    pm_avg <- condense_func(df, mean, label = "mean")
    pm_min <- condense_func(df, min, label = "min")
    pm_max <- condense_func(df, max, label = "max")
    pm <- dplyr::left_join(pm_avg, pm_min, by = c("MP", "pm")) %>%
      dplyr::left_join(pm_max, by = c("MP", "pm"))
    pm$`Reference MP` <- ifelse(grepl("ref", pm$MP), "True", "False")
    g <- ggplot(pm, aes(pm, mean, group = MP, colour = MP)) +
      ggplot2::geom_ribbon(aes(ymin = min, ymax = max, fill = MP), alpha = 0.1, colour = NA) +
      ggplot2::geom_line(alpha = 1, lwd = 0.85, mapping = ggplot2::aes_string(lty = "`Reference MP`")) +
      ggplot2::coord_cartesian(expand = FALSE, ylim = c(min(pm$min), 1))
  }

  g <- g + theme_pbs() + ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(colour = "grey85"),
    panel.grid.major.x = ggplot2::element_line(colour = "grey85"),
    panel.grid.minor.y = ggplot2::element_line(colour = "grey96")
  ) +
    ggplot2::xlab("Performance metric") + ggplot2::ylab("Probability") +
    ggplot2::guides(
      col = ggplot2::guide_legend(order = 1),
      fill = ggplot2::guide_legend(order = 1)
    )
  if (!is.null(custom_pal)) {
    g <- g + ggplot2::scale_color_manual(values = custom_pal) +
      ggplot2::scale_fill_manual(values = custom_pal)
  }
  if (rotate_labels) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  g
}
