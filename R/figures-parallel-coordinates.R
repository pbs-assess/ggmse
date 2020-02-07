#' Make parallel coordinates plot of performance metrics
#'
#' @param pm_df_list A named list of performance metric data frames from
#'   [get_probs()]. The names will be used as the plot labels.
#' @param type The type of plot. Multipanel `"facet"` vs. single panel
#'   `"single"`. In the single panel version, a shaded ribbon represents the
#'   upper and lower values across the scenarios on the line represents the
#'   mean.
#' @param custom_pal An optional custom color palette. Should be a named
#'   character vector
#' @param groups An optional grouping structure to separate the lines. For
#'   example, this allows for conservation and fisheries performance metrics to
#'   use distinct line segments. Should be a list with the performance metrics
#'   grouped. See the example below.
#' @param rotate_labels Logical: rotate the performance metric labels 90
#'   degrees?
#' @importFrom ggplot2 coord_cartesian geom_ribbon guide_legend xlab ylab
#'   element_line scale_color_manual scale_colour_manual scale_fill_manual theme
#'   element_text aes_string guide_legend guides
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
#' plot_parallel_coords(pm, groups = list(c("P40", "P100", "PNOF"), c("LTY", "AAVY")))
plot_parallel_coords <- function(pm_df_list, type = c("facet", "single"),
                                 custom_pal = NULL,
                                 groups = NULL,
                                 rotate_labels = type == "facet") {
  type <- match.arg(type)

  df <- purrr::map_df(
    names(pm_df_list),
    ~ dplyr::bind_cols(pm_df_list[[.x]],
      scenario = rep(.x, nrow(pm_df_list[[.x]]))
    )
  )

  if (!is.null(groups)) {
    ids <- purrr::map_df(groups, ~ tibble::tibble(pm = .x), .id = "group")
  }

  if (type == "facet") {
    df_long <- reshape2::melt(df,
      id.vars = c("MP", "scenario"),
      value.name = "prob",
      variable.name = "pm"
    )
    df_long$`Reference MP` <- ifelse(grepl("ref", df_long$MP), "True", "False")

    if (!is.null(groups)) {
      if (is.factor(df_long$pm)) {
        ids$pm <- factor(ids$pm, levels = levels(df_long$pm))
      }
      df_long <- left_join(df_long, ids, by = "pm")
      df_long$group <- paste(df_long$group, df_long$MP)
    } else {
      df_long$group <- df_long$MP
    }

    npm <- length(unique(df_long$pm))
    g <- ggplot(df_long, aes_string("pm", "prob", group = "group", colour = "MP")) +
      geom_line(lwd = 0.7, mapping = aes_string(lty = "`Reference MP`")) +
      coord_cartesian(
        expand = FALSE, ylim = c(min(df_long$prob), 1.0),
        xlim = c(1 - 0.2, npm + 0.2)
      ) +
      facet_wrap(~scenario)
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

    if (!is.null(groups)) {
      if (is.factor(pm$pm)) {
        ids$pm <- factor(ids$pm, levels = levels(pm$pm))
      }
      pm <- left_join(pm, ids, by = "pm")
      pm$group <- paste(pm$group, pm$MP)
    } else {
      pm$group <- pm$MP
    }

    npm <- length(unique(pm$pm))
    g <- ggplot(pm, aes_string("pm", "mean", group = "group", colour = "MP")) +
      geom_ribbon(aes(ymin = min, ymax = max, fill = MP), alpha = 0.1, colour = NA) +
      geom_line(alpha = 1, lwd = 0.85, mapping = aes_string(lty = "`Reference MP`")) +
      coord_cartesian(expand = FALSE, ylim = c(min(pm$min), 1),
        xlim = c(1 - 0.2, npm + 0.2))
  }

  g <- g + theme_pbs()

  if (!is.null(custom_pal)) {
    g <- g + scale_color_manual(values = custom_pal) +
      scale_fill_manual(values = custom_pal)
  }
  if (rotate_labels) {
    g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  g <- g + theme(
    panel.grid.major.y = element_line(colour = "grey85"),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.minor.y = element_line(colour = "grey96")
  ) +
    xlab("Performance metric") + ylab("Probability") +
    guides(
      col = guide_legend(order = 1),
      fill = guide_legend(order = 1)
    )

  g
}
