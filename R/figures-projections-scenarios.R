#' Plot projections for various scenarios on the same panels
#'
#' @param object_list A named list of MSE objects from DLMtool. Names become
#'   scenario names.
#' @param probs The quantiles to plot as ribbons. This represents the tail
#'   probability.
#' @param rel_widths A numeric vector of length 2 the controls the relative
#'   width of the SSB and F columns (first element of the numeric vector) and
#'   the catch column (second element of the numeric vector). Depending on the
#'   number of years in the projection on the size of the axis labels, you may
#'   need to tweak the second element of this vector to make all 3 columns the
#'   same width. Figured it out by trial and error.
#' @param palette The name of an \pkg{RColorBrewer} colour palette.
#' @param catch_breaks Optional y-axis tick locations for the catch column.
#' @param catch_labels Optional y-axis tick labels for the catch column. Helpful
#'   for dealing with large numbers.
#' @param catch_ylim An optional numeric factor of length two that represents
#'   the y axis limits for the catch panels.
#' @param msy_ylim SSB and F column y limits.
#' @return A ggplot object
#' @importFrom ggplot2 scale_fill_brewer scale_colour_brewer
#' @export
#'
#' @examples
#' x <- list()
#' x[[1]] <- mse_example
#' x[[2]] <- mse_example
#' names(x) <- c("Scenario 1", "Scenario 2")
#' plot_scenario_projections(x)
#' plot_scenario_projections(mse_example)
plot_scenario_projections <- function(
                                      object_list,
                                      probs = 0.5,
                                      rel_widths = c(2, 1.2),
                                      palette = "Dark2",
                                      catch_breaks = NULL,
                                      catch_labels = NULL,
                                      catch_ylim = NULL,
                                      msy_ylim = c(0, 4.5)) {
  if (!is.list(object_list)) {
    object_list <- list(object_list)
    names(object_list) <- "Scenario"
  }

  if (is.null(object_list[[1]]@OM$CurrentYr[[1]])) {
    warning(
      "Missing `object@OM$CurrentYr`.\n",
      "Please run the MSE with a newer GitHub DLMtool version\n",
      "or set `object@OM$CurrentYr` yourself.\n",
      "Setting CurrentYr = 0 for now.",
      call. = FALSE
    )
    this_year <- 0
  } else {
    this_year <- object_list[[1]]@OM$CurrentYr[[1]]
  }
  probs <- c(probs, probs)

  bbmsy_ffmsy <- purrr::map_dfr(object_list, ~ {
    ts_data <- get_ts(object = .x, type = c("SSB", "FM"), this_year = this_year)
    get_ts_quantiles(ts_data, probs = probs)
  }, .id = "scenario")

  catch <- purrr::map_dfr(object_list, ~ {
    ts_data <- get_ts(object = .x, type = "C", this_year = this_year)
    get_ts_quantiles(ts_data, probs = probs)
  }, .id = "scenario")

  bbmsy_ffmsy$Type <- gsub("_", "/", bbmsy_ffmsy$Type)
  bbmsy_ffmsy$Type <- gsub("MSY", "[MSY]", bbmsy_ffmsy$Type)

  mp_names <- sort(unique(bbmsy_ffmsy$mp_name))
  ref_grep <- grepl("ref", mp_names)
  if (any(ref_grep)) { # move ref MPs to end
    mp_names <- c(mp_names[!ref_grep], mp_names[ref_grep])
  }
  bbmsy_ffmsy$mp_name <- factor(bbmsy_ffmsy$mp_name, levels = mp_names)
  catch$mp_name <- factor(catch$mp_name, levels = mp_names)

  g1 <- bbmsy_ffmsy %>%
    ggplot(aes_string("real_year", "m", colour = "scenario", fill = "scenario")) +
    geom_line(na.rm = TRUE) +
    facet_grid(mp_name ~ Type, labeller = ggplot2::label_parsed) +
    geom_ribbon(aes_string(x = "real_year", ymin = "l", ymax = "u"),
      colour = NA, alpha = 0.07, na.rm = TRUE
    ) +
    theme_pbs() +
    coord_cartesian(expand = FALSE, ylim = msy_ylim) +
    scale_colour_brewer(palette = palette) +
    scale_fill_brewer(palette = palette) + ylab("Value") + xlab("Year") +
    geom_vline(xintercept = this_year, lty = 2, alpha = 0.3) +
    ggplot2::theme(panel.spacing = grid::unit(-0.1, "lines"))

  g2 <- catch %>%
    ggplot(aes_string("real_year", "m", colour = "scenario", fill = "scenario")) +
    geom_line(na.rm = TRUE) +
    facet_grid(mp_name ~ Type) +
    geom_ribbon(aes_string(x = "real_year", ymin = "l", ymax = "u"),
      colour = NA, alpha = 0.07, na.rm = TRUE
    ) +
    theme_pbs() +
    scale_colour_brewer(palette = palette) +
    scale_fill_brewer(palette = palette) + ylab("Catch") + xlab("Year") +
    geom_vline(xintercept = this_year, lty = 2, alpha = 0.3) +
    ggplot2::theme(panel.spacing = grid::unit(-0.1, "lines")) +
    coord_cartesian(expand = FALSE)

  if (!is.null(catch_ylim)) {
    suppressMessages({
      g2 <- g2 + ggplot2::coord_cartesian(expand = FALSE, ylim = catch_ylim)
    })
  }

  if (!is.null(catch_breaks) && is.null(catch_labels)) {
    catch_labels <- catch_breaks
  }
  if (!is.null(catch_breaks)) {
    suppressMessages({
      g2 <- g2 +
        ggplot2::scale_y_continuous(breaks = catch_breaks, labels = catch_labels)
      g2 <- g2 + ggplot2::coord_cartesian(expand = FALSE, ylim = catch_ylim)
    })
  }

  g3 <- cowplot::plot_grid(
    g1 + theme(legend.position = "none"),
    g2 + theme(legend.position = "none"),
    rel_widths = rel_widths, align = "h"
  )

  legend <- cowplot::get_legend(
    # create some space for the legend
    g1 + theme(
      legend.box.margin = ggplot2::margin(0.2, 0.2, 12, .2),
      legend.position = "bottom"
    ) +
      labs(colour = "Scenario", fill = "Scenario")
  )

  cowplot::plot_grid(g3, legend, rel_heights = c(4, .2), nrow = 2)
}
