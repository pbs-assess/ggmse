#' Plot convergence
#'
#' This function plots cumulative performance metric values across iterations to
#' assess convergence.
#'
#' @param object_list A (named) list of DLMtool MSE objects.
#' @param pm_list A list or vector of performance metrics as character.
#' @param label_gap Controls the gap on the right of each panel to make space
#'   for the MP labels. This value is multiplied by the maximum number of
#'   iterations to expand the x axis.
# @param use_labels Use labels (`TRUE`) or use a colour legend (`FALSE`)?
#' @param custom_pal An optional custom color palette. Should be a named
#'   character vector
#' @param ylim Y limits. Defaults to the minimum observed performance metric
#'   value (via lazy evaluation) and 1.
#' @param satisficed An optional named numeric vector. The names correspond to
#'   the performance metrics and the values correspond to the threshold.
#'   This will add a horizontal line on the relevant panels.
#' @param french French?
#'
#' @return A ggplot2 plot.
#' @export
#'
#' @examples
#' plot_convergence(mse_example)
#' plot_convergence(mse_example, satisficed = c("LTY" = 0.9))
plot_convergence <- function(object_list, pm_list = c("LTY", "PNOF"),
                             label_gap = 1.15, custom_pal = NULL,
                             ylim = c(min(df$value), 1),
                             satisficed = NULL, french = FALSE) {

  if (!is.list(object_list)) {
    object_list <- list("Scenario 1" = object_list)
  }

  if (class(object_list[[1]]) != "MSE") {
    stop("`object` must be object of class 'MSE'", call. = FALSE)
  }
  if (object_list[[1]]@nMPs < 2) {
    stop("This function requires > 1 MP in the MSE object", call. = FALSE)
  }

  out <- purrr::map(object_list, ~{
    df <- purrr::map_df(pm_list, function(pm_f) {
      pmval <- eval(call(pm_f, .x))
      pmval@Prob[!is.finite(pmval@Prob)] <- 0
      cum_mean <- apply(pmval@Prob, 2, cumsum) /
        apply(pmval@Prob, 2, seq_along)
      vals <- as.vector(cum_mean)
      mp <- rep(.x@MPs, each = .x@nsim)
      data.frame(
        iter = seq_len(.x@nsim), value = vals,
        mp_name = mp, pm_name = pm_f, stringsAsFactors = FALSE
      )
    })
    df$pm_name <- factor(df$pm_name, levels = as.character(pm_list))
    df
  })

  df <- dplyr::bind_rows(out, .id = "scenario")

  # last_iter <- df[df$iter == max(df$iter), , drop = FALSE]
  g <- ggplot2::ggplot(df, aes_string("iter", "value", colour = "mp_name")) +
    ggplot2::geom_line() +
    theme_pbs() +
    ggplot2::labs(
      x = en2fr("Cumulative replicate", french, allow_missing = TRUE),
      y = en2fr("Performance metric probability", french, allow_missing = TRUE),
      color = en2fr("MP", french, allow_missing = TRUE)
    )
  if (length(object_list) > 1) {
    g <- g + ggplot2::facet_grid(pm_name ~ scenario)
  } else {
    g <- g + ggplot2::facet_wrap(~ pm_name)
  }

  if (!is.null(custom_pal)) {
    g <- g + scale_color_manual(values = custom_pal) +
      scale_fill_manual(values = custom_pal)
  }

  g <- g + ggplot2::scale_x_continuous(breaks = pretty(unique(df$iter))) +
    ggplot2::coord_cartesian(xlim = c(1, max(object_list[[1]]@nsim)), ylim = ylim, expand = FALSE)

  if (!is.null(satisficed)) {
    temp <- as.data.frame(satisficed)
    temp$pm_name <- row.names(temp)
    g <- g + geom_hline(data = temp, mapping = aes_string(yintercept = "satisficed"),
      lty = 2, alpha = 0.5)
  }

  g
}
