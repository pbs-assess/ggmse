#' Plot convergence
#'
#' This function plots cumulative performance metric values across iterations to
#' assess convergence.
#'
#' @param object A DLMtool MSE object.
#' @param pm_list A list or vector of performance metrics as character.
#' @param label_gap Controls the gap on the right of each panel to make space
#'   for the MP labels. This value is multiplied by the maximum number of
#'   iterations to expand the x axis.
#' @param use_labels Use labels (`TRUE`) or use a colour legend (`FALSE`)?
#' @param ylim Y limits.
#'
#' @return A ggplot2 plot.
#' @export
#'
#' @examples
#' plot_convergence(mse_example)
plot_convergence <- function(object, pm_list = c("LTY", "PNOF"),
                             label_gap = 1.15, use_labels = FALSE,
                             ylim = c(NA, 1)) {
  if (class(object) != "MSE") {
    stop("`object` must be object of class 'MSE'", call. = FALSE)
  }
  if (object@nMPs < 2) {
    stop("This function requires > 1 MP in the MSE object", call. = FALSE)
  }

  df <- purrr::map_df(pm_list, function(pm_f) {
    pmval <- eval(call(pm_f, object))
    pmval@Prob[!is.finite(pmval@Prob)] <- 0
    cum_mean <- apply(pmval@Prob, 2, cumsum) /
      apply(pmval@Prob, 2, seq_along)
    vals <- as.vector(cum_mean)
    mp <- rep(object@MPs, each = object@nsim)
    data.frame(
      iter = seq_len(object@nsim), value = vals,
      mp_name = mp, pm_name = pm_f, stringsAsFactors = FALSE
    )
  })
  df$pm_name <- factor(df$pm_name, levels = as.character(pm_list))

  last_iter <- df[df$iter == max(df$iter), , drop = FALSE]
  g <- ggplot2::ggplot(df, aes_string("iter", "value", colour = "mp_name")) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(pm_name)) +
    gfplot::theme_pbs() +
    ggplot2::labs(
      x = "Cumulative iteration", y = "Performance metric value",
      color = "MP"
    )

  if (use_labels) {
    g <- g + ggrepel::geom_text_repel(
      data = last_iter,
      nudge_x = 0.2,
      mapping = aes_string(label = "mp_name"),
      direction = "y",
      hjust = 0,
      segment.size = 0.2
    ) +
      ggplot2::guides(colour = FALSE) +
      ggplot2::scale_x_continuous(
        breaks = pretty(unique(df$iter)),
        limits = c(1, max(df$iter) * label_gap)
      ) +
      ggplot2::coord_cartesian(xlim = c(1, max(df$iter) * label_gap), expand = FALSE, ylim = ylim) +
      ggplot2::geom_vline(xintercept = max(df$iter), lty = 1, col = "grey65")
  } else {
    g <- g + ggplot2::scale_x_continuous(breaks = pretty(unique(df$iter))) +
      ggplot2::coord_cartesian(xlim = c(1, max(object@nsim)), ylim = ylim, expand = FALSE)
  }
  g
}
