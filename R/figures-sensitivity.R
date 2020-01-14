#' Sensitivity plot
#'
#' Plot sensitivity of performance metrics to OM slots across iterations
#'
#' @param object A DLMtool MSE object.
#' @param pm_function A DLMtool performance metric function. The y-axis of the
#'   figure will represent the mean `@Stat` value (within each iteration-MP
#'   combination, i.e., across time) from the object returned by this function.
#' @param mp A character vector of MPs to plot (must be included in the MSE
#'   object). Defaults to all.
#' @param slots A character vector of OM slots to plot. Will be plotted in this
#'   order. Set `slots = "all"` to plot all available OM and observation slots.
#' @param ylab The y-axis label.
#'
#' @return
#' A ggplot object
#' @export
#'
#' @examples
#' library(DLMtool)
#' om@nsim <- 10
#' x <- runMSE(om, MPs = c("AvC", "CC1"))
#' plot_sensitivity(x)
plot_sensitivity <- function(object, pm_function, mp = object@MPs,
                             slots = c("D", "hs", "M", "ageM", "L50", "Linf", "K", "Isd"),
                             ylab = "Performance metric value") {

  if (class(object) != "MSE")
    stop("`object` must be class 'MSE'", call. = FALSE)
  if (class(pm_function) != "PM")
    stop("`pm_function` must be a function of class 'PM'", call. = FALSE)
  if (any(!slots %in% union(colnames(object@OM), colnames(object@Obs))))
    stop("All `slots` must be valid `object@OM` or `object@Obs` slot names.", call. = FALSE)

  obs <- suppressMessages(reshape2::melt(object@Obs,
    variable.name = "om_slot", value.name = "om_value"
  )) %>%
    mutate(iter = rep(seq_len(dim(object@Obs)[1]), dim(object@Obs)[2])) %>%
    mutate(slot_type = "Obs")
  om <- suppressMessages(reshape2::melt(object@OM,
    id.vars = NULL, variable.name = "om_slot", value.name = "om_value"
  )) %>%
    mutate(iter = rep(seq_len(dim(object@OM)[1]), dim(object@OM)[2])) %>%
    mutate(slot_type = "OM")
  om <- suppressWarnings(bind_rows(obs, om))
  pm <- pm_function(object)
  xx <- apply(pm@Stat[, , ], c(1, 2), mean)
  xx <- as.data.frame(xx)
  row.names(xx) <- NULL
  colnames(xx) <- object@MPs
  pm <- suppressMessages(
    reshape2::melt(xx, variable.name = "mp", value.name = "pm_value")
  ) %>%
    mutate(iter = rep(seq_len(dim(xx)[1]), dim(xx)[2]))

  dat <- dplyr::inner_join(om, pm, by = "iter")

  if (slots[[1]] == "all" && length(slots) == 1L)
    slots <- union(colnames(object@OM), colnames(object@Obs))

  dplyr::filter(
    dat,
    mp %in% mp,
    om_slot %in% slots
  ) %>%
    ggplot2::ggplot(aes(om_value, pm_value)) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::facet_grid(mp ~ factor(om_slot, levels = slots), scales = "free_x") +
    ggplot2::geom_smooth(
      se = FALSE, col = "red",
      method = "loess", span = 0.75, formula = "y ~ x"
    ) +
    gfplot::theme_pbs() +
    ggplot2::labs(x = "Parameter value", y = ylab)
}
