#' Create a spider plot
#'
#' @param mse_obj A DLMtool object of class [MSE-class]
#' @param pm_list A list of strings defining valid Performance metrics
#' @param palette A palette color as recognized by [ggplot2::scale_color_brewer()]
#' @param ... Other arguments to pass to [ggspider::spider_web()].
#'
#' @return A ggplot object
#' @importFrom ggspider spider_web
#' @export
#'
#' @examples
#' spider(mse_example)
spider <- function(mse_obj,
                   pm_list = list(
                     "LTY",
                     "P10",
                     "P40",
                     "P80",
                     "P100",
                     "PNOF",
                     "AAVY"
                   ),
                   palette = "Set2", ...) {
  x <- eval_pm(mse_obj, pm_list)

  browser()
  ggspider::spider_web(x,
    "mp",
    "pm",
    "prob",
    palette = palette,
    leg_main_title = "MP",
    leg_lty_title = "MP type",
    ...
  ) + ggplot2::labs(color = "MP")
}
