#' Create a spider plot
#'
#' @param mse_obj A DLMtool object of class [MSE-class]
#' @param pm_list A list of strings defining valid Performance metrics
#' @param palette A palette color as recognized by [ggplot2::scale_color_brewer()]
#'
#' @return A ggplot object
#' @importFrom ggspider
#' @export
#'
#' @examples
#' spider(mse)
spider <- function(mse_obj,
                   pm_list = list("LTY",
                                  "P10",
                                  "P40",
                                  "P80",
                                  "P100",
                                  "PNOF",
                                  "AAVY"),
                   palette = "Spectral"){
  x <- eval_pm(mse_obj, pm_list)
  ggspider::spider_web(x,
                       "mp",
                       "pm",
                       "prob",
                       palette = palette,
                       leg_main_title = "Management Procedure",
                       leg_lty_title = "MP type")
}
