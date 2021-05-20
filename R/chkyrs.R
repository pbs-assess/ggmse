#' Check the years to summarize performance
#'
#' @param Yrs Numeric vector of length 2 with year indices to summarize performance.
#' If NULL, the performance is summarized over all projection years.
#' `Yrs` can also be length one, in which case if it is positive it is the first `Yrs` and
#' if negative the last `Yrs` of the projection period.
#' @param MSEobj An object of class `MSE`
#'
#' @return A numeric vector of length 2 with year indices to summarize performance
#' @examples
#' \dontrun{
#' MSE <- runMSE()
#' chk_yrs(NULL, MSE) # returns c(1, MSE@proyears)
#' chk_yrs(c(2,5), MSE) # returns c(2,5)
#' chk_yrs(c(70,80), MSE) # returns c(MSE@proyears-10,MSE@proyears)
#' chk_yrs(5, MSE) # returns c(1,5)
#' chk_yrs(-5, MSE) # returns c(46,50)
#' }
#'
#' @keywords internal
#' @export
chk_yrs <- function(Yrs, MSEobj) {
  if (class(MSEobj) !='MSE') stop('Require object of class MSE', call.=FALSE)
  if (is.null(Yrs)) {
    y.st <- 1
    y.end <- MSEobj@proyears
  } else {
    if (length(Yrs) == 1) {
      if (Yrs == 0) stop("Yrs must be postive or negative", call.=FALSE)
      if (Yrs < 0) {
        y.st <- MSEobj@proyears + Yrs[1] + 1
        y.end <- MSEobj@proyears
      } else {
        y.st <- 1
        y.end <- y.st + Yrs[1] - 1
      }
    } else {
      if (length(Yrs)>2) stop("Yrs must be numeric vector of length 1 or 2", call.=FALSE)

      y.st <- Yrs[1]
      y.end <- Yrs[2]

      if (Yrs[1] > Yrs[2]) stop("Yrs[1] is > Yrs[2]", call.=FALSE)
      if (any(Yrs < 1)) stop("Yrs must be positive", call.=FALSE)

      if (Yrs[2] > MSEobj@proyears) {
        message('Yrs[2] is greater than MSEobj@proyears. Setting Yrs[2] = MSEobj@proyears')
        y.end <- MSEobj@proyears
      }
      if (Yrs[1] > MSEobj@proyears) {
        message('Yrs[1] is greater than MSEobj@proyears. Setting Yrs[1] = Yrs[2] - Yrs[1]')
        y.st <- max(1,y.end - (Yrs[2] - Yrs[1]))
      }
    }
  }

  return(c(y.st, y.end))
}
