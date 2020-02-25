#' Sample additional index error
#'
#' This function adds log-normally distributed error to `OM@cpars$AddIerr`. It
#' also sets the survey hyperdepletion/hyperstability parameter to a specified
#' value (default 1).
#'
#' This is useful if you want to override the default of estimating
#' hyperdepletion/hyperstability, observation error, and autocorrelation in the
#' additional "real" indices as is done in DLMtool.
#'
#' @param om A DLMtool operating model that has been run through
#'   [MSEtool::SRA_scope()].
#' @param beta The hyperdepletion/hyperstability parameter. 1 indicates none.
#'
#' @return A DLMtool operating model.
#' @export
#'
#' @examples
#' library(MSEtool)
#' om <- DLMtool::testOM
#' om@nsim <- 5
#' set.seed(1)
#' om <- SRA_scope(om,
#'   data = list(Chist = runif(10), Index = runif(10), I_sd = rep(0.1, 10))
#' )
#' om2 <- sample_AddIerr(om@OM)
#' matplot(t(om2@cpars$AddIerr[,1,]), type = "l", lty = 1)

sample_AddIerr <- function(om, beta = 1) {
  nsim_cpars <- length(om@cpars$R0)
  n.ind <- dim(om@cpars$Data@AddInd)[2]
  om@cpars$AddIbeta <- matrix(beta, nrow = nsim_cpars, ncol = n.ind)
  set.seed(om@seed)
  .cv <- stats::runif(nsim_cpars, min = min(om@Iobs), max = max(om@Iobs))
  .n <- om@nyears + om@proyears
  om@cpars$AddIerr <- array(NA, dim = c(nsim_cpars, n.ind, .n))
  set.seed(om@seed)
  for (i in seq_len(n.ind)) {
    for (j in seq_len(nsim_cpars)) {
      om@cpars$AddIerr[j,i,] <- DLMtool::trlnorm(.n, mu = 1, cv = .cv[j])
    }
  }
  om
}