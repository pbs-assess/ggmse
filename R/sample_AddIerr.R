#' Sample additional index error
#'
#' This function adds log-normally distributed error to `OM@cpars$AddIerr`. It
#' also sets the survey hyperdepletion/hyperstability parameter based on
#' `OM@beta`. This is useful if you want to override the default of estimating
#' hyperdepletion/hyperstability, observation error, and autocorrelation in the
#' additional "real" indices as is done in MSEtool.
#'
#' @param om A MSEtool operating model that has been run through
#'   [SAMtool::RCM()].
#'
#' @return A MSEtool operating model.
#' @export
#'
#' @examples
#' library(SAMtool)
#' om <- MSEtool::testOM
#' om@@nsim <- 5
#' set.seed(1)
#' om <- RCM(om,
#'   data = list(Chist = runif(10), Index = runif(10), I_sd = rep(0.1, 10))
#' )
#' om2 <- sample_AddIerr(om@OM)
#' matplot(t(om2@cpars$AddIerr[,1,]), type = "l", lty = 1)

sample_AddIerr <- function(om) {
  nsim_cpars <- length(om@cpars$R0)
  n.ind <- dim(om@cpars$Data@AddInd)[2]

  set.seed(om@seed)
  if (!"beta" %in% names(om@cpars)) {
    om@cpars$AddIbeta <- matrix(stats::runif(nsim_cpars * n.ind,
      om@beta[1], om@beta[2]), nrow = nsim_cpars, ncol = n.ind)
  } else {
    om@cpars$AddIbeta <- matrix(rep(om@cpars$beta, n.ind), nrow = nsim_cpars, ncol = n.ind)
  }
  if (!"Iobs" %in% names(om@cpars)) {
    .sd <- stats::runif(nsim_cpars, min = min(om@Iobs), max = max(om@Iobs))
  } else {
    .sd <- om@cpars$Iobs
  }
  .n <- om@nyears + om@proyears
  om@cpars$AddIerr <- array(NA, dim = c(nsim_cpars, n.ind, .n))
  set.seed(om@seed)
  for (i in seq_len(n.ind)) {
    for (j in seq_len(nsim_cpars)) {
        om@cpars$AddIerr[j,i,] <- exp(stats::rnorm(.n, mean = -0.5 * .sd[j]^2, sd = .sd[j]))
    }
  }
  om
}