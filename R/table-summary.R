
#' Summary of status estimates
#'
#' A wrapper function that returns biomass and F estimates for a set of conditioned operating models
#' to support reporting for the Fish Stocks Provisions.
#'
#' @param rcm A list containing RCModel objects.
#' @param scenario A character vector of names corresponding to rcm.
#' @param MPD Logical, whether to plot individual simulations (FALSE) or from the single fit (TRUE) in RCModel@mean_fit.
#' @param LRP The fraction of BMSY for the limit reference point
#' @param USR The fraction of BMSY for the upper stock reference
#' @param FUN Function for summarizing results across simulations for each scenario, e.g., median or mean. Set to NULL to
#' return values from all simulations.
#' @param ref A logical vector to identify reference operating models for the summary statistics
#' @export
#' @return A data frame with:
#' \itemize{
#' \item B0
#' \item R0
#' \item h
#' \item M
#' \item Bt (terminal year spawning biomass)
#' \item Ft (terminal year fishing mortality)
#' \item BMSY
#' \item FMSY
#' \item MSY
#' \item B_BMSY
#' \item p_LRP (probability above the LRP)
#' \item p_USR (probability above USR)
#' \item p_BMSY (probability above BMSY)
#' \item F_FMSY
#' \item p_FMSY (probability below FMSY)
#' }
#' @importFrom reshape2 melt dcast
FSP_summary <- function(rcm, scenario = paste("Scenario", 1:length(rcm)), MPD = FALSE,
                        LRP = 0.4, USR = 0.8, FUN = median, ref = rep(TRUE, length(rcm))) {
  dat <- Map(.FSP_summary, rcm = rcm, .scenario = scenario, ref = ref,
             MPD = MPD, LRP = LRP, USR = USR) %>%
    bind_rows()

  if(is.null(FUN)) {
    return(dat)
  }

  if (any(ref)) {
    dat_ref <- dat %>%
      filter(reference == TRUE) %>%
      mutate(scenario = "Reference")

    dat <- rbind(dat, dat_ref)
  }

  dat %>%
    dplyr::select(!reference) %>%
    reshape2::melt(id.vars = c("simulation", "scenario")) %>%
    group_by(scenario, variable) %>%
    summarise(value = FUN(value) %>% signif(4) %>% format()) %>%
    reshape2::dcast(list("variable", "scenario"))
}

.FSP_summary <- function(rcm, .scenario, ref = TRUE, MPD = FALSE, LRP = 0.4, USR = 0.8) {
  stopifnot(!MPD)
  if(MPD) {
    B0 <- rcm@mean_fit$report[["E0_SR"]]
    R0 <- rcm@mean_fit$report[["R0"]]

    h <- rcm@mean_fit$report[["h"]]

    M <- rcm@mean_fit$report[["Mest"]]
    if(is.null(M)) M <- rcm@mean_fit$obj$env$data$M_data[rcm@OM@nyears, 1]

    Bterminal <- rcm@mean_fit$report$E[rcm@OM@nyears]
    Fterminal <- rcm@mean_fit$report$F_at_age[rcm@OM@nyears, ]

  } else {
    B0 <- vapply(rcm@Misc, getElement, numeric(1), "E0_SR")
    R0 <- vapply(rcm@Misc, getElement, numeric(1), "R0")

    h <- vapply(rcm@Misc, getElement, numeric(1), "h")

    M <- vapply(rcm@Misc, function(x) ifelse(is.null(x$Mest), NA_real_, x$Mest), numeric(1))

    if(all(is.na(M))) M <- rcm@OM@cpars$M_ageArray[, 1, rcm@OM@nyears]

    Bterminal <- rcm@SSB[, rcm@OM@nyears]
    Fterminal <- vapply(rcm@Misc, function(x) max(x$F_at_age[rcm@OM@nyears, ]), numeric(1))
  }

  BMSY <- .rcm_calc_MSY(rcm, MPD = MPD, type = "SSBMSY")
  FMSY <- .rcm_calc_MSY(rcm, MPD = MPD, type = "FMSY")
  MSY <- .rcm_calc_MSY(rcm, MPD = MPD, type = "MSY")

  B_BMSY <- Bterminal/BMSY
  p_LRP <- mean(B_BMSY >= LRP)
  p_USR <- mean(B_BMSY >= USR)
  p_BMSY <- mean(B_BMSY >= 1)

  F_FMSY <- Fterminal/FMSY
  p_FMSY <- mean(F_FMSY <= 1)

  data.frame(B0 = B0,
             R0 = R0,
             h = h,
             M = M,
             Bt = Bterminal,
             Ft = Fterminal,
             BMSY = BMSY,
             FMSY = FMSY,
             MSY = MSY,
             B_BMSY = B_BMSY,
             p_LRP = p_LRP,
             p_USR = p_USR,
             p_BMSY = p_BMSY,
             F_FMSY = F_FMSY,
             p_FMSY = p_FMSY,
             simulation = 1:length(MSY),
             scenario = .scenario,
             reference = ref)
}

