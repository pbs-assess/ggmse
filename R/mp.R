#' Eliminate survey data for selected years
#'
#' This function factory creates a \pkg{DLMtool} management procedure (MP)
#' function that only sees survey data from selected years. For example, this
#' could change the data that an MP sees from an annual survey into a biennial
#' or triennial survey. It could also be used to eliminate commercial
#' observations from some years if the default slots affected were changed.
#'
#' @param mp An existing management procedure function of class `"MP"` that will
#'   work with \pkg{DLMtool}.
#' @param slots The slots for which you want to reduce observations.
#' @param index A function that takes the number of years of observations and
#'   returns a vector indexing the years that should be turned into `NA` values.
#'   The default anonymous function discards observations for odd years.
#'
#' @return A management procedure function of class `"MP"` for use with
#'   \pkg{DLMtool}.
#' @export
#' @importFrom methods slot slot<-
#'
#' @examples
#' library(DLMtool)
#' om <- DLMtool::testOM
#' om@nsim <- 3
#' temp_mp <- reduce_survey(Islope1)
#' mse <- runMSE(OM = om, MPs = "temp_mp")
reduce_survey <- function(mp, slots = c("Ind", "CAA", "CAL", "ML"),
                          index = function(x) seq(1, x, by = 2)) {
  f <- function(x, Data, reps = 100, ...) {
    for (slot_i in slots) {
      Data <- remove_years(Data, slot_i, index = index)
    }
    mp(x = x, Data = Data, reps = reps, ...)
  }
  `class<-`(f, "MP")
}

remove_years <- function(dat, slot, index) {
  .ncol <- ncol(slot(dat, slot))
  .dims <- length(dim(slot(dat, slot)))
  if (.dims == 1L) {
    slot(dat, slot)[index(.ncol)] <- NA
  } else if (.dims == 2L) {
    slot(dat, slot)[, index(.ncol)] <- NA
  } else if (.dims == 3L) {
    slot(dat, slot)[, index(.ncol), ] <- NA
  } else {
    stop("The dimensions of the data slot must be 1, 2, or 3.")
  }
  dat
}

#' Alternative versions of \pkg{DLMtool} and \pkg{MSEtool} MP functions
#'
#' Management procedures (MPs) with a `.` in front of their name use
#' [reduce_survey()] to eliminate odd years of survey observations to reflect
#' the biennial nature of most groundfish surveys in British Columbia.
#'
#' @param x A position in the data object. As per \pkg{DLMtool}.
#' @param Data A data object. As per \pkg{DLMtool}.
#' @param reps The number of stochastic samples of the MP recommendation(s). As
#'   per \pkg{DLMtool}.
#' @param ... Other arguments to pass to the MP function.
#'
#' @rdname MPs
#' @export
CC_hist20 <- function(x, Data, reps = 100, ...) {
  DLMtool::CC1(x, Data, reps, yrsmth = 20, xx = 0, ...)
}
class(CC_hist20) <- "MP"

#' @rdname MPs
#' @export
CC_hist <- DLMtool::AvC

#' @rdname MPs
#' @export
.DDSS_MSY <- reduce_survey(MSEtool::DDSS_MSY)

#' @rdname MPs
#' @export
.DDSS_4010 <- reduce_survey(MSEtool::DDSS_4010)

#' @rdname MPs
#' @export
GB_slope6_0.66 <- function(x, Data, reps = 100, ...) {
  DLMtool::GB_slope(x, Data, reps, yrsmth = 6, lambda = 0.66, ...)
}
class(GB_slope6_0.66) <- "MP"

#' @rdname MPs
#' @export
.GB_slope6_0.66 <- reduce_survey(GB_slope6_0.66)

#' @rdname MPs
#' @export
GB_slope6_1 <- function(x, Data, reps = 100, ...) {
  DLMtool::GB_slope(x, Data, reps, yrsmth = 6, lambda = 1, ...)
}
class(GB_slope6_1) <- "MP"

#' @rdname MPs
#' @export
.GB_slope6_1 <- reduce_survey(GB_slope6_1)

#' @rdname MPs
#' @export
GB_slope8_0.66 <- function(x, Data, reps = 100, ...) {
  DLMtool::GB_slope(x, Data, reps, yrsmth = 8, lambda = 0.66, ...)
}
class(GB_slope8_0.66) <- "MP"

#' @rdname MPs
#' @export
.GB_slope8_0.66 <- reduce_survey(GB_slope8_0.66)

#' @rdname MPs
#' @export
GB_slope8_1 <- function(x, Data, reps = 100, ...) {
  DLMtool::GB_slope(x, Data, reps, yrsmth = 8, lambda = 1, ...)
}
GB_slope8_1

#' @rdname MPs
#' @export
.GB_slope8_1 <- reduce_survey(GB_slope8_1)

#' @rdname MPs
#' @export
.ICI <- reduce_survey(DLMtool::ICI)

#' @rdname MPs
#' @export
.ICI2 <- reduce_survey(DLMtool::ICI2)

#' @rdname MPs
#' @export
Iratio6 <- function(x, Data, reps = 100, ...) {
  DLMtool::Iratio(x, Data, reps, yrs = c(3, 6), ...)
}
class(Iratio6) <- "MP"

#' @rdname MPs
#' @export
.Iratio6 <- reduce_survey(Iratio6)

#' @rdname MPs
#' @export
Iratio8 <- function(x, Data, reps = 100, ...) {
  DLMtool::Iratio(x, Data, reps, yrs = c(4, 8), ...)
}
class(Iratio8) <- "MP"

#' @rdname MPs
#' @export
.Iratio8 <- reduce_survey(Iratio8)

Islope_mod_ <- function(x, Data, reps = 100, yrsmth = 6, lambda, xx,
  increase_cap = 1.2, ...) {
  tac <- DLMtool::Islope_(x, Data, reps, yrsmth = yrsmth,
    lambda = lambda, xx = xx, ...
  )$TAC
  last_catch_rec <- Data@MPrec[x]
  tac[tac > (increase_cap * last_catch_rec)] <- increase_cap * last_catch_rec
  tac <- DLMtool::TACfilter(tac)
  Rec <- new("Rec")
  Rec@TAC <- tac
  Rec
}

#' @rdname MPs
#' @export
Islope0.4_100 <- function(x, Data, reps = 100, ...) {
  Islope_mod_(x, Data, reps = reps, lambda = 0.4, xx = 0, ...)
}
class(Islope0.4_100) <- "MP"

#' @rdname MPs
#' @export
.Islope0.4_100 <- reduce_survey(Islope0.4_100)

#' @rdname MPs
#' @export
Islope0.4_80 <- function(x, Data, reps = 100, ...) {
  Islope_mod_(x, Data, reps = reps, lambda = 0.4, xx = 0.2, ...)
}
class(Islope0.4_80) <- "MP"

#' @rdname MPs
#' @export
.Islope0.4_80 <- reduce_survey(Islope0.4_80)

#' @rdname MPs
#' @export
Islope0.2_100 <- function(x, Data, reps = 100, ...) {
  Islope_mod_(x, Data, reps = reps, lambda = 0.2, xx = 0, ...)
}
class(Islope0.2_100) <- "MP"

#' @rdname MPs
#' @export
.Islope0.2_100 <- reduce_survey(Islope0.2_100)

#' @rdname MPs
#' @export
Islope0.2_80 <- function(x, Data, reps = 100, ...) {
  Islope_mod_(x, Data, reps = reps, lambda = 0.2, xx = 0.2, ...)
}
class(Islope0.2_80) <- "MP"

#' @rdname MPs
#' @export
.Islope0.2_80 <- reduce_survey(Islope0.2_80)

#' @rdname MPs
#' @export
.Islope1 <- reduce_survey(DLMtool::Islope1)

#' @rdname MPs
#' @export
.Islope2 <- reduce_survey(DLMtool::Islope2)

#' @rdname MPs
#' @export
.Islope3 <- reduce_survey(DLMtool::Islope3)

#' @rdname MPs
#' @export
.Islope4 <- reduce_survey(DLMtool::Islope4)

#' @rdname MPs
#' @export
.IT5 <- reduce_survey(DLMtool::IT5)

#' @rdname MPs
#' @export
.IT10 <- reduce_survey(DLMtool::IT10)

#' @rdname MPs
#' @export
.Itarget1 <- reduce_survey(DLMtool::Itarget1)

#' @rdname MPs
#' @export
.Itarget2 <- reduce_survey(DLMtool::Itarget2)

#' @rdname MPs
#' @export
.Itarget3 <- reduce_survey(DLMtool::Itarget3)

#' @rdname MPs
#' @export
.Itarget4 <- reduce_survey(DLMtool::Itarget4)

#' @rdname MPs
#' @export
.ITM <- reduce_survey(DLMtool::ITM)

#' Historical Index Target based on natural mortality rate
#'
#' This MP is based on [DLMtool::ITM()] but since the reference index level to
#' the index over some historical time period.
#'
#' @param x A position in the data object. As per \pkg{DLMtool}.
#' @param Data A data object. As per \pkg{DLMtool}.
#' @param reps The number of stochastic samples of the MP recommendation(s). As
#'   per \pkg{DLMtool}.
#' @param yrsmth_hist Number of historical years to consider for reference index
#'   level.
#' @param ... Other arguments to pass to the MP function.
#'
#' @export
ITM_hist <- function(x, Data, reps = 100, yrsmth_hist = 10, ...) {
  mc <- (5 + Data@Mort[x] * 25) / 100
  if (mc > 0.2) mc <- 0.2
  yrsmth <- floor(4 * (1 / Data@Mort[x])^(1 / 4))
  ind <- max(1, (length(Data@Year) - yrsmth + 1)):length(Data@Year)

  # get mean index over last 10 historical years:
  yrlast <- match(Data@LHYear[1], Data@Year)
  yrfirst <- yrlast - yrsmth_hist + 1
  I_ref <- Data@Ind[x, seq(yrfirst, yrlast)]
  I_ref <- mean(I_ref, na.rm = TRUE)

  deltaI <- mean(Data@Ind[x, ind], na.rm = TRUE) / I_ref
  if (deltaI < (1 - mc)) {
    deltaI <- 1 - mc
  }
  if (deltaI > (1 + mc)) {
    deltaI <- 1 + mc
  }
  TAC <- Data@MPrec[x] * deltaI * DLMtool::trlnorm(reps, 1, Data@CV_Ind[ x, 1])
  TAC <- DLMtool::TACfilter(TAC)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}
class(ITM_hist) <- "MP"

#' @rdname MPs
#' @export
.ITM_hist<- reduce_survey(ITM_hist)

# #' @rdname MPs
# #' @export
# .LstepCC1 <- reduce_survey(DLMtool::LstepCC1)
#
# #' @rdname MPs
# #' @export
# .LstepCC2 <- reduce_survey(DLMtool::LstepCC2)
#
# #' @rdname MPs
# #' @export
# .LstepCC3 <- reduce_survey(DLMtool::LstepCC3)
#
# #' @rdname MPs
# #' @export
# .LstepCC4 <- reduce_survey(DLMtool::LstepCC4)
#
# #' @rdname MPs
# #' @export
# .Ltarget1 <- reduce_survey(DLMtool::Ltarget1)
#
# #' @rdname MPs
# #' @export
# .Ltarget2 <- reduce_survey(DLMtool::Ltarget2)
#
# #' @rdname MPs
# #' @export
# .Ltarget3 <- reduce_survey(DLMtool::Ltarget3)
#
# #' @rdname MPs
# #' @export
# .Ltarget4 <- reduce_survey(DLMtool::Ltarget4)
#
# #' @rdname MPs
# #' @export
# .Ltarget95 <- reduce_survey(DLMtool::Ltarget95)

#' SBT simple MP in log space
#'
#' This MP is based on [DLMtool::SBT1()] but fits the linear regression in log space. SBT stands for "southern bluefin tuna".
#'
#' @param x A position in the data object. As per \pkg{DLMtool}.
#' @param Data A data object. As per \pkg{DLMtool}.
#' @param reps The number of stochastic samples of the MP recommendation(s). As
#'   per \pkg{DLMtool}.
#' @param plot Logical.
#' @param yrsmth The number of years for evaluating trend in relative abundance indices.
#' @param k1 Control parameter.
#' @param k2 Control parameter.
#' @param gamma Control parameter.
#'
#' @export
SBT1_log <- function(x, Data, reps = 100, plot = FALSE, yrsmth = 10, k1 = 1.5,
                 k2 = 3, gamma = 1) {
  dependencies <- "Data@Cat, Data@Year, Data@Ind"
  Cr <- length(Data@Cat[x, ])
  cct <- trlnorm(reps, Data@Cat[x, Cr], Data@CV_Cat)
  ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
  I_hist <- Data@Ind[x, ind]
  test <- summary(lm(log(I_hist) ~ ind))$coefficients[2, 1:2]
  if (reps > 1) {
    lambda <- rnorm(reps, test[1], test[2])
  }
  else {
    lambda <- test[1]
  }
  TAC <- cct * (1 + k2 * lambda)
  cond <- lambda < 0
  TAC[cond] <- cct[cond] * (1 - k1 * -lambda[cond]^gamma)
  TAC <- TACfilter(TAC)
  if (plot) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mfrow = c(1, 2))
    plot(Data@Year[ind], log(I_hist),
      bty = "l", xlab = "Year",
      ylab = "log(Index)", type = "l", lwd = 2
    )
    lines(Data@Year[ind], predict(lm(log(I_hist) ~ ind)), lty = 2)
    ylim <- range(c(Data@Cat[x, ind], TAC))
    plot(c(Data@Year[ind], max(Data@Year[ind]) + 1), c(Data@Cat[x, ind], NA),
      bty = "l", xlab = "Year", ylab = paste0("Catch (", Data@Units, ")"),
      type = "l", lwd = 2, ylim = ylim
    )
    points(max(Data@Year[ind]), Data@Cat[x, max(ind)], pch = 16, cex = 1.5)
    boxplot(TAC, at = max(Data@Year[ind]) + 1, add = TRUE, axes = FALSE, col = "blue")
  }
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

#' @rdname MPs
#' @export
.SBT1_log <- reduce_survey(SBT1_log)

stepwise_NAs <- function(x) {
  df <- data.frame(x = x)
  out <- tidyr::fill(df, x, .direction = "down")
  out$x
}

#' Index-based MP from Cox et al. (2019)
#'
#' Index-based MP used in the most recent outside Yelloweye Rockfish assessment
#' in BC.
#'
#' @param x A position in the data object.
#' @param Data A data object.
#' @param reps The number of stochastic samples of the MP recommendation(s).
#' @param delta_min Most negative drop proportion allowed in the index.
#' @param delta_max Most positive increased proportion allowed in the index.
#' @param lambda Smoothing parameter. 0 means always use the last TAC. 1 means
#'   no smoothing. Can take any value in between.
#' @param tac_floor TAC when `delta_min` is met or exceeded.
#'
#' @export
#' @references
#' Sean P Cox, Beau Doherty, Ashleen J Benson, Samuel DN Johnson, and Dana
#' Haggarty. Evaluation of potential rebuilding strategies for Outside Yelloweye
#' Rockfish in British Columbia. Working Paper 2017GRF02.
#'
#' @examples
#' IDX(1, DLMtool::SimulatedData)
#' IDX(1, DLMtool::SimulatedData, lambda = 0.5)
IDX <- function(x, Data, reps = 100, delta_min = -0.5,
                delta_max = 0.25, lambda = 1, tac_floor = 0) {
  dependencies <- "Data@Ind"

  if (lambda < 0) lambda <- 0
  if (lambda > 1) lambda <- 1
  if (tac_floor < 0) tac_floor <- 0

  this_year <- length(Data@Year)

  # Stepwise fill in NAs with last available value:
  temp_Ind <- stepwise_NAs(Data@Ind[x,,drop=TRUE])
  delta_ind_y <- temp_Ind[this_year] / temp_Ind[this_year - 1] - 1
  catch_rec <- Data@MPrec[x]

  if (delta_ind_y <= delta_min) {
    TAC <- tac_floor
  }
  if (delta_min < delta_ind_y && delta_ind_y <= delta_max) {
    TAC <- (1 + delta_ind_y) * catch_rec
  }
  if (delta_ind_y > delta_max) {
    TAC <- (1 + delta_max) * catch_rec
  }

  TAC <- lambda * TAC + (1 - lambda) * catch_rec
  TAC <- DLMtool::TACfilter(TAC)

  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}
class(IDX) <- "MP"

#' @rdname MPs
#' @export
.IDX <- reduce_survey(IDX)

#' @rdname MPs
#' @export
IDX_smooth <- function(x, Data, reps = 100, ...) {
  IDX(x, Data, reps, lambda = 0.5, ...)
}
class(IDX_smooth) <- "MP"

#' @rdname MPs
#' @export
.IDX_smooth <- reduce_survey(IDX_smooth)

#' @rdname MPs
#' @export
SP4010_prior <- function (x, Data, reps = 1) {
  do_Assessment <- SP(x = x, Data = Data, use_r_prior = TRUE)
  Rec <- HCR_ramp(Assessment = do_Assessment, reps = reps,
    LRP = 0.1, TRP = 0.4, RP_type = "SSB_SSB0")
  Rec
}
class(SP4010_prior) <- "MP"

#' @rdname MPs
#' @export
SP4010 <- MSEtool::make_MP("SP", "HCR_ramp", LRP = 0.1, TRP = 0.4, RP_type = "SSB_SSB0")

#' @rdname MPs
#' @export
.SP4010 <- reduce_survey(SP4010)

#' @rdname MPs
#' @export
SP8040 <- MSEtool::make_MP("SP", "HCR_ramp", LRP = 0.4, TRP = 0.8, RP_type = "SSB_SSBMSY")

#' @rdname MPs
#' @export
.SP8040 <- reduce_survey(SP8040)

#' @rdname MPs
#' @export
SP8040_Fox <- MSEtool::make_MP("SP_Fox", "HCR_ramp", LRP = 0.4, TRP = 0.8, RP_type = "SSB_SSBMSY")

#' @rdname MPs
#' @export
.SP8040_Fox <- reduce_survey(SP8040_Fox)

#' @rdname MPs
#' @export
SP6040 <- MSEtool::make_MP("SP", "HCR_ramp", LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY")

#' @rdname MPs
#' @export
.SP6040 <- reduce_survey(SP6040)

#' @rdname MPs
#' @export
SP6040_Fox <- MSEtool::make_MP("SP_Fox", "HCR_ramp", LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY")

#' @rdname MPs
#' @export
.SP6040_Fox <- reduce_survey(SP6040_Fox)

#' @rdname MPs
#' @export
.SP_MSY <- reduce_survey(MSEtool::SP_MSY)

#' @rdname MPs
#' @export
SP_MSY_Fox <- MSEtool::make_MP("SP_Fox", "HCR_MSY")

#' @rdname MPs
#' @export
.SP_MSY_Fox <- reduce_survey(SP_MSY_Fox)

#' @rdname MPs
#' @export
.SCA4010 <- reduce_survey(MSEtool::SCA_4010)

#' @rdname MPs
#' @export
SCA6040 <- MSEtool::make_MP("SCA", "HCR_ramp", LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY")

#' @rdname MPs
#' @export
.SCA6040 <- reduce_survey(SCA6040)

IT_hist_ <- function(x, Data, reps = 100, yrsmth = 5, mc = 0.05, yrsmth_hist = 10) {
  # Based on DLMtool::IT_
  dependencies <- "Data@Ind, Data@MPrec, Data@CV_Ind"
  ind <- max(1, (length(Data@Year) - yrsmth + 1)):length(Data@Year)
  # get mean index over last 10 historical years:
  yrlast <- match(Data@LHYear[1], Data@Year)
  yrfirst <- yrlast - yrsmth_hist + 1
  I_ref <- Data@Ind[x, seq(yrfirst, yrlast)]
  I_ref <- mean(I_ref, na.rm = TRUE)
  # FIXME: could incorporate Ind CV here with trlnorm:
  deltaI <- mean(Data@Ind[x, ind], na.rm = TRUE) / I_ref
  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc
  TAC <- Data@MPrec[x] * deltaI * DLMtool::trlnorm(reps, 1, Data@CV_Ind[x, 1])
  TAC <- DLMtool::TACfilter(TAC)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

#' @rdname MPs
#' @export
IT5_hist <- function(x, Data, reps = 100, ...) {
  IT_hist_(x, Data, reps, yrsmth = 5, mc = 0.05, yrsmth_hist = 10)
}
class(IT5_hist) <- "MP"

#' @rdname MPs
#' @export
.IT5_hist <- reduce_survey(IT5_hist)

#' @rdname MPs
#' @export
IT10_hist <- function(x, Data, reps = 100, ...) {
  IT_hist_(x, Data, reps, yrsmth = 5, mc = 0.10, yrsmth_hist = 10)
}
class(IT10_hist) <- "MP"

#' @rdname MPs
#' @export
.IT10_hist <- reduce_survey(IT10_hist)

#' @rdname MPs
#' @export
CC100 <- DLMtool::CC1

#' @rdname MPs
#' @export
CC90 <- DLMtool::CC2

#' @rdname MPs
#' @export
CC80 <- DLMtool::CC3

#' @rdname MPs
#' @export
CC70 <- DLMtool::CC4

#' @rdname MPs
#' @export
CC60 <- DLMtool::CC5

#' PBS groundfish surplus production wrapper function
#'
#' @param x Index
#' @param Data Data
#' @param reps Reps
#' @param LRP Lower reference point
#' @param TRP Target reference point
#' @param RP_type Reference point type
#' @param start Starting list for the model
#' @param use_r_prior Logical but whether to use a prior
#' @param tac_max_increase Maximum proportional increase in TAC
#' @param tac_max_decrease Maximum proportional decrease in TAC
#' @param tac_floor Floor for TAC
#' @param tac_increase_buffer Proportional buffer below which TAC won't change
#'
#' @export
SP_gf <- function (x, Data, reps = 1, LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY",
  start = list(r_prior = c(0.3, 0.1)), use_r_prior = TRUE, tac_max_increase = 1.2, tac_max_decrease = 0.5, tac_floor = 0.1, tac_increase_buffer = 1.05) {
  dependencies <- "Data@Cat, Data@Ind"
  do_Assessment <- SP(x = x, Data = Data,
    control = list(iter.max = 10000, eval.max = 20000), n_seas = 1,
    use_r_prior = use_r_prior, start = start)
  Rec <- HCR_ramp(Assessment = do_Assessment, reps = reps, LRP = LRP,
    TRP = TRP, RP_type = RP_type)
  if (!is.na(Rec@TAC)) {
    if (as.list(do_Assessment@SD, "Std. Error")$log_FMSY > 1) {
      warning("Std. Error too large.")
      Rec@TAC <- Data@MPrec[x]
    }
    if (Rec@TAC > Data@MPrec[x] && Rec@TAC < tac_increase_buffer * Data@MPrec[x]) {
      Rec@TAC <- Data@MPrec[x]
    }
    if (Rec@TAC > tac_max_increase * Data@MPrec[x]) {
      warning("TAC > 1.2 last TAC.")
      Rec@TAC <- tac_max_increase * Data@MPrec[x]
    }
    if (Rec@TAC < tac_max_decrease * Data@MPrec[x]) {
      warning("TAC < 0.5 last TAC.")
      Rec@TAC <- tac_max_decrease * Data@MPrec[x]
    }
    if (Rec@TAC < tac_floor * Data@Cat[x,Data@LHYear]) {
      warning("TAC < 0.1 last historical catch.")
      Rec@TAC <- tac_floor * Data@Cat[x,Data@LHYear]
    }
  }
  Rec@Misc <- MSEtool:::Assess_diagnostic(x, Data, do_Assessment, include_assessment = FALSE)
  return(Rec)
}

#' @rdname MPs
#' @export
SP6040_prior <- function(x, Data, reps = 1) {
  SP_gf(x, Data, reps = 1, LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY")
}
class(SP6040_prior) <- "MP"

#' @rdname MPs
#' @export
.SP6040_prior <- reduce_survey(SP6040_prior)

#' @rdname MPs
#' @export
SP8040_prior <- function(x, Data, reps = 1) {
  SP_gf(x, Data, reps = 1, LRP = 0.4, TRP = 0.8, RP_type = "SSB_SSBMSY")
}
class(SP8040_prior) <- "MP"

#' @rdname MPs
#' @export
.SP8040_prior <- reduce_survey(SP8040_prior)

#' @rdname MPs
#' @export
SP4010_prior <- function(x, Data, reps = 1) {
  SP_gf(x, Data, reps = 1, LRP = 0.1, TRP = 0.4, RP_type = "SSB_SSB0")
}
class(SP4010_prior) <- "MP"

#' @rdname MPs
#' @export
.SP4010_prior <- reduce_survey(SP4010_prior)
