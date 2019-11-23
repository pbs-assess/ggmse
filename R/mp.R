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

# #' @rdname MPs
# #' @export
# .DD <- reduce_survey(DLMtool::DD)
#
# #' @rdname MPs
# #' @export
# .DD4010 <- reduce_survey(DLMtool::DD4010)

AvC20 <- function(x, Data, reps = 100, ...) {
  DLMtool::CC1(x, Data, reps, yrsmth = 20, xx = 0, ...)
}

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

#' @rdname MPs
#' @export
.GB_slope6_0.66 <- reduce_survey(GB_slope6_0.66)

#' @rdname MPs
#' @export
GB_slope6_1 <- function(x, Data, reps = 100, ...) {
  DLMtool::GB_slope(x, Data, reps, yrsmth = 6, lambda = 1, ...)
}
#' @rdname MPs
#' @export
.GB_slope6_1 <- reduce_survey(GB_slope6_1)

#' @rdname MPs
#' @export
GB_slope8_0.66 <- function(x, Data, reps = 100, ...) {
  DLMtool::GB_slope(x, Data, reps, yrsmth = 8, lambda = 0.66, ...)
}

#' @rdname MPs
#' @export
.GB_slope8_0.66 <- reduce_survey(GB_slope8_0.66)

#' @rdname MPs
#' @export
GB_slope8_1 <- function(x, Data, reps = 100, ...) {
  DLMtool::GB_slope(x, Data, reps, yrsmth = 8, lambda = 1, ...)
}
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
#' @rdname MPs
#' @export
.Iratio6 <- reduce_survey(Iratio6)

#' @rdname MPs
#' @export
Iratio8 <- function(x, Data, reps = 100, ...) {
  DLMtool::Iratio(x, Data, reps, yrs = c(4, 8), ...)
}

#' @rdname MPs
#' @export
.Iratio8 <- reduce_survey(Iratio8)

Islope_mod_ <- function(x, Data, reps = 100, yrsmith = 6, lambda, xx, ...) {
  tac <- DLMtool::Islope1(x, Data, reps,
    yrsmth = yrsmith,
    lambda = lambda, xx = xx, ...
  )
  last_catch_rec <- Data@Cat[x, length(Data@Cat[x, ])]
  tac[tac > (1.2 * last_catch_rec)] <- 1.2 * last_catch_rec
  tac
}

#' @rdname MPs
#' @export
Islope_mod1 <- function(x, Data, reps = 100, ...) {
  Islope_mod_(x, Data, reps = 100, lambda = 0.4, xx = 0.2, ...)
}

#' @rdname MPs
#' @export
.Islope_mod1 <- reduce_survey(Islope_mod1)

#' @rdname MPs
#' @export
Islope_mod2 <- function(x, Data, reps = 100, ...) {
  Islope_mod_(x, Data, reps = 100, lambda = 0.4, xx = 0.3, ...)
}
.Islope_mod2 <- reduce_survey(Islope_mod2)

#' @rdname MPs
#' @export
Islope_mod3 <- function(x, Data, reps = 100, ...) {
  Islope_mod_(x, Data, reps = 100, lambda = 0.4, xx = 0.3, ...)
}

#' @rdname MPs
#' @export
.Islope_mod3 <- reduce_survey(Islope_mod3)

#' @rdname MPs
#' @export
Islope_mod4 <- function(x, Data, reps = 100, ...) {
  Islope_mod_(x, Data, reps = 100, lambda = 0.2, xx = 0.2, ...)
}

#' @rdname MPs
#' @export
.Islope_mod4 <- reduce_survey(Islope_mod4)

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

#' @rdname MPs
#' @export
.SBT1 <- reduce_survey(DLMtool::SBT1)

# #' @rdname MPs
# #' @export
# .SBT2 <- reduce_survey(DLMtool::SBT2)

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
  delta_ind_y <- Data@Ind[x, this_year] / Data@Ind[x, this_year - 1] - 1
  catch_rec <- Data@MPrec[x]

  # FIXME: Data@Cat vs. MPrec (last TAC)? Different now, right? Which to use?
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

#' @rdname MPs
#' @export
.IDX_smooth <- reduce_survey(IDX_smooth)

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