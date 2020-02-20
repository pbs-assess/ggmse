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
#' @importFrom graphics boxplot lines par points
#' @importFrom stats lm predict rnorm
#'
#' @examples
#' library(DLMtool)
#' om <- DLMtool::testOM
#' om@nsim <- 3
#' temp_mp <- reduce_survey(Islope1)
#' # mse <- runMSE(OM = om, MPs = "temp_mp")
reduce_survey <- function(mp, slots = c("Ind", "CAA", "CAL", "ML"),
                          index = function(x) seq(1, x, by = 2)) {
  force(mp)
  force(slots)
  force(index)
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
Iratio2 <- function(x, Data, reps = 100, ...) {
  ind <- which(!is.na(Data@Ind[x, , drop = TRUE]))
  numerator_yrs <- rev(which(!is.na(ind)))[1:2]
  denominator_yrs <- rev(which(!is.na(ind)))[3:5]
  yrs1 <- length(ind) - min(numerator_yrs) + 1
  yrs2 <- length(ind) - min(denominator_yrs) + 1
  DLMtool::Iratio(x, Data, reps, yrs = c(yrs1, yrs2), ...)
}
class(Iratio2) <- "MP"

#' @rdname MPs
#' @export
.Iratio2 <- reduce_survey(Iratio2)

Islope_mod_ <- function(x, Data, reps = 100, yrsmth = 6, lambda, xx,
                        increase_cap = 1.2, ...) {
  tac <- DLMtool::Islope_(x, Data, reps,
    yrsmth = yrsmth,
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

#' Itarget MP
#'
#' @param x A position in the data object. As per \pkg{DLMtool}.
#' @param Data A data object. As per \pkg{DLMtool}.
#' @param reps The number of stochastic samples of the MP recommendation(s). As
#'   per \pkg{DLMtool}.
#' @param w A smoothing parameter that defines the "steepness" of the adjustment
#'   slope.
#' @param lambda Fraction of the average index over the 10 years prior to the
#'   projection period below which future TACs are reduced quadratically.
#' @param delta `I_{target}` is `(1 + \delta) I_{ave}`; the target index value.
#' @param xx Catch target defined as `(1 - xx)C_{ave}`, where `xx` is the
#'   proportional difference between the future catch and the average historical
#'   catch over the last 5 years of the historical period.
#' @param index_target_window Index target window in years.
#' @param index_current_window Index current window in years. Years over
#'   which the average index is calculated.
#' @param catch_target_window Catch target window in years.
#'
#' @export
#' @examples
#' Itarget(1, DLMtool::SimulatedData)
Itarget <- function(
  x,
  Data,
  reps = 1,
  w = 0,
  lambda = 0.2,
  delta = 1,
  xx = 1,
  index_target_window = 10,
  index_current_window = 5,
  catch_target_window = 5) {

  ind_index_proj <- (length(Data@Year) - (index_current_window - 1)):length(Data@Year)
  ylast_hist <- (Data@LHYear[1] - Data@Year[1]) + 1
  ind_catch_hist <- ((ylast_hist - (catch_target_window - 1)):ylast_hist)
  ind_index_hist <- ((ylast_hist - (index_target_window - 1)):ylast_hist)
  C_dat <- Data@Cat[x, ind_catch_hist]

  Irecent <- mean(Data@Ind[x, ind_index_proj], na.rm = TRUE)
  Iave <- mean(Data@Ind[x, ind_index_hist], na.rm = TRUE)
  TACtarg <- xx * mean(C_dat, na.rm = TRUE)
  .Itarget <- delta * Iave
  I0 <- lambda * Iave

  if (Irecent >= I0) {
    TAC <- TACtarg * ((w + (1 - w) * ((Irecent - I0) / (.Itarget - I0))))
  } else {
    TAC <- w * TACtarg * (Irecent / I0)^2
  }
  if (TAC < 0) TAC <- 0

  # browser()
  TAC <- DLMtool::TACfilter(TAC)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}
class(Itarget) <- "MP"

#' @rdname Itarget
#' @export
Itarget_base <- function(x, Data, reps = 1) {
  Itarget(x = x, Data = Data, reps = reps, lambda = 0.2, delta = 1, w = 0.5, xx = 1)
}
class(Itarget_base) <- "MP"

#' @rdname Itarget
#' @export
Itarget_w0.8 <- function(x, Data, reps = 1) {
  Itarget(x = x, Data = Data, reps = reps, lambda = 0.2, delta = 1, w = 0.8, xx = 1)
}
class(Itarget_w0.8) <- "MP"

#' @rdname Itarget
#' @export
Itarget_x0.2 <- function(x, Data, reps = 1) {
  Itarget(x = x, Data = Data, reps = reps, lambda = 0.2, delta = 1, w = 0.5, xx = 1.2)
}
class(Itarget_x0.2) <- "MP"

#' @rdname Itarget
#' @export
Itarget_x0.8 <- function(x, Data, reps = 1) {
  Itarget(x = x, Data = Data, reps = reps, lambda = 0.2, delta = 1, w = 0.8, xx = 0.8)
}
class(Itarget_x0.8) <- "MP"

#' @rdname Itarget
#' @export
Itarget_d1.2 <- function(x, Data, reps = 1) {
  Itarget(x = x, Data = Data, reps = reps, lambda = 0.2, delta = 1.2, w = 0.5, xx = 1)
}
class(Itarget_d1.2) <- "MP"

#' @rdname Itarget
#' @export
Itarget_d0.8 <- function(x, Data, reps = 1) {
  Itarget(x = x, Data = Data, reps = reps, lambda = 0.2, delta = 0.8, w = 0.5, xx = 1)
}
class(Itarget_d0.8) <- "MP"

#' @rdname MPs
#' @export
.Itarget_base <- reduce_survey(Itarget_base)

#' @rdname MPs
#' @export
.Itarget_w0.8 <- reduce_survey(Itarget_w0.8)

#' @rdname MPs
#' @export
.Itarget_x0.2 <- reduce_survey(Itarget_x0.2)

#' @rdname MPs
#' @export
.Itarget_x0.8 <- reduce_survey(Itarget_x0.8)

#' @rdname MPs
#' @export
.Itarget_d1.2 <- reduce_survey(Itarget_d1.2)

#' @rdname MPs
#' @export
.Itarget_d0.8 <- reduce_survey(Itarget_d0.8)

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
  TAC <- Data@MPrec[x] * deltaI * DLMtool::trlnorm(reps, 1, Data@CV_Ind[x, 1])
  TAC <- DLMtool::TACfilter(TAC)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}
class(ITM_hist) <- "MP"

#' @rdname MPs
#' @export
.ITM_hist <- reduce_survey(ITM_hist)

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
#' @param tac_floor TAC when `delta_min` is met or exceeded. If left as `NULL`,
#'   the floor will be set to 20% of average catch from the last
#' 5 years.
#' @param year_ref Number of years before the present year for the comparison of
#'   the index value. Default looks back one year. For a biennial survey this
#'   should be changed to something larger such as 2 or the recommended TAC will
#'   never change. Note that the algorithm fills in `NA` values in the index as
#'   the previous non-NA value.
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
                delta_max = 0.25, lambda = 1, tac_floor = NULL,
                year_ref = 1) {
  dependencies <- "Data@Ind"

  if (lambda < 0) lambda <- 0
  if (lambda > 1) lambda <- 1

  if (is.null(tac_floor)) {
    yrlast <- match(Data@LHYear[1], Data@Year)
    yrfirst <- yrlast - 5 + 1
    C_dat <- Data@Cat[x, yrfirst:yrlast]
    tac_floor <- 0.2 * mean(C_dat, na.rm = TRUE)
  }
  if (tac_floor < 0) tac_floor <- 0

  this_year <- length(Data@Year)

  # Stepwise fill in NAs with last available value:
  temp_Ind <- stepwise_NAs(Data@Ind[x, , drop = TRUE])
  delta_ind_y <- temp_Ind[this_year] / temp_Ind[this_year - year_ref] - 1
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

IDX2 <- IDX
formals(IDX2)$tac_floor <- 2

#' @rdname MPs
#' @export
.IDX <- reduce_survey(IDX2)

#' @param ... Other arguments to pass to [IDX].
#' @rdname IDX
#' @export
IDX_smooth <- function(x, Data, reps = 100, tac_floor = 1, ...) {
  IDX(x, Data, reps, lambda = 0.5, tac_floor = tac_floor, ...)
}
class(IDX_smooth) <- "MP"

IDX_smooth2 <- IDX_smooth
formals(IDX_smooth2)$tac_floor <- 2

#' @rdname MPs
#' @export
.IDX_smooth <- reduce_survey(IDX_smooth2)

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

#' @param plot Logical. Show the plot?
#' @param yrsmth Years over which to calculate mean catches.
#' @param xx Parameter controlling the TAC. Mean catches are multiplied by `(1 - xx)`.
#'
#' @rdname MPs
#' @export
CC1.0 <- DLMtool::CC1

#' @rdname MPs
#' @export
CC100 <- DLMtool::CC1

#' @rdname MPs
#' @export
CC0.9 <- DLMtool::CC2

#' @rdname MPs
#' @export
CC0.8 <- DLMtool::CC3

#' @rdname MPs
#' @export
CC0.7 <- DLMtool::CC4

#' @rdname MPs
#' @export
CC0.6 <- DLMtool::CC5

#' @rdname MPs
#' @export
CC1.1 <- DLMtool::CC1
formals(CC1.1)$xx <- -0.1
class(CC1.1) <- "MP"

#' @rdname MPs
#' @export
CC1.2 <- DLMtool::CC1
formals(CC1.2)$xx <- -0.1
class(CC1.2) <- "MP"

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
SP_gf <- function(x, Data, reps = 1, LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY",
                  start = list(r_prior = c(0.3, 0.1)), use_r_prior = TRUE, tac_max_increase = 1.2, tac_max_decrease = 0.5, tac_floor = 0.1, tac_increase_buffer = 1.05) {
  do_Assessment <- MSEtool::SP(
    x = x, Data = Data,
    control = list(iter.max = 10000, eval.max = 20000), n_seas = 1,
    use_r_prior = use_r_prior, start = start
  )
  Rec <- MSEtool::HCR_ramp(
    Assessment = do_Assessment, reps = reps, LRP = LRP,
    TRP = TRP, RP_type = RP_type
  )
  if (!is.na(Rec@TAC)) {
    if (as.list(do_Assessment@SD, "Std. Error")$log_FMSY > 1) {
      warning("Std. Error too large; using last TAC")
      Rec@TAC <- Data@MPrec[x]
    }
    if (Rec@TAC > Data@MPrec[x] && Rec@TAC < tac_increase_buffer * Data@MPrec[x]) {
      Rec@TAC <- Data@MPrec[x]
    }
    if (Rec@TAC > tac_max_increase * Data@MPrec[x]) {
      warning("TAC > tac_max_increase; using tac_max_increase")
      Rec@TAC <- tac_max_increase * Data@MPrec[x]
    }
    if (Rec@TAC < tac_max_decrease * Data@MPrec[x]) {
      warning("TAC < tac_max_decrease last TAC; using tac_max_decrease")
      Rec@TAC <- tac_max_decrease * Data@MPrec[x]
    }
    if (Rec@TAC < tac_floor * Data@Cat[x, Data@LHYear]) {
      warning("TAC < tac_floor; using tac_floor")
      Rec@TAC <- tac_floor * Data@Cat[x, Data@LHYear]
    }
  }
  # Rec@Misc <- MSEtool:::Assess_diagnostic(x, Data, do_Assessment, include_assessment = FALSE)
  return(Rec)
}

#' @param ... Other args
#' @rdname SP_gf
#' @export
SP6040_gf <- function(x, Data, reps = 1, ...) {
  SP_gf(x, Data, reps = 1, LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY", ...)
}
class(SP6040_gf) <- "MP"

#' @rdname SP_gf
#' @export
.SP6040_gf <- reduce_survey(SP6040_gf)

#' @rdname SP_gf
#' @export
SP8040_gf <- function(x, Data, reps = 1, ...) {
  SP_gf(x, Data, reps = 1, LRP = 0.4, TRP = 0.8, RP_type = "SSB_SSBMSY", ...)
}
class(SP8040_gf) <- "MP"

#' @rdname SP_gf
#' @export
.SP8040_gf <- reduce_survey(SP8040_gf)

#' @rdname SP_gf
#' @export
SP4010_gf <- function(x, Data, reps = 1, ...) {
  SP_gf(x, Data, reps = 1, LRP = 0.1, TRP = 0.4, RP_type = "SSB_SSB0", ...)
}
class(SP4010_gf) <- "MP"

#' @rdname SP_gf
#' @export
.SP4010_gf <- reduce_survey(SP4010_gf)

#' @param mp MP to wrap
#' @param r_prior Mean and SD of r prior
#' @export
#' @rdname SP_gf
#' @examples
#' my_mp <- add_SP_prior(SP4010_gf, c(0.3, 0.05))
#' library(DLMtool)
#' om <- DLMtool::testOM
#' om@nsim <- 5
#' om@proyears <- 10
#' mse <- runMSE(om, MPs = "my_mp")
add_SP_prior <- function(mp, r_prior, tac_max_increase = 1.2,
                         tac_max_decrease = 0.5, tac_floor = 0.1,
                         tac_increase_buffer = 1.05, ...) {
  force(mp)
  force(r_prior)
  force(tac_max_increase)
  force(tac_max_decrease)
  force(tac_floor)
  force(tac_increase_buffer)

  f <- function(x, Data, reps = 1, ...) {
    mp(
      x = x, Data = Data, reps = reps, start = list(r_prior = r_prior),
      tac_max_increase = tac_max_increase, tac_max_decrease = tac_max_decrease,
      tac_floor = tac_floor, tac_increase_buffer = tac_increase_buffer, ...
    )
  }
  `class<-`(f, "MP")
}

#' Use AddInd
#'
#' @param mp MP to use
#'
#' @export
#' @examples
#' my_mp <- use_AddInd(IDX)
#' library(DLMtool)
#' om <- DLMtool::testOM
#' om@nsim <- 5
#' om@proyears <- 10
#' mse <- runMSE(om, MPs = "my_mp")
use_AddInd <- function(mp) {
  force(mp)
  f <- function(x, Data, reps = 1) {
    Data@Ind <- Data@AddInd[, 1, ]
    Data@CV_Ind <- Data@CV_AddInd[, 1, ]
    mp(x = x, Data = Data, reps = reps)
  }
  `class<-`(f, "MP")
}
