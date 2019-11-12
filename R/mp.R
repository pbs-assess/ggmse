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

#' Biennial survey versions of many \pkg{DLMtool} and \pkg{MSEtool} MP functions
#'
#' These management procedures (MPs) use [reduce_survey()] to eliminate odd
#' years of survey observations to reflect the biennial nature of most
#' groundfish surveys in British Columbia.
#'
#' @param x A position in the data object. As per \pkg{DLMtool}.
#' @param Data A data object. As per \pkg{DLMtool}.
#' @param reps The number of stochastic samples of the MP recommendation(s). As
#'   per \pkg{DLMtool}.
#' @param ... Other arguments to pass to the MP function.
#'
#' @rdname MPs
#' @export
.DD <- reduce_survey(DLMtool::DD)

#' @rdname MPs
#' @export
.DD4010 <- reduce_survey(DLMtool::DD4010)

#' @rdname MPs
#' @export
.DDSS_MSY <- reduce_survey(MSEtool::DDSS_MSY)

#' @rdname MPs
#' @export
.DDSS_4010 <- reduce_survey(MSEtool::DDSS_4010)

#' @rdname MPs
#' @export
.GB_slope <- reduce_survey(DLMtool::GB_slope)

#' @rdname MPs
#' @export
.ICI <- reduce_survey(DLMtool::ICI)

#' @rdname MPs
#' @export
.ICI2 <- reduce_survey(DLMtool::ICI2)

#' @rdname MPs
#' @export
.Iratio <- reduce_survey(DLMtool::Iratio)

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

#' @rdname MPs
#' @export
.LstepCC1 <- reduce_survey(DLMtool::LstepCC1)

#' @rdname MPs
#' @export
.LstepCC2 <- reduce_survey(DLMtool::LstepCC2)

#' @rdname MPs
#' @export
.LstepCC3 <- reduce_survey(DLMtool::LstepCC3)

#' @rdname MPs
#' @export
.LstepCC4 <- reduce_survey(DLMtool::LstepCC4)

#' @rdname MPs
#' @export
.Ltarget1 <- reduce_survey(DLMtool::Ltarget1)

#' @rdname MPs
#' @export
.Ltarget2 <- reduce_survey(DLMtool::Ltarget2)

#' @rdname MPs
#' @export
.Ltarget3 <- reduce_survey(DLMtool::Ltarget3)

#' @rdname MPs
#' @export
.Ltarget4 <- reduce_survey(DLMtool::Ltarget4)

#' @rdname MPs
#' @export
.Ltarget95 <- reduce_survey(DLMtool::Ltarget95)

#' @rdname MPs
#' @export
.SBT1 <- reduce_survey(DLMtool::SBT1)

#' @rdname MPs
#' @export
.SBT2 <- reduce_survey(DLMtool::SBT2)

#' @rdname MPs
#' @export
.SCA_4010 <- reduce_survey(MSEtool::SCA_4010)

#' @rdname MPs
#' @export
.SCA_MSY <- reduce_survey(MSEtool::SCA_MSY)

#' @rdname MPs
#' @export
.SP_4010 <- reduce_survey(MSEtool::SP_4010)

#' @rdname MPs
#' @export
.SP_MSY <- reduce_survey(MSEtool::SP_MSY)

#' @rdname MPs
#' @export
.SPMSY <- reduce_survey(DLMtool::SPMSY)

#' @rdname MPs
#' @export
.SPmod <- reduce_survey(DLMtool::SPmod)