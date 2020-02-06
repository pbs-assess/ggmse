# Performance Metrics

#' Evaluate Performance Metrics
#'
#' @param mse_obj MSE object, output of the DLMtool [DLMtool::runMSE()] function
#' @param pm_list List of performace metric names
#' @param refs Optional. List containing the reference limits for each metric
#' @param yrs Optional. Numeric vector of length 2 with year indices to summarize performance
#'
#' @return A data frame containing the Management procedures, Performance metrics, probability, probability caption, description, and class of the management procedure.
#' @importFrom DLMtool MPtype
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' eval_pm(mse_example, list("PNOF", "P100", "P10", "P40", "LTY", "AAVY"))
eval_pm <- function(mse_obj,
                    pm_list = NULL,
                    refs = NULL,
                    yrs = NULL) {
  if (is.null(pm_list)) {
    stop("pm_list is a required argument.",
      call. = FALSE
    )
  }

  run_pm <- list()
  for (i in seq_along(pm_list)) {
    ref <- refs[[pm_list[i]]]
    yr <- yrs[pm_list[i]]
    if (is.null(ref)) {
      if (is.null(yr)) {
        run_pm[[i]] <- eval(call(pm_list[[i]], mse_obj))
      } else {
        run_pm[[i]] <- eval(call(pm_list[[i]], mse_obj, Yrs = yr))
      }
    } else {
      if (is.null(yr)) {
        run_pm[[i]] <- eval(call(pm_list[[i]], mse_obj, Ref = ref))
      } else {
        run_pm[[i]] <- eval(call(pm_list[[i]], mse_obj, Ref = ref, Yrs = yr))
      }
    }
  }

  out <- list()
  for (i in seq_along(pm_list)) {
    pm <- pm_list[[i]]
    prob <- run_pm[[match(pm, pm_list)]]@Mean
    probcap <- run_pm[[match(pm, pm_list)]]@Caption
    name <- run_pm[[match(pm, pm_list)]]@Name

    mp_type <- MPtype(mse_obj@MPs)
    class <- mp_type[match(mse_obj@MPs, mp_type[, 1]), 2]

    out[[i]] <- as_tibble(data.frame(
      id = i,
      mp = mse_obj@MPs,
      pm = pm,
      prob = prob,
      probcap = probcap,
      english = name,
      class = class
    ))
  }
  do.call(rbind, out)
}

#' Function factory for creating DLMtool Performace Metrics (class "PM") functions
#'
#' @param pm_type The type of performance metric
#' @param ref Reference level to use in secondary part of probability
#' @param yrs A vector of years to include. If NULL, all will be used.
#'
#' @return A DLMtool PM function
#' @importFrom DLMtool calcProb calcMean ChkYrs
#' @importFrom methods new
#' @export
#' @rdname pm
#'
#' @examples
#' P10 <- pm_factory("SBMSY", 0.1)
pm_factory <- function(pm_type,
                       ref = 0.1,
                       yrs = NULL) {
  force(pm_type)
  force(ref)
  force(yrs)
  pm_obj <- new("PMobj")
  pm_obj@Ref <- ref
  if (pm_type == "SBMSY") {
    created_by_pm_factory <- function(mse_obj) {
      yrs <- DLMtool::ChkYrs(yrs, mse_obj)
      pm_obj@Name <- "Spawning Biomass relative to SBMSY"
      pm_obj@Caption <- paste0("Prob. SB > ", ifelse(ref == 1, "", ref), " SBMSY (Years ", yrs[1], " - ", yrs[2], ")")
      pm_obj@Stat <- mse_obj@B_BMSY[, , yrs[1]:yrs[2]]
      pm_obj@Prob <- DLMtool::calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- DLMtool::calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  } else if (pm_type == "AAVY") {
    created_by_pm_factory <- function(mse_obj) {
      yrs <- DLMtool::ChkYrs(yrs, mse_obj)
      pm_obj@Name <- paste0("Average Annual Variability in Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <- paste0("Prob. AAVY < ", ref * 100, "% (Years ", yrs[1], "-", yrs[2], ")")
      y1 <- yrs[1]:(yrs[2] - 1)
      y2 <- (yrs[1] + 1):yrs[2]
      if (mse_obj@nMPs > 1) {
        pm_obj@Stat <- apply(
          ((((mse_obj@C[, , y1] - mse_obj@C[, , y2]) /
            mse_obj@C[, , y2])^2)^0.5),
          c(1, 2),
          mean
        )
      } else {
        pm_obj@Stat <- array(apply(((((mse_obj@C[, 1, y1] - mse_obj@C[, 1, y2]) /
          mse_obj@C[, 1, y2])^2)^0.5), c(1), mean))
      }
      pm_obj@Prob <- DLMtool::calcProb(pm_obj@Stat < pm_obj@Ref, mse_obj)
      pm_obj@Mean <- DLMtool::calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  } else if (pm_type == "PNOF") {
    created_by_pm_factory <- function(mse_obj) {
      yrs <- DLMtool::ChkYrs(yrs, mse_obj)
      pm_obj@Name <- "Probability of not overfishing (F<FMSY)"
      pm_obj@Caption <- paste0("Prob. F < ", ifelse(ref == 1, "", ref), " FMSY (Years ", yrs[1], " - ", yrs[2], ")")
      pm_obj@Stat <- mse_obj@F_FMSY[, , yrs[1]:yrs[2]]
      pm_obj@Prob <- DLMtool::calcProb(pm_obj@Stat < pm_obj@Ref, mse_obj)
      pm_obj@Mean <- DLMtool::calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  } else if (pm_type == "LTY") {
    created_by_pm_factory <- function(mse_obj) {
      yrs <- DLMtool::ChkYrs(yrs, mse_obj)
      pm_obj@Name <- paste0("Average Yield relative to Reference Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <- paste0("Prob. Yield > ", ifelse(ref == 1, "", ref), " Ref. Yield (Years ", yrs[1], " - ", yrs[2], ")")
      ref_yield <- array(mse_obj@OM$RefY, dim = dim(mse_obj@C[, , yrs[1]:yrs[2]]))
      pm_obj@Stat <- mse_obj@C[, , yrs[1]:yrs[2]] / ref_yield
      pm_obj@Prob <- DLMtool::calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- DLMtool::calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  } else if (pm_type == "Yield") {
    created_by_pm_factory <- function(mse_obj) {
      yrs <- DLMtool::ChkYrs(yrs, mse_obj)
      pm_obj@Name <- paste0("Average Yield relative to Reference Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <- paste0("Prob. Yield > ", ifelse(ref == 1, "", ref), " Ref. Yield (Years ", yrs[1], " - ", yrs[2], ")")
      ref_yield <- array(mse_obj@OM$RefY, dim = dim(mse_obj@C[, , yrs[1]:yrs[2]]))
      pm_obj@Stat <- mse_obj@C[, , yrs[1]:yrs[2]] / ref_yield
      pm_obj@Prob <- DLMtool::calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- DLMtool::calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }
  class(created_by_pm_factory) <- "PM"
  created_by_pm_factory
}

#' Various performance metrics
#'
#' @param mse_obj An object of class [MSE-class] from the [DLMtool] package
#'
#' @return An object of class [PMobj-class] from the [DLMtool] package
#' @export
#' @rdname pm
#' @examples
#' P10(mse_example)
#' PNOF_yrs6_20(mse_example)
P10 <- pm_factory("SBMSY", 0.1)

#' @rdname pm
#' @export
P10_yrs6_20 <- pm_factory("SBMSY", 0.1, c(6, 20))

#' @rdname pm
#' @export
P10_yrs21_35 <- pm_factory("SBMSY", 0.1, c(21, 35))

#' @rdname pm
#' @export
P10_yrs36_50 <- pm_factory("SBMSY", 0.1, c(36, 50))

#' @rdname pm
#' @export
P40 <- pm_factory("SBMSY", 0.4)
#' @rdname pm
#' @export
P40_yrs6_20 <- pm_factory("SBMSY", 0.4, c(6, 20))

#' @rdname pm
#' @export
P40_yrs21_35 <- pm_factory("SBMSY", 0.4, c(21, 35))

#' @rdname pm
#' @export
P40_yrs36_50 <- pm_factory("SBMSY", 0.4, c(36, 50))

#' @rdname pm
#' @export
P80 <- pm_factory("SBMSY", 0.8)

#' @rdname pm
#' @export
P80_yrs6_20 <- pm_factory("SBMSY", 0.8, c(6, 20))

#' @rdname pm
#' @export
P80_yrs21_35 <- pm_factory("SBMSY", 0.8, c(21, 35))

#' @rdname pm
#' @export
P80_yrs36_50 <- pm_factory("SBMSY", 0.8, c(36, 50))

#' @rdname pm
#' @export
P100 <- pm_factory("SBMSY", 1)

#' @rdname pm
#' @export
P100_yrs6_20 <- pm_factory("SBMSY", 1, c(6, 20))

#' @rdname pm
#' @export
P100_yrs21_35 <- pm_factory("SBMSY", 1, c(21, 35))

#' @rdname pm
#' @export
P100_yrs36_50 <- pm_factory("SBMSY", 1, c(36, 50))

#' @rdname pm
#' @export
PNOF <- pm_factory("PNOF", 1)

#' @rdname pm
#' @export
PNOF_yrs6_20 <- pm_factory("PNOF", 1, c(6, 20))

#' @rdname pm
#' @export
PNOF_yrs21_35 <- pm_factory("PNOF", 1, c(21, 35))

#' @rdname pm
#' @export
PNOF_yrs36_50 <- pm_factory("PNOF", 1, c(36, 50))

#' @rdname pm
#' @export
LTY <- pm_factory("LTY", 0.5)

#' @rdname pm
#' @export
Yield <- pm_factory("Yield", 1)

#' @rdname pm
#' @export
AAVY <- pm_factory("AAVY", 0.2)

#' @rdname pm
#' @export
LTY_MSY <- function(mse_obj, ref = 0.5, yrs = c(36, 50)) {
  pm_obj <- new("PMobj")
  pm_obj@Ref <- ref
  yrs <- DLMtool::ChkYrs(yrs, mse_obj)
  pm_obj@Name <- paste0("Average Yield relative to Yield from FMSYref (Years ", yrs[1], "-", yrs[2], ")")
  pm_obj@Caption <- paste0("Prob. Yield > ", ifelse(ref == 1, "", ref), " Ref. Yield (Years ", yrs[1], " - ", yrs[2], ")")

  if (!"FMSYref" %in% mse_obj@MPs) {
    stop("FMSYref must be an MP to use this performance metric.", call. = FALSE)
  }
  FMSYref_i <- which(mse_obj@MPs == "FMSYref")
  ref_yield <- array(NA, dim = dim(mse_obj@C[, , yrs[1]:yrs[2]]))

  for (i in seq_len(dim(ref_yield)[2])) {
    ref_yield[, i, ] <- mse_obj@C[, FMSYref_i, yrs[1]:yrs[2]]
  }
  pm_obj@Stat <- mse_obj@C[, , yrs[1]:yrs[2]] / ref_yield
  pm_obj@Prob <- DLMtool::calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
  pm_obj@Mean <- DLMtool::calcMean(pm_obj@Prob)
  pm_obj@MPs <- mse_obj@MPs
  pm_obj
}
class(LTY_MSY) <- "PM"

#' @rdname pm
#' @export
STY_MSY <- LTY_MSY
formals(STY_MSY)$yrs <- c(6, 20)

#' @rdname pm
#' @export
Decline <- function(mse_obj, ref = 0, yrs = c(6, 20)) {
  pm_obj <- new("PMobj")
  pm_obj@Ref <- ref
  yrs <- DLMtool::ChkYrs(yrs, mse_obj)
  pm_obj@Name <- paste0(
    "Probability of decline (Years ",
    yrs[1], "-", yrs[2], ")"
  )
  pm_obj@Caption <- paste0(
    "Prob. decline in B ",
    "(Years ", yrs[1], "-", yrs[2], ")"
  )

  slopes <- apply(mse_obj@SSB[, , yrs[1]:yrs[2]], c(1, 2), function(.x) {
    X <- cbind(rep(1, length(.x)), seq_along(.x))
    m <- stats::.lm.fit(X, log(.x))
    stats::coef(m)[[2]]
  })

  # fake to make DLMtool happy:
  Stat <- array(NA, dim = dim(mse_obj@C[, , yrs[1]:yrs[2]]))
  for (i in seq_len(dim(mse_obj@SSB[, , yrs[1]:yrs[2]])[3])) {
    Stat[, , i] <- slopes
  }

  pm_obj@Stat <- Stat
  pm_obj@Prob <- DLMtool::calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
  pm_obj@Mean <- DLMtool::calcMean(pm_obj@Prob)
  pm_obj@MPs <- mse_obj@MPs
  pm_obj
}
class(Decline) <- "PM"
