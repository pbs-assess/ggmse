# Performance Metrics

#' Evaluate Performance Metrics
#'
#' @param mse_obj MSE object, output of the DLMtool [DLMtool::runMSE()] function
#' @param pm_list List of performace metric names
#' @param refs Optional. List containing the reference limits for each metric
#' @param yrs Numeric vector of length 2 with year indices to summarize performance
#'
#' @returns A data frame containing the Management procedures, Performace metrics,
#' probability, probability caption, description, and class of the management procedure.
eval_pm <- function(mse_obj,
                    pm_list = NULL,
                    refs = NULL,
                    yrs = NULL){

  if(is.null(pm_list)){
    stop("pm_list is a required argument.",
         call. = FALSE)
  }

  run_pm <- list()
  for(i in seq_along(pm_list)){
    ref <- refs[[pm_list[i]]]
    yr <- yrs[pm_list[i]]
    if(is.null(ref)){
      if(is.null(yr)){
        run_pm[[i]] <- eval(call(pm_list[[i]], mse_obj))
      }else{
        run_pm[[i]] <- eval(call(pm_list[[i]], mse_obj, Yrs = yr))
      }
    }else{
      if(is.null(yr)){
        run_pm[[i]] <- eval(call(pm_list[[i]], mse_obj, Ref = ref))
      }else{
        run_pm[[i]] <- eval(call(pm_list[[i]], mse_obj, Ref = ref, Yrs = yr))
      }
    }
  }

  out <- list()
  for(i in seq_along(pm_list)){
    pm <- pm_list[[i]]
    prob <- run_pm[[match(pm, pm_list)]]@Mean
    probcap <- run_pm[[match(pm, pm_list)]]@Caption
    name <- run_pm[[match(pm, pm_list)]]@Name

    mp_type <- MPtype(mse_obj@MPs)
    class <- mp_type[match(mse_obj@MPs, mp_type[,1]), 2]

    out[[i]] <- as_tibble(data.frame(id = i,
                                     mp = mse_obj@MPs,
                                     pm = pm,
                                     prob = prob,
                                     probcap = probcap,
                                     english = name,
                                     class = class))

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
#' @export
#'
#' @examples
#' P10 <- pm_factory("SBMSY", 0.1)
pm_factory <- function(pm_type,
                       ref = 0.1,
                       yrs = NULL){
  force(pm_type)
  force(ref)
  force(yrs)
  pm_obj <- new("PMobj")
  pm_obj@Ref <- ref
  if(pm_type == "SBMSY"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- "Spawning Biomass relative to SBMSY"
      pm_obj@Caption <- paste0("Prob. SB > ", ifelse(ref == 1, "", ref), " SBMSY (Years ", yrs[1], " - ", yrs[2], ")")
      pm_obj@Stat <- mse_obj@B_BMSY[ , , yrs[1]:yrs[2]]
      pm_obj@Prob <- calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }else if(pm_type == "AAVY"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <-  paste0("Average Annual Variability in Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <-  paste0('Prob. AAVY < ', ref * 100, "% (Years ", yrs[1], "-", yrs[2], ")")
      y1 <- yrs[1]:(yrs[2] - 1)
      y2 <-(yrs[1] + 1):yrs[2]
      if(mse_obj@nMPs > 1){
        pm_obj@Stat <-  apply(((((mse_obj@C[, , y1] - mse_obj@C[, , y2]) /
                                  mse_obj@C[, , y2]) ^ 2) ^ 0.5),
                              c(1, 2),
                              mean)
      }else{
        pm_obj@Stat <- array(apply(((((mse_obj@C[, 1, y1] - mse_obj@C[, 1, y2]) /
                                        mse_obj@C[, 1, y2]) ^ 2) ^ 0.5), c(1), mean))
      }
      pm_obj@Prob <- calcProb(pm_obj@Stat < pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }else if(pm_type == "PNOF"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- "Probability of not overfishing (F<FMSY)"
      pm_obj@Caption <- paste0("Prob. F < ", ifelse(ref == 1, "", ref), " FMSY (Years ", yrs[1], " - ", yrs[2], ")")
      pm_obj@Stat <- mse_obj@F_FMSY[ , , yrs[1]:yrs[2]]
      pm_obj@Prob <- calcProb(pm_obj@Stat < pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }else if(pm_type == "LTY"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- paste0("Average Yield relative to Reference Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <- paste0("Prob. Yield > ", ifelse(ref == 1, "", ref), " Ref. Yield (Years ", yrs[1], " - ", yrs[2], ")")
      ref_yield <- array(mse_obj@OM$RefY, dim = dim(mse_obj@C[, , yrs[1]:yrs[2]]))
      pm_obj@Stat <- mse_obj@C[, , yrs[1]:yrs[2]] / ref_yield
      pm_obj@Prob <- calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }else if(pm_type == "Yield"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- paste0("Average Yield relative to Reference Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <- paste0("Prob. Yield > ", ifelse(ref == 1, "", ref), " Ref. Yield (Years ", yrs[1], " - ", yrs[2], ")")
      ref_yield <- array(mse_obj@OM$RefY, dim = dim(mse_obj@C[, , yrs[1]:yrs[2]]))
      pm_obj@Stat <- mse_obj@C[, , yrs[1]:yrs[2]] / ref_yield
      pm_obj@Prob <- calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }
  class(created_by_pm_factory) <- "PM"
  created_by_pm_factory
}

P10 <- pm_factory("SBMSY", 0.1)
P10_yrs6_20 <- pm_factory("SBMSY", 0.1, c(6, 20))
P10_yrs21_35 <- pm_factory("SBMSY", 0.1, c(21, 35))
P10_yrs36_50 <- pm_factory("SBMSY", 0.1, c(36, 50))
P40 <- pm_factory("SBMSY", 0.4)
P40_yrs6_20 <- pm_factory("SBMSY", 0.4, c(6, 20))
P40_yrs21_35 <- pm_factory("SBMSY", 0.4, c(21, 35))
P40_yrs36_50 <- pm_factory("SBMSY", 0.4, c(36, 50))
P80 <- pm_factory("SBMSY", 0.8)
P80_yrs6_20 <- pm_factory("SBMSY", 0.8, c(6, 20))
P80_yrs21_35 <- pm_factory("SBMSY", 0.8, c(21, 35))
P80_yrs36_50 <- pm_factory("SBMSY", 0.8, c(36, 50))
P100 <- pm_factory("SBMSY", 1)
P100_yrs6_20 <- pm_factory("SBMSY", 1, c(6, 20))
P100_yrs21_35 <- pm_factory("SBMSY", 1, c(21, 35))
P100_yrs36_50 <- pm_factory("SBMSY", 1, c(36, 50))
PNOF <- pm_factory("PNOF", 1)
PNOF_yrs6_20 <- pm_factory("PNOF", 1, c(6, 20))
PNOF_yrs21_35 <- pm_factory("PNOF", 1, c(21, 35))
PNOF_yrs36_50 <- pm_factory("PNOF", 1, c(36, 50))
LTY <- pm_factory("LTY", 0.5)
Yield <- pm_factory("Yield", 1)
AAVY <- pm_factory("AAVY", 0.2)
