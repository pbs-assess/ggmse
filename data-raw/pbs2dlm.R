#' Create the data file for the given species by running gfplot routines
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_data()
#' fetch_data("yelloweye rockfish")
#' }
fetch_data <- function(species_name = "shortraker rockfish", file) {
  d <- list()
  d$commercial_samples <- gfplot::get_commercial_samples(species_name)
  d$survey_samples <- gfplot::get_survey_samples(species_name)
  d$catch <- gfplot::get_catch(species_name)
  d$survey_index <- gfplot::get_survey_index(species_name)
  saveRDS(d, file)
}

#' Load the data in from the data file for the given species
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @return the contents of the rds file as a list
#'
#' @importFrom rlang abort
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- load_data()
#' }
load_data <- function(species_name = "shortraker rockfish",
                      file = file.path(here::here("generated-data"),
                                       paste0(gsub(" ",
                                                   "-",
                                                   species_name),
                                              ".rds"))){
  if(!file.exists(file)){
    abort("Error, file ", file, " does not exist. To create it, run fetch_data().\n")
  }
  readRDS(file)
}

#' Does the data file exist or not for the given species
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @return the contents of the rds file as a list
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_file_exists("shortraker rockfish")
#' }
data_file_exists <- function(species_name,
                             file = file.path(here::here("generated-data"),
                                              paste0(gsub(" ",
                                                          "-",
                                                          species_name),
                                                     ".rds"))){
  file.exists(file)
}

#' Create a Stock object for DLMtool from data and values, to be used in an Operating Model (OM)
#'
#' @param d An S4 object of class DLMtool Data. If NULL, default values from the statring_stock
#'   will be used in the returned object. If supplied, select values will be copied from the
#'   Data object into the returned Stock object.
#'
#' @return An S4 object of class DLMtool Stock.
#'
#' @export
#' @examples
#' \dontrun{
#' library(gfplot)
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' dat <- create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' stk <- create_dlm_stock(dat, starting_stock = "Rockfish")
#' stk2 <- create_dlm_stock(starting_stock = "Rockfish")
#' stk3 <- create_dlm_stock()
#' }
create_dlm_stock <- function(dat = NULL,
                             starting_stock = NA,
                             name = obj@Name,
                             common_name = obj@Common_Name,
                             species = obj@Species,
                             maxage = obj@maxage,
                             r0 = obj@R0,
                             m = obj@M,
                             m2 = obj@M2,
                             mexp = obj@Mexp,
                             msd = obj@Msd,
                             mgrad = obj@Mgrad,
                             h = obj@h,
                             srrel = obj@SRrel,
                             perr = obj@Perr,
                             ac = obj@AC,
                             period = obj@Period,
                             amplitude = obj@Amplitude,
                             linf = obj@Linf,
                             k = obj@K,
                             t0 = obj@t0,
                             lencv = obj@LenCV,
                             ksd = obj@Ksd,
                             linfsd = obj@Linfsd,
                             l50 = obj@L50,
                             l50_95= obj@L50_95,
                             d = obj@D,
                             a = obj@a,
                             b = obj@b,
                             size_area_1 = obj@Size_area_1,
                             frac_area_1 = obj@Frac_area_1,
                             prob_staying = obj@Prob_staying,
                             fdisc = obj@Fdisc,
                             src = obj@Source){

  if(is.na(starting_stock)){
    obj <- methods::new("Stock")
  }else if(starting_stock %in% avail("Stock")){
    obj <- get(starting_stock)
  }else{
    stop("starting_stock '", starting_stock, "', doesn't exist. Use one of:\n",
         paste(avail("Stock"), collapse = "\n"))
  }

  obj@Name <- ifelse(is.null(dat), name, dat@Name)
  obj@Common_Name <- ifelse(is.null(dat), common_name, dat@Common_Name)
  obj@Species <- ifelse(is.null(dat), species, dat@Species)
  obj@maxage <- ifelse(is.null(dat), maxage, dat@MaxAge)
  obj@R0 <- r0
  obj@M <- m
  obj@M2 <- m2
  obj@Mexp <- mexp
  obj@Msd <- msd
  obj@Mgrad <- mgrad
  obj@h <- h
  obj@SRrel <- srrel
  obj@Perr <- perr
  obj@AC <- ac
  obj@Period <- period
  obj@Amplitude <- amplitude
  obj@Linf <- ifelse(is.null(dat), linf, dat@vbLinf)
  obj@K <- ifelse(is.null(dat), k, dat@vbK)
  obj@t0 <- ifelse(is.null(dat), t0, dat@vbt0)
  obj@LenCV <- ifelse(is.null(dat), lencv, dat@LenCV)
  obj@Ksd <- ifelse(is.null(dat), ksd, dat@CV_vbK)
  # obj@Kgrad <- ifelse(is.null(dat), kgrad, dat@CV_vbK)
  obj@Linfsd <- ifelse(is.null(dat), linfsd, dat@CV_vbLinf)
  # obj@Linfgrad <- linfgrad
  obj@L50 <- ifelse(is.null(dat), l50, dat@L50)
  obj@L50_95<- ifelse(is.null(dat), l50_95, dat@L95 - dat@L50)
  obj@D <- d
  obj@a <- ifelse(is.null(dat), a, dat@wla)
  obj@b <- ifelse(is.null(dat), b, dat@wlb)
  obj@Size_area_1 <- size_area_1
  obj@Frac_area_1 <- frac_area_1
  obj@Prob_staying <- prob_staying
  obj@Fdisc <- fdisc
  obj@Source <- src

  obj
}

#' Create a Fleet object for DLMtool from data and values, to be used in an Operating Model (OM)
#'
#' @param d An S4 object of class DLMtool Data. If NULL, default values from the statring_fleet
#'   will be used in the returned object. If supplied, select values will be copied from the
#'   Data object into the returned Fleet object.
#'
#' @return An S4 object of class DLMtool Fleet.
#'
#' @export
#' @examples
#' \dontrun{
#' library(gfplot)
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' dat <- create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' flt <- create_dlm_fleet(dat, starting_fleet = "Generic_Fleet")
#' flt2 <- create_dlm_fleet(starting_fleet = "Generic_Fleet")
#' flt3 <- create_dlm_fleet()
#' }
create_dlm_fleet <- function(dat = NULL,
                             starting_fleet = NA,
                             name = obj@Name,
                             nyears = obj@nyears,
                             spat_targ = obj@Spat_targ,
                             effyears = obj@EffYears,
                             efflower = obj@EffLower,
                             esd = obj@Esd,
                             qinc = obj@qinc,
                             qcv = obj@qcv,
                             l5 = obj@L5,
                             lfs = obj@LFS,
                             vmaxlen = obj@Vmaxlen,
                             isrel = obj@isRel,
                             lr5 = obj@LR5,
                             lfr = obj@LFR,
                             rmaxlen = obj@Rmaxlen,
                             dr = obj@DR,
                             selyears = obj@SelYears,
                             absselyears = obj@AbsSelYears,
                             l5lower = obj@L5Lower,
                             l5upper = obj@L5Upper,
                             lfslower = obj@LFSLower,
                             lfsupper = obj@LFSUpper,
                             vmaxlower = obj@VmaxLower,
                             vmaxupper = obj@VmaxUpper,
                             currentyr = obj@CurrentYr,
                             mpa = obj@MPA){

  if(is.na(starting_fleet)){
    obj <- methods::new("Fleet")
  }else if(starting_fleet %in% avail("Fleet")){
    obj <- get(starting_fleet)
  }else{
    stop("starting_fleet '", starting_fleet, "', doesn't exist. Use one of:\n",
          paste(avail("Fleet"), collapse = "\n"))
  }

  obj@Name <- ifelse(is.null(dat), name, dat@Name)
  obj@nyears <- nyears
  obj@Spat_targ <- spat_targ
  obj@EffYears <- effyears
  obj@EffLower <- efflower
  obj@Esd <- esd
  obj@qinc <- qinc
  obj@qcv <- qcv
  obj@L5 <- l5
  obj@LFS <- lfs
  obj@Vmaxlen <- vmaxlen
  obj@isRel <- isrel
  obj@LR5 <- lr5
  obj@LFR <- lfr
  obj@Rmaxlen <- rmaxlen
  obj@DR <- dr
  obj@SelYears <- selyears
  obj@AbsSelYears <- absselyears
  obj@L5Lower <- l5lower
  obj@L5Upper <- l5upper
  obj@LFSLower <- lfslower
  obj@LFSUpper <- lfsupper
  obj@VmaxLower <- vmaxlower
  obj@VmaxUpper <- vmaxupper
  obj@CurrentYr <- currentyr
  obj@MPA <- mpa

  obj
}

#' Create an Obs object for DLMtool from data and values, to be used in an Operating Model (OM)
#'
#' @param d An S4 object of class DLMtool Data. If NULL, default values from the starting_obs
#'   will be used in the returned object. If supplied, select values will be copied from the
#'   Data object into the returned Obs object.
#'
#' @return An S4 object of class DLMtool Obs.
#' @export
#' @examples
#' \dontrun{
#' library(gfplot)
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' dat <- create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' obs <- create_dlm_obs(dat, starting_obs = "Imprecise_Unbiased")
#' obs <- create_dlm_obs(starting_obs = "Imprecise_Unbiased")
#' obs <- create_dlm_obs()
#' }
create_dlm_obs <- function(dat = NULL,
                           starting_obs = NA,
                           name = obj@Name,
                           cobs = obj@Cobs,
                           cbiascv = obj@Cbiascv,
                           caa_nsamp = obj@CAA_nsamp,
                           caa_ess = obj@CAA_ESS,
                           cal_nsamp = obj@CAL_nsamp,
                           cal_ess = obj@CAL_ESS,
                           iobs = obj@Iobs,
                           ibiascv = obj@Ibiascv,
                           btobs = obj@Btobs,
                           btbiascv = obj@Btbiascv,
                           beta = obj@beta,
                           lenmbiascv = obj@LenMbiascv,
                           mbiascv = obj@Mbiascv,
                           kbiascv = obj@Kbiascv,
                           t0biascv = obj@t0biascv,
                           linfbiascv = obj@Linfbiascv,
                           lfcbiascv = obj@LFCbiascv,
                           lfsbiascv = obj@LFSbiascv,
                           fmsybiascv = obj@FMSYbiascv,
                           fmsy_mbiascv = obj@FMSY_Mbiascv,
                           bmsy_b0biascv = obj@BMSY_B0biascv,
                           irefbiascv = obj@Irefbiascv,
                           brefbiascv = obj@Brefbiascv,
                           crefbiascv = obj@Crefbiascv,
                           dbiascv = obj@Dbiascv,
                           dobs = obj@Dobs,
                           hbiascv = obj@hbiascv,
                           recbiascv = obj@Recbiascv){

  if(is.na(starting_obs)){
    obj <- methods::new("Obs")
  }else if(starting_obs %in% avail("Obs")){
    obj <- get(starting_obs)
  }else{
    stop("starting_obs '", starting_obs, "', doesn't exist. Use one of:\n",
          paste(avail("Obs"), collapse = "\n"))
  }

  obj@Name <- ifelse(is.null(dat), name, dat@Name)
  obj@Cobs <- cobs
  obj@Cbiascv <- cbiascv
  obj@CAA_nsamp <- caa_nsamp
  obj@CAA_ESS <- caa_ess
  obj@CAL_nsamp <- cal_nsamp
  obj@CAL_ESS <- cal_ess
  obj@Iobs <- iobs
  obj@Ibiascv <- ibiascv
  obj@Btobs <- btobs
  obj@Btbiascv <- btbiascv
  obj@beta <- beta
  obj@LenMbiascv <-lenmbiascv
  obj@Mbiascv <- mbiascv
  obj@Kbiascv <- kbiascv
  obj@t0biascv <- t0biascv
  obj@Linfbiascv <- linfbiascv
  obj@LFCbiascv <- lfcbiascv
  obj@LFSbiascv <- lfsbiascv
  obj@FMSYbiascv <- fmsybiascv
  obj@FMSY_Mbiascv <- fmsy_mbiascv
  obj@BMSY_B0biascv <- bmsy_b0biascv
  obj@Irefbiascv <- irefbiascv
  obj@Brefbiascv <- brefbiascv
  obj@Crefbiascv <- crefbiascv
  obj@Dbiascv <- dbiascv
  obj@hbiascv <- hbiascv
  obj@Recbiascv <- recbiascv

  obj
}

#' Create an Imp object for DLMtool from data and values, to be used in an Operating Model (OM)
#'
#' @param d An S4 object of class DLMtool Data. If NULL, default values from the starting_imp
#'   will be used in the returned object. If supplied, select values will be copied from the
#'   Data object into the returned Imp object.
#'
#' @return An S4 object of class DLMtool Imp.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gfplot)
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' dat <- create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' imp <- create_dlm_imp(dat, starting_imp = "Perfect_Imp")
#' imp <- create_dlm_imp(starting_imp = "Perfect_Imp")
#' imp <- create_dlm_imp()
#' }
create_dlm_imp <- function(dat = NULL,
                           starting_imp = NA,
                           name = obj@Name,
                           tacfrac = obj@TACFrac,
                           tacsd = obj@TACSD,
                           taefrac = obj@TAEFrac,
                           taesd = obj@TAESD,
                           sizelimfrac = obj@SizeLimFrac,
                           sizelimsd = obj@SizeLimSD){

  if(is.na(starting_imp)){
    obj <- methods::new("Imp")
  }else if(starting_imp %in% avail("Imp")){
    obj <- get(starting_imp)
  }else{
    stop("starting_imp '", starting_imp, "', doesn't exist. Use one of:\n",
          paste(avail("Imp"), collapse = "\n"))
  }

  obj@Name <- ifelse(is.null(dat), name, dat@Name)
  obj@TACFrac <- tacfrac
  obj@TACSD <- tacsd
  obj@TAEFrac <- taefrac
  obj@TAESD <- taesd
  obj@SizeLimFrac <- sizelimfrac
  obj@SizeLimSD <- sizelimsd

  obj
}
