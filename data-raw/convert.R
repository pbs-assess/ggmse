#' Convert groundfish PBS data to a DLMtool Data object
#'
#' Takes the output from \pkg{gfplot} data fetching functions and converts them to a
#' DLMtool data object.
#'
#' @param dat A list object output from [gfplot::cache_pbs_data()] or a list
#'   object containing the elements `commercial_samples`, `survey_samples`,
#'   `catch`, `survey_index`.
#' @param name A name for the stock.
#' @param common_name A common name for the stock.
#' @param area The groundfish statistical area to subset the catch by.
#' @param survey A survey abbreviation designating which survey to use for the
#'   relative index of abundance.
#' @param max_year The most recent year of data to include. Default is the max
#'   year found in the data.
#' @param min_mean_length The minimum number of samples to include a mean length
#'   measurement for a given year.
#' @param length_bin_interval An interval for the length bins.
#' @param unsorted_only Include unsorted commercial samples only
#'
#' @importClassesFrom DLMtool Data
#' @importFrom gfplot tidy_catch tidy_survey_index bind_samples fit_mat_ogive
#' @importFrom gfplot fit_vb fit_length_weight
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr tibble rename
#' @importFrom rlang .data
#' @return An S4 object of class DLMtool Data.
#' @export
#'
#' @examples
#' \donttest{
#' library(gfplot)
#' species <- "pacific cod"
#' d <- list()
#' d$commercial_samples <- get_commercial_samples(species)
#' d$survey_samples <- get_survey_samples(species)
#' d$catch <- get_catch(species)
#' d$survey_index <- get_survey_index(species)
#' create_dlm_data(d, name = "BC Pacific Cod", area = "3[CD]+")
#'
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' }
create_dlm_data <- function(dat,
  name = "",
  common_name = "",
  area = "3[CD]+",
  survey = "SYN WCVI",
  max_year = max_year_dat,
  min_mean_length = 10,
  length_bin_interval = 2,
  unsorted_only = FALSE) {

  # Setup ----------
  obj <- methods::new("Data")
  obj@Name <- name
  obj@Common_Name <- common_name
  obj@Units <- "kg"

  max_year_dat <- max(dat$survey_samples$year,
                      dat$commercial_samples$year,
                      dat$catch$year,
                      dat$survey_index$year)
  dat <- purrr::map(dat, ~filter(.x, year <= max_year))

  # Catch ----------
  catch <- tidy_catch(dat$catch, areas = area)
  catch <- catch %>%
    group_by(.data$year) %>%
    summarise(value = sum(.data$value)) %>%
    ungroup()
  ind <- tidy_survey_index(dat$survey_index, survey = survey)
  all_years <- tibble(year = seq(min(c(catch$year, ind$year)), max_year))
  catch <- left_join(all_years, catch, by = "year")

  obj@Cat <- t(matrix(catch$value))
  obj@Year <- catch$year
  obj@t <- length(obj@Cat)
  obj@AvC <- mean(obj@Cat, na.rm = TRUE)
  obj@CV_Cat <- stats::sd(catch$value, na.rm = TRUE) / mean(catch$value, na.rm = TRUE)

  # Index of abundance ----------
  ind <- left_join(all_years, ind, by = "year")
  ind <- t(matrix(ind$biomass))
  obj@Ind <- ind / mean(ind, na.rm = TRUE) # standardise

  # Maturity ----------
  # samps <- bind_samples(
  # dat_comm = dat$commercial_samples,
  # dat_survey = dat$survey_samples
  # )
  samps <- dat$survey_samples
  m_mat <- fit_mat_ogive(samps, type = "length")
  mat_perc <- extract_maturity_perc(stats::coef(m_mat$model))
  se_l50 <- delta_method(~ -(log((1/0.5) - 1) + x1 + x3) / (x2 + x4),
    mean = stats::coef(m_mat$model), cov = stats::vcov(m_mat$model))

  obj@L50 <- mat_perc$f.p0.5 # TODO, female only?
  obj@L95 <- mat_perc$f.p0.95
  obj@CV_L50 <- se_l50 / obj@L50

  # VB model ----------
  mvb <- suppressWarnings(fit_vb(samps, sex = "female"))
  .summary <- summary(TMB::sdreport(mvb$model))
  se <- .summary[,"Std. Error"]
  cv <- se / abs(.summary[,"Estimate"])

  obj@vbK <- mvb$pars[["k"]]
  obj@vbLinf <- mvb$pars[["linf"]]
  obj@vbt0 <- mvb$pars[["t0"]]
  obj@LenCV <- sd2cv(exp(mvb$pars[["log_sigma"]]))
  obj@CV_vbK <- cv[["k"]]
  obj@CV_vbLinf <- cv[["linf"]]
  obj@CV_vbt0 <- cv[["t0"]]

  # Length weight model ----------
  mlw <- fit_length_weight(samps, sex = "female")
  .summary <- summary(TMB::sdreport(mlw$model))
  se <- .summary[,"Std. Error"]

  obj@wla <- exp(mlw$pars[["log_a"]])
  obj@wlb <- mlw$pars[["b"]]
  obj@CV_wla <- sd2cv(se[["log_a"]]) # log scale
  obj@CV_wlb <- se[["b"]] / mlw$pars[["b"]]

  # Mean length timeseries ----------
  if(unsorted_only){
    dat$commercial_samples <- dat$commercial_samples %>%
      filter(sampling_desc == "UNSORTED")
  }
  ml <- tidy_mean_length(dat$commercial_samples) %>%
    filter(.data$n > min_mean_length, .data$year <= max_year) %>%
    right_join(all_years, by = "year")

  obj@ML <- t(matrix(ml$mean_length))

  # Catch at age ----------
  obj@CAA <- tidy_caa(dat$commercial_samples, yrs = all_years$year)
  obj@MaxAge <- ncol(obj@CAA[1, , ])

  # Catch at length ----------
  # CAL Catch-at-length data. An array with dimensions nsim x nyears
  # x length(CAL_bins). Non-negative integers
  length_bins <- seq(0, 1e4, length_bin_interval)
  obj@CAL <- tidy_cal(dat$commercial_samples, interval = length_bin_interval,
    yrs = all_years$year)
  # CAL_bins The values delimiting the length bins for the catch-at-length
  # data. Vector. Non-negative real numbers
  obj@CAL_bins <- get_cal_bins(obj@CAL, length_bin_interval = length_bin_interval) # mid points

  obj
}

#' Generate mean-length time series
#'
#' @param dat Commercial or biological samples from [get_commercial_samples()] or
#'   [get_survey_samples()].
#' @param unsorted_only Logical for whether to only include the unsorted samples.
#'   Only applies to the commercial data.
#' @export
tidy_mean_length <- function(dat, unsorted_only = FALSE) {
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  if ("sampling_desc" %in% names(dat) && unsorted_only) {
    dat <- filter(dat, .data$sampling_desc == "UNSORTED")
  }
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length)) #RF removed female filtering #female only, .data$sex %in% 2
  group_by(dat, .data$year) %>%
    summarise(n = n(), mean_length = mean(.data$length)) %>%
    ungroup()
}

#' @export
#' @rdname tidy_caa
get_cal_bins <- function(cal_dat, length_bin_interval) {
  seq(length_bin_interval,
    ncol(cal_dat[1, , ]) * length_bin_interval,
    length_bin_interval) - length_bin_interval / 2 # mid points
}

#' @export
#' @rdname tidy_caa
tidy_cal <- function(dat, yrs, unsorted_only = TRUE, interval = 1,
  sex = c(1, 2)) {
  length_bins <- seq(0, 1e4, interval)
  dat %>%
    select(-.data$age) %>%
    mutate(length_bin = length_bins[findInterval(.data$length, length_bins)]) %>%
    rename(age = .data$length_bin) %>% # hack
    tidy_caa(yrs = yrs, interval = interval, unsorted_only = unsorted_only,
      sex = sex)
}

#' Generate length-at-age or catch-at-age data
#'
#' @param dat Commercial or biological samples from [get_commercial_samples()] or
#'   [get_survey_samples()].
#' @param yrs A complete set of years to include in the matrix.
#' @param unsorted_only Logical for whether to only include the unsorted samples.
#'   Only applies to the commercial data.
#' @param interval Interval for the complete set of ages or lengths. For example,
#'   for length bins of interval 2, `interval = 2`.
#'
#' @return A catch at age or catch at length matrix as an array.
#'   1 x nyears x nage/nlength
#' \donttest{
#' d_commercial <- gfdata::get_commercial_samples(222)
#' cal <- tidy_cal(d_commercial, yrs = 2005:2010, interval = 5)
#' cal[1, , ]
#' get_cal_bins(cal, length_bin_interval = 5)
#'
#' d_survey <- gfdata::get_survey_samples(222, ssid = 1)
#' caa <- tidy_caa(d_survey, yrs = 2010:2012)
#' caa[1, , ]
#'
#' }
#' @export
tidy_caa <- function(dat, yrs, unsorted_only = FALSE, interval = 1,
  sex = c(1, 2)) {
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  if ("sampling_desc" %in% names(dat) && unsorted_only) {
    dat <- filter(dat, .data$sampling_desc == "UNSORTED")
  }
  dat <- filter(dat, !is.na(.data$age), .data$sex %in% sex)

  caa <- group_by(dat, .data$year, .data$age) %>%
    summarise(N = n()) %>%
    ungroup()

  caa <- left_join(
    expand.grid(age = seq(0, max(caa$age), interval), year = unique(caa$year)),
    caa,
    by = c("age", "year")
  )
  caa$N <- ifelse(is.na(caa$N), 0, caa$N)
  caa <- left_join(
    expand.grid(age = seq(0, max(caa$age), interval), year = yrs),
    caa,
    by = c("age", "year")
  )
  caa <- reshape2::dcast(caa, year ~ age, value.var = "N")[, -1L]
  array(as.numeric(as.matrix(caa)),
    dim = c(1L, nrow(caa), ncol(caa))
  ) # nsim x nyears x MaxAge/Length
}

extract_maturity_perc <- function(object) {
  m.p0.5 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.5)
  m.p0.95 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.95)
  m.p0.05 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.05)

  f.p0.5 <- logit_perc(a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.5)
  f.p0.95 <- logit_perc(a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.95)
  f.p0.05 <- logit_perc(a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.05)
  list(m.p0.5 = m.p0.5, m.p0.95 = m.p0.95, m.p0.05 = m.p0.05, f.p0.5 = f.p0.5,
    f.p0.95 = f.p0.95, f.p0.05 = f.p0.05)
}

delta_method <- function(g, mean, cov) {
  # simplified from msm::deltamethod
  cov <- as.matrix(cov)
  n <- length(mean)
  g <- list(g)
  syms <- paste0("x", seq_len(n))
  for (i in seq_len(n)) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(stats::deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  sqrt(diag(new.covar))
}

sd2cv <- function(.sd) {
  sqrt(exp(.sd^2) - 1)
}

logit_perc <- function(a, b, perc = 0.5) {
  -(log((1 / perc) - 1) + a) / b
}
