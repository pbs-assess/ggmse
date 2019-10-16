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
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length)) # RF removed female filtering #female only, .data$sex %in% 2
  group_by(dat, .data$year) %>%
    dplyr::summarise(n = dplyr::n(), mean_length = mean(.data$length)) %>%
    ungroup()
}

get_cal_bins <- function(cal_dat, length_bin_interval) {
  seq(
    length_bin_interval,
    ncol(cal_dat[1, , ]) * length_bin_interval,
    length_bin_interval
  ) - length_bin_interval / 2 # mid points
}

#' @param sex 1 or 2 for male or female.
#' @export
#' @rdname tidy_caa
tidy_cal <- function(dat, yrs, unsorted_only = TRUE, interval = 1,
  sex = c(1, 2)) {
  length_bins <- seq(0, 1e4, interval)
  dat %>%
    select(-.data$age) %>%
    mutate(length_bin = length_bins[findInterval(.data$length, length_bins)]) %>%
    rename(age = .data$length_bin) %>% # hack
    tidy_caa(
      yrs = yrs, interval = interval, unsorted_only = unsorted_only,
      sex = sex
    )
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
#' @param sex Numeric vector indicating males and/or or females.
#'
#' @return A catch at age or catch at length matrix as an array.
#'   1 x nyears x nage/nlength
#' @examples
#' \donttest{
#' d_commercial <- gfdata::get_commercial_samples(222)
#' cal <- tidy_cal(d_commercial, yrs = 2005:2010, interval = 5)
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
    dplyr::summarise(N = dplyr::n()) %>%
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

  f.p0.5 <- logit_perc(
    a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.5
  )
  f.p0.95 <- logit_perc(
    a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.95
  )
  f.p0.05 <- logit_perc(
    a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.05
  )
  list(
    m.p0.5 = m.p0.5, m.p0.95 = m.p0.95, m.p0.05 = m.p0.05, f.p0.5 = f.p0.5,
    f.p0.95 = f.p0.95, f.p0.05 = f.p0.05
  )
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
