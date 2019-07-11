#' Factory to create loading functions for various species
#'
#' @param species_name The species name as can be used in extraction with [gfdata::get_commercial_samples]
#'  and similar functions
#' @param file The filename to load from
#'
#' @return A function which can be used to load data automatically
#' @importFrom gfdata get_catch get_commercial_samples get_survey_samples get_survey_index
#' @export
#'
#' @examples
#' load_data_pcod <- load_data_factory("pacific cod",
#'   file = here::here("generated-data",
#'                     "pacific-cod.rds"))
load_data_factory <- function(species_name, file) {
  created_by_load_data_factory <- function(unsorted_only = TRUE, private = FALSE) {
    if (!file.exists(file)) {
      d <- list()
      d$commercial_samples <- gfdata::get_commercial_samples(species_name,
        unsorted_only = unsorted_only
      )
      d$catch <- gfdata::get_catch(species_name)
      d$survey_samples <- gfdata::get_survey_samples(species_name)
      d$survey_index <- gfdata::get_survey_index(species_name)
      saveRDS(d, file = file)
    } else {
      d <- readRDS(file)
    }
    if (private) {
      d$commercial_samples <- NULL
      d$catch <- NULL
    }
    d
  }
  created_by_load_data_factory
}

load_data_pcod <- load_data_factory("pacific cod",
  file = here::here(
    "generated-data",
    "pacific-cod.rds"
  )
)

load_data_shortraker <- load_data_factory("shortraker rockfish",
  file = here::here(
    "generated-data",
    "shortraker-rockfish.rds"
  )
)

load_data_rex <- load_data_factory("rex sole",
  file = here::here(
    "generated-data",
    "rex-sole.rds"
  )
)

#' Filter Rex sole data
#'
#' @param d data as retrieved from [gfdata::get_commercial_samples()] and similar functions
#' @param minimum_year Minimum year for filtering
#' @param maximum_year Maximum year for filtering
#' @param major_stat_area_codes vector of major stat area codes
#' @param survey_series_descs vector of survey descriptions as they appear in the survey_series_desc
#'  field of the survey_index item of the call to [gfdata::get_survey_index()]
#'
#' @return The filtered data
#' @importFrom dplyr mutate filter %>%
#' @importFrom lubridate year
#' @export
filter_data_rex <- function(d,
                            minimum_year = 1996,
                            maximum_year = 2018,
                            major_stat_area_codes = c("03", "04"),
                            survey_series_descs = c("West Coast Vancouver Island Synoptic Bottom Trawl")) {
  d$commercial_samples <- d$commercial_samples %>%
    dplyr::filter(major_stat_area_code %in% major_stat_area_codes) %>%
    dplyr::mutate(year = lubridate::year(trip_start_date)) %>%
    dplyr::filter(year <= maximum_year, year >= minimum_year)

  d$survey_samples <- d$survey_samples %>%
    dplyr::filter(major_stat_area_code %in% major_stat_area_codes) %>%
    dplyr::mutate(year = lubridate::year(trip_start_date)) %>%
    dplyr::filter(year <= maximum_year, year >= minimum_year)

  d$catch <- d$catch %>%
    dplyr::filter(major_stat_area_code %in% major_stat_area_codes) %>%
    dplyr::mutate(year = lubridate::year(fe_start_date)) %>%
    dplyr::filter(year <= maximum_year, year >= minimum_year)

  d$survey_index <- d$survey_index %>%
    dplyr::filter(survey_series_desc %in% survey_series_descs) %>%
    dplyr::filter(year <= maximum_year, year >= minimum_year)

  d
}
