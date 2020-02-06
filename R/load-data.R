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
#' \dontrun{
#' load_data_pcod <- load_data_factory("pacific cod",
#'   file = here::here(
#'     "generated-data",
#'     "pacific-cod.rds"
#'   )
#' )
#' }
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

# #' Loads Pacific Cod data
# #'
# #' @param unsorted_only If TRUE, return only unsorted records in the commercial samples
# #' @param private If TRUE, do not include commercial samples and catch in the returned data
# #'
# #' @return A list with the outputs of [gfdata::get_catch()], [gfdata::get_survey_samples()]# , [gfdata::get_survey_index()], and [gfdata::get_commercial_samples()].
# #' @importFrom here here
# #' @export
# load_data_pcod <- load_data_factory("pacific cod",
#   file = here("generated-data", "pacific-cod.rds")
# )
#
# #' Loads Shortraker Rockfish data
# #'
# #' @param unsorted_only If TRUE, return only unsorted records in the commercial samples
# #' @param private If TRUE, do not include commercial samples and catch in the returned data
# #'
# #' @return A list with the outputs of [gfdata::get_catch()], [gfdata::get_survey_samples()]# , [gfdata::get_survey_index()], and [gfdata::get_commercial_samples()].
# #' @export
# load_data_shortraker <- load_data_factory("shortraker rockfish",
#   file = here("generated-data", "shortraker-rockfish.rds")
# )
#
# #' Loads Rex Sole data
# #'
# #' @param unsorted_only If TRUE, return only unsorted records in the commercial samples
# #' @param private If TRUE, do not include commercial samples and catch in the returned data
# #'
# #' @return A list with the outputs of [gfdata::get_catch()], [gfdata::get_survey_samples()]# , [gfdata::get_survey_index()], and [gfdata::get_commercial_samples()].
# #' @export
# load_data_rex <- load_data_factory("rex sole",
#   file = here("generated-data", "rex-sole.rds")
# )