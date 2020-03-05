#' Make all typical plots
#'
#' This function returns a large set of typical plots in a list object. These
#' can then be printed to a file or included in an R Markdown document.
#'
#' @param mse_list A list of DLMtool MSE objects representing different
#'   scenarios. The list should be named with the scenario names.
#' @param pm A character vector of performance metrics. These performance
#'   metrics should exist in the current workspace or via an attached package
#'   such as DLMtool.
#' @param scenario_df A data frame with the columns `scenario`,
#'   `scenario_human`, and `scenario_type`. `scenario_type` should contain
#'   `"Reference"` and `"Robustness"` entries.
#' @param mp_ref Reference MPs.
#' @param mp_sat A character vector of satisficed management procedures (MPs).
#' @param mp_not_sat MPs that were *not* satisfied (a giant projection plot will
#'   be made with these) for `eg_scenario` (see below).
#' @param mp_not_sat2 MPs that were *not* satisfied to highlight in a projection
#'   plot for `eg_scenario` (see below). I.e. probably some subset of the full
#'   not satisfied set.
#' @param custom_pal A named character vector of colors for the MPs. Names
#'   should correspond to the MP names. Should include all satisficed and
#'   reference MPs.
#' @param eg_scenario An example scenario (as character) that will be used for
#'   the projection plot of not-satisficed MPs.
#' @param tradeoff Character vector of length 2 of tradeoff PMs.
#' @param catch_breaks An optional numeric vector of y-axis breaks for the catch
#'   projection panels.
#' @param catch_labels An optional numeric vector of y-axis labels for the catch
#'   projection panels. This can be useful, for example, if you want the labels
#'   to be in 1000 t insead of t.
#' @param catch_ylim Optional y-axis limits for catch, e.g. c(0, 100)
#' @param dodge The dodge width for [plot_dots()] etc.
#' @param satisficed_criteria A named numeric vector designating the satisficed
#'   criteria for use in a 'tigure' plot. See [plot_tigure()].
#' @param skip_projections Logical: skip the projection and worm plots for speed?
#' @param omit_index_fn A function that indexes years in the projection period to
#'   omit from the plot. See [plot_index()].
#' @param survey_type Which survey to plot. Passed to [plot_index()].
#'
#' @return A named list object containing the ggplot objects.
#' @importFrom purrr set_names
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous
#'
#' @export
#' @examples
#' # Fake but fast example follows:
#' # In reality, you might get here with something like:
#' # mse <- lapply(om_list, runMSE, MPs = mps)
#' # or
#' # mse <- purrr::map(om_list, runMSE, MPs = mps)
#' library(DLMtool)
#' # Instead, let's use the same example thrice:
#' mse <- list()
#' mse[[1]] <- mse_example
#' mse[[2]] <- mse_example
#' mse[[3]] <- mse_example
#' names(mse) <- c("sc1", "sc2", "sc3")
#'
#' # Use more meaningful names than this:
#' scenario_df <- tibble::tribble(
#'   ~scenario, ~scenario_human, ~scenario_type,
#'   "sc1", "Scenario 1", "Reference",
#'   "sc2", "Scenario 2", "Reference",
#'   "sc3", "Scenario 3", "Robustness"
#' )
#'
#' `LT LRP` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
#' `LT USR` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
#' STC <- gfdlm::pm_factory("LTY", 0.5, c(1, 10))
#' LTC <- gfdlm::pm_factory("LTY", 0.5, c(36, 50))
#' pm <- c("LT LRP", "LT USR", "STC", "LTC")
#'
#' custom_pal <- c(RColorBrewer::brewer.pal(3, "Dark2"), "grey60")
#' names(custom_pal) <- c("CC1.0", "Itarget1", "Iratio2", "FMSYref75")
#'
#' plots <- plot_factory(
#'   mse,
#'   pm = pm,
#'   scenario_df = scenario_df,
#'   mp_sat = c("Itarget1", "Iratio2", "FMSYref75"),
#'   mp_ref = c("FMSYref75"),
#'   mp_not_sat = c("CC1.0"),
#'   custom_pal = custom_pal,
#'   eg_scenario = "sc1",
#'   tradeoff = c("LT LRP", "STC"),
#'   satisficed_criteria = c("LT LRP" = 0.9, "STC" = 0.8)
#' )
#' names(plots)
#' plots$tigure_minimum
#' \donttest{
#' plots$convergence
#' plots$tigure_refset_avg
#' plots$tigure_refset
#' plots$worms_proj
#' plots$parallel_refset
#' plots$dot_refset
#' plots$radar_refset
#' plots$lollipops_refset
#' plots$projections$sc1
#' plots$projections_not_sat
#' }
plot_factory <- function(
                         mse_list,
                         pm,
                         scenario_df,
                         mp_ref,
                         mp_sat,
                         mp_not_sat,
                         mp_not_sat2 = mp_not_sat,
                         custom_pal = NULL,
                         eg_scenario = scenario_df$scenario[1],
                         tradeoff = pm[1:2],
                         catch_breaks = NULL,
                         catch_labels = catch_breaks, catch_ylim = NULL,
                         dodge = 0.8,
                         satisficed_criteria = NULL,
                         skip_projections = FALSE,
                         omit_index_fn = function(x) NULL,
                         survey_type = c("Ind", "AddInd")) {
  survey_type <- match.arg(survey_type)
  if (!is.list(mse_list)) {
    stop("`mse_list` must be a list.", call. = FALSE)
  }
  if (!all(vapply(mse_list, class, FUN.VALUE = character(1L)) == "MSE")) {
    stop("`mse_list` must contain DLMtool MSE objects.", call. = FALSE)
  }
  if (!is.data.frame(scenario_df)) {
    stop("`scenario_df` must be a data frame.", call. = FALSE)
  }
  if (!all(c("scenario", "scenario_human", "scenario_type")
  %in% colnames(scenario_df))) {
    stop("`scenario_df` must have columns `c(\"scenario\", \"scenario_human\", \"scenario_type\")`",
      call. = FALSE
    )
  }
  if (!is.null(custom_pal)) {
    if (is.null(names(custom_pal))) {
      stop("`custom_pal` must be a *named* character vector.", call. = FALSE)
    }
    if (!all(mp_sat %in% names(custom_pal))) {
      stop("`custom_pal` must have names that include all of the satisficed MPs (`mp_sat`).",
        call. = FALSE
      )
    }
  }
  if (!is.null(satisficed_criteria)) {
    if (is.null(names(satisficed_criteria))) {
      stop("`satisficed_criteria` must be a *named* character vector.", call. = FALSE)
    }
    if (!all(names(satisficed_criteria) %in% pm)) {
      stop("`names(satisficed_criteria)` not all in `pm`.", call. = FALSE)
    }
  }
  if (!eg_scenario %in% scenario_df$scenario) {
    stop("`eg_scenario` must be in `scenario_df$scenario`.", call. = FALSE)
  }
  if (!all(scenario_df$scenario %in% names(mse_list))) {
    stop("Not all `scenario_df$scenario` in `names(mse_list)`.", call. = FALSE)
  }
  if (!all(names(mse_list) %in% scenario_df$scenario)) {
    stop("Not all `names(mse_list)` in `scenario_df$scenario`.", call. = FALSE)
  }

  progress(
    before = "Calculating performance metrics",
    after = "", text = ""
  )
  get_filtered_scenario <- function(type, column) {
    dplyr::filter(sc, scenario_type == type) %>%
      dplyr::pull(!!column) %>%
      purrr::set_names()
  }
  sc <- scenario_df
  scenarios <- sc$scenario %>% set_names()
  scenarios_human <- sc$scenario_human %>% set_names()
  scenarios_ref <- get_filtered_scenario("Reference", "scenario")
  scenarios_ref_human <- get_filtered_scenario("Reference", "scenario_human")
  scenarios_rob <- get_filtered_scenario("Robustness", "scenario")
  scenarios_rob_human <- get_filtered_scenario("Robustness", "scenario_human")

  pm_df_list <- purrr::map(mse_list[scenarios_ref], ~ get_probs(.x, pm))
  pm_df_list_rob <- purrr::map(mse_list[scenarios_rob], ~ get_probs(.x, pm))

  pm_df <- dplyr::bind_rows(pm_df_list, .id = "scenario")
  pm_avg <- dplyr::group_by(pm_df, MP) %>% dplyr::summarise_if(is.numeric, mean)
  pm_min <- dplyr::group_by(pm_df, MP) %>% dplyr::summarise_if(is.numeric, min)

  mse_sat <- purrr::map(scenarios, ~ DLMtool::Sub(mse_list[[.x]], MPs = mp_sat))
  mp_sat_with_ref <- union(mp_sat, mp_ref)
  mse_sat_with_ref <-
    purrr::map(scenarios_ref, ~ DLMtool::Sub(mse_list[[.x]], MPs = mp_sat_with_ref))

  g <- list()

  g$tigure_refset_avg <- gfdlm::plot_tigure(pm_avg,
    satisficed = satisficed_criteria,
  )
  g$tigure_refset_min <- gfdlm::plot_tigure(pm_min,
    satisficed = satisficed_criteria,
  )

  g$tigure_refset <- map(pm_df_list, dplyr::filter, MP %in% mp_sat) %>%
    set_names(scenarios_ref_human) %>%
    plot_tigure_facet(ncol = 2)

  g$tigure_robset <- map(pm_df_list_rob, dplyr::filter, MP %in% mp_sat) %>%
    set_names(scenarios_rob_human) %>%
    plot_tigure_facet()

  # Convergence ---------------------------------------------------------------

  progress("convergence")

  g$convergence <- scenarios %>%
    purrr::map(~ DLMtool::Sub(mse_list[[.x]], MPs = mp_sat_with_ref)) %>%
    set_names(scenarios_human) %>%
    gfdlm::plot_convergence(pm, ylim = c(0.3, 1), custom_pal = custom_pal)

  # Projections ---------------------------------------------------------------

  if (!skip_projections) {
    progress("projection")

    # All scenarios:
    xx <- map(scenarios, ~ DLMtool::Sub(mse_list[[.x]], MPs = mp_sat_with_ref)) %>%
      set_names(scenarios_human)
    g$projections <- map(names(xx), ~ {
      g <- plot_main_projections(xx[[.x]],
        catch_breaks = catch_breaks,
        catch_labels = catch_labels, catch_ylim = catch_ylim
      )
    })
    names(g$projections) <- names(scenarios)

    # All not satisficed ones for "example scenario":
    g$projections_not_sat <-
      DLMtool::Sub(mse_list[[eg_scenario]], MPs = mp_not_sat) %>%
      plot_main_projections(
        catch_breaks = catch_breaks,
        catch_labels = catch_labels, catch_ylim = catch_ylim
      )

    # Highlighted not satisficed ones:
    mp_eg_not_sat <- mp_not_sat2[mp_not_sat2 %in% mp_not_sat]
    g$projections_not_sat2 <-
      DLMtool::Sub(mse_list[[eg_scenario]], MPs = mp_eg_not_sat) %>%
      plot_main_projections(
        catch_breaks = catch_breaks,
        catch_labels = catch_labels, catch_ylim = catch_ylim
      )

    # Scenario projections ----------------------------------------------------

    progress("combined-scenario projection")

    g$projections_scenarios <- map(
      scenarios,
      ~ DLMtool::Sub(mse_list[[.x]], MPs = mp_sat_with_ref)
    ) %>%
      set_names(scenarios_human) %>%
      plot_scenario_projections(
        catch_breaks = catch_breaks,
        catch_labels = catch_labels, catch_ylim = catch_ylim
      )

    g$projections_scenarios_ref <- map(
      scenarios_ref,
      ~ DLMtool::Sub(mse_list[[.x]], MPs = mp_sat_with_ref)
    ) %>%
      set_names(scenarios_ref_human) %>%
      plot_scenario_projections(
        catch_breaks = catch_breaks,
        catch_labels = catch_labels
      )

    g$projections_scenarios_rob <- map(
      scenarios_rob,
      ~ DLMtool::Sub(mse_list[[.x]], MPs = mp_sat_with_ref)
    ) %>%
      set_names(scenarios_rob_human) %>%
      plot_scenario_projections(
        catch_breaks = catch_breaks,
        catch_labels = catch_labels
      )

    # Index projections -------------------------------------------------------

    g$projections_index <- map(
      scenarios, ~ {
        temp <- mse_list[[.x]] # https://github.com/DLMtool/DLMtool/issues/295
        # temp@Misc$Data <- temp@Misc$Data[match(mp_sat_with_ref, temp@MPs)]
        DLMtool::Sub(temp, MPs = mp_sat_with_ref)
      }
    ) %>%
      set_names(scenarios_human) %>%
      plot_index(type = survey_type, omit_index_fn = omit_index_fn)
  } else {
    progress(text = "", before = "Skipping the projection figures.", after = "")
  }
  # Kobe ----------------------------------------------------------------------

  progress("Kobe")

  MPs <- union(mp_sat, mp_ref[mp_ref != "NFref"])

  g$kobe_ref <-
    purrr::map(scenarios_ref, ~ DLMtool::Sub(mse_list[[.x]], MPs = MPs)) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_kobe_grid()

  g$kobe_rob <-
    purrr::map(scenarios_rob, ~ DLMtool::Sub(mse_list[[.x]], MPs = MPs)) %>%
    set_names(scenarios_rob_human) %>%
    gfdlm::plot_kobe_grid()

  g$kobe <-
    purrr::map(scenarios, ~ DLMtool::Sub(mse_list[[.x]], MPs = MPs)) %>%
    set_names(scenarios_human) %>%
    gfdlm::plot_kobe_grid()


  # Radar plots ---------------------------------------------------------------

  progress("radar")

  MPs <- union(mp_sat, mp_ref)

  g$radar_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_ref_human) %>%
    plot_radar_facet(custom_pal = custom_pal)

  g$radar_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_rob_human) %>%
    plot_radar_facet(custom_pal = custom_pal)

  g$radar_refset_avg <- pm_avg %>%
    dplyr::filter(MP %in% MPs) %>%
    plot_radar(custom_pal = custom_pal)

  g$radar_refset_min <- pm_min %>%
    dplyr::filter(MP %in% MPs) %>%
    plot_radar(custom_pal = custom_pal)

  # Dot plots -----------------------------------------------------------------

  progress("dot")

  g$dot_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_dots(type = "facet", custom_pal = custom_pal, dodge = dodge)

  g$dot_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_rob_human) %>%
    gfdlm::plot_dots(type = "facet", custom_pal = custom_pal, dodge = dodge)

  g$dot_refset_avg <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    gfdlm::plot_dots(type = "single", custom_pal = custom_pal, dodge = dodge)

  # Parallel coordinate plots -------------------------------------------------

  progress("parallel coordinate")

  g$parallel_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_parallel_coords(type = "facet", custom_pal = custom_pal)

  g$parallel_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_rob_human) %>%
    gfdlm::plot_parallel_coords(type = "facet", custom_pal = custom_pal)

  g$parallel_refset_avg <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    gfdlm::plot_parallel_coords(type = "single", custom_pal = custom_pal)

  # Lollipops -----------------------------------------------------------------

  progress("lollipop")

  g$lollipops_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_lollipop(custom_pal = custom_pal, dodge = dodge)

  g$lollipops_refset_avg <- pm_avg %>%
    dplyr::filter(MP %in% MPs) %>%
    gfdlm::plot_lollipop(custom_pal = custom_pal, dodge = dodge)

  g$lollipops_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% MPs) %>%
    gfdlm::plot_lollipop(custom_pal = custom_pal, dodge = dodge)

  # Bivariate trade-off plots -------------------------------------------------

  progress("bivariate trade-off")

  g$tradeoff_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% union(mp_sat, mp_ref[mp_ref != "NFref"])) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_tradeoff(tradeoff[1], tradeoff[2], custom_pal = custom_pal)

  g$tradeoff_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% union(mp_sat, mp_ref[mp_ref != "NFref"])) %>%
    set_names(scenarios_rob_human) %>%
    gfdlm::plot_tradeoff(tradeoff[1], tradeoff[2], custom_pal = custom_pal) +
    facet_wrap(~scenario, ncol = 2)

  # Psychedelic pyramid worms -------------------------------------------------

  if (!skip_projections) {
    progress(paste("psychedelic worm", clisymbols::symbol$mustache))

    MPs <- union(mp_sat, mp_ref[mp_ref != "NFref"])

    suppressMessages({
      d <- purrr::map(scenarios, ~ DLMtool::Sub(mse_list[[.x]], MPs = MPs)) %>%
        set_names(scenarios_human)
      g$worms_proj <-
        d %>% plot_worms_grid(include_historical = FALSE) +
        coord_fixed(xlim = c(0, 2.5), ylim = c(0, 2), expand = FALSE) +
        scale_x_continuous(breaks = c(0, 1, 2)) +
        scale_y_continuous(breaks = c(0, 1))
      g$worms_hist_proj <-
        d %>% plot_worms_grid(include_historical = TRUE) +
        coord_fixed(xlim = c(0, 3), ylim = c(0, 3), expand = FALSE)

      d <- purrr::map(scenarios_ref, ~ DLMtool::Sub(mse_list[[.x]], MPs = MPs)) %>%
        set_names(scenarios_ref_human)
      g$worms_proj_ref <-
        d %>% plot_worms_grid(include_historical = FALSE) +
        coord_fixed(xlim = c(0, 2.5), ylim = c(0, 2), expand = FALSE) +
        scale_x_continuous(breaks = c(0, 1, 2)) +
        scale_y_continuous(breaks = c(0, 1))
      g$worms_hist_proj_ref <-
        d %>% plot_worms_grid(include_historical = TRUE) +
        coord_fixed(xlim = c(0, 3), ylim = c(0, 3), expand = FALSE)

      d <- purrr::map(scenarios_rob, ~ DLMtool::Sub(mse_list[[.x]], MPs = MPs)) %>%
        set_names(scenarios_rob_human)
      g$worms_proj_rob <-
        d %>% plot_worms_grid(include_historical = FALSE) +
        coord_fixed(xlim = c(0, 2.5), ylim = c(0, 2), expand = FALSE) +
        scale_x_continuous(breaks = c(0, 1, 2)) +
        scale_y_continuous(breaks = c(0, 1))
      g$worms_hist_proj_rob <-
        d %>% plot_worms_grid(include_historical = TRUE) +
        coord_fixed(xlim = c(0, 3), ylim = c(0, 3), expand = FALSE)

    })
  } else {
    progress(text = "", before = paste0(
      "Skipping the psychedelic worm ",
      clisymbols::symbol$mustache, " figures."
    ), after = "")
  }

  g
}

progress <- function(text, before = "Creating", after = "figures") {
  cat(
    crayon::green(clisymbols::symbol$tick),
    before, text, after, "\n"
  )
}
