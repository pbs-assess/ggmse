#' Make all typical plots
#'
#' This function returns a large set of typical plots in a list object. These
#' can then be printed to a file or included in an R Markdown document.
#'
#' @param mse_list A list of DLMtool MSE objects representing different
#'   scenarios. The list should be named with the scenario names.
#' @param pm A character vector of performance metrics. These performance
#'   metrics should exist in the current workspace or via an attached package such as DLMtool.
#' @param scenario_df A data frame with the columns `scenario`,
#'   `scenario_human`, and `scenario_type`. `scenario_type` should contain
#'   `"Reference"` and `"Robustness"` entries.
#' @param mp_sat A character vector of satisficed management procedures
#'   (MPs).
#' @param mp_not_sat MPs that were *not* satisfied (a projection plot will be
#'   made with these) for `eg_scenario` (see below).
#' @param mp_not_sat_highlight MPs that were *not* satisfied to highlight
#'   in a projection plot for `eg_scenario` (see below). I.e. probably some subset of the full not satisfied set.
#' @param mp_ref Reference MPs.
#' @param custom_pal A named character vector of colors for the MPs.
#'   Names should correspond to the MP names. Should include all
#'   satisficed and reference MPs.
#' @param eg_scenario An example scenario (as character) which will be used for
#'   the projection plot of not-satisficed MPs.
#' @param tradeoff Character vector of length 2 of tradeoff PMs.
#' @param catch_breaks An optional numeric vector of y-axis breaks for the catch
#'   projection panels.
#' @param catch_labels An optional numeric vector of y-axis labels for the catch
#'   projection panels. This can be useful, for example, if you want the labels
#'   to be in 1000 t insead of t.
#' @param satisficed_criteria A named numeric vector designating the satisficed
#'   criteria for use in a 'tigure' plot. See [plot_tigure()].
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
#' custom_pal <- c(RColorBrewer::brewer.pal(3, "Set2"), "grey60")
#' names(custom_pal) <- c("CC100", ".Itarget1", ".Iratio2", "FMSYref75")
#'
#' plots <- plot_factory(
#'   mse,
#'   pm = pm,
#'   scenario_df = scenario_df,
#'   mp_sat = c(".Itarget1", ".Iratio2", "FMSYref75"),
#'   mp_not_sat = c("CC100"),
#'   mp_not_sat_highlight = c("CC100"),
#'   mp_ref = c("FMSYref75"),
#'   custom_pal = custom_pal,
#'   eg_scenario = "sc1",
#'   tradeoff = c("LT LRP", "STC"),
#'   satisficed_criteria = c("LT LRP" = 0.9, "STC" = 0.8)
#' )
#' names(plots)
#' plots$tigure_minimum
#' \donttest{
#' plots$convergence
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
                         mp_sat,
                         mp_not_sat,
                         mp_not_sat_highlight,
                         mp_ref,
                         custom_pal,
                         eg_scenario,
                         tradeoff,
                         catch_breaks = NULL,
                         catch_labels = catch_breaks,
                         satisficed_criteria = NULL) {
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
  if (is.null(names(custom_pal))) {
    stop("`custom_pal` must be a *named* character vector.", call. = FALSE)
  }
  if (!is.null(satisficed_criteria)) {
    if (is.null(names(satisficed_criteria))) {
      stop("`satisficed_criteria` must be a *named* character vector.", call. = FALSE)
    }
    if (!all(names(satisficed_criteria) %in% pm)) {
      stop("`names(satisficed_criteria)` not all in `pm`.", call. = FALSE)
    }
  }
  if (!all(mp_sat %in% names(custom_pal))) {
    stop("`custom_pal` must have names that include all of the satisficed MPs (`mp_sat`).",
      call. = FALSE
    )
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

  # pm_list <- purrr::map(mse_list, ~ get_probs(.x, pm))
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

  g$tigure_all_scenarios_avg <- gfdlm::plot_tigure(pm_avg)
  # .ggsave("pm-table-avg", 4.25, 6.5)
  g$tigure_minimum <- gfdlm::plot_tigure(pm_min,
    satisficed = satisficed_criteria,
  )
  # .ggsave("pm-table-min", 4.25, 6.5)

  g$tigure_refset <- map(pm_df_list, dplyr::filter, MP %in% mp_sat) %>%
    set_names(scenarios_ref_human) %>%
    plot_tigure_facet(ncol = 2)
  # .ggsave("pm-tigures-ref-set", 7, 6.5)

  g$tigure_robset <- map(pm_df_list_rob, dplyr::filter, MP %in% mp_sat) %>%
    set_names(scenarios_rob_human) %>%
    plot_tigure_facet()
  # .ggsave("pm-tigures-rob-set", 7, 2.25)

  # Convergence -----------------------------------------------------------------
  progress("convergence")

  g$convergence <- scenarios %>%
    purrr::map(~ DLMtool::Sub(mse_list[[.x]], MPs = mp_sat_with_ref)) %>%
    set_names(scenarios_human) %>%
    gfdlm::plot_convergence(pm, ylim = c(0.3, 1), custom_pal = custom_pal)
  # .ggsave("converge", 9, 10)

  # Projections -----------------------------------------------------------------
  progress("projection")

  g$projections <- map(names(mse_sat_with_ref), ~ {
    g <- plot_main_projections(mse_sat_with_ref[[.x]],
      catch_breaks = catch_breaks,
      catch_labels = catch_labels
    )
    # .ggsave(paste0("projections-satisficed-", .x), 7.5, 7.5)
  })
  names(g$projections) <- names(mse_sat_with_ref)

  # All not satisficed ones for "base":
  g$projections_not_sat <-
    DLMtool::Sub(mse_list[[eg_scenario]], MPs = mp_not_sat) %>%
    plot_main_projections(
      catch_breaks = catch_breaks,
      catch_labels = catch_labels
    )
  # .ggsave(paste0("projections-all-not-satisficed"), 7.5, 27)

  # Example not satisficed ones for "base":
  # mp_eg_not_sat <- c(
  #   "CC_hist",
  #   "CC90",
  #   ".GB_slope8_0.66",
  #   ".Islope0.2_80",
  #   ".ICI2",
  #   ".IDX_smooth",
  #   ".IT5_hist",
  #   ".ITM_hist",
  #   ".SP6040_prior"
  # )
  mp_eg_not_sat <- mp_not_sat_highlight[mp_not_sat_highlight %in% mp_not_sat]
  g$projections_not_sat_highlight <-
    DLMtool::Sub(mse_list[[eg_scenario]], MPs = mp_eg_not_sat) %>%
    plot_main_projections(
      catch_breaks = catch_breaks,
      catch_labels = catch_labels
    )
  # .ggsave(paste0("projections-eg-not-satisficed"), 8, 9.5)

  # Kobe ------------------------------------------------------------------------
  progress("Kobe")

  MPs <- union(mp_sat, mp_ref[mp_ref != "NFref"])

  g$kobe <-
    purrr::map(scenarios_ref, ~ DLMtool::Sub(mse_list[[.x]], MPs = MPs)) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_kobe_grid()
  # .ggsave("kobe-grid-satisficed", 9.5, 10.5)

  # Radar plots -----------------------------------------------------------------
  progress("radar")

  g$radar_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_ref_human) %>%
    plot_radar_facet(custom_pal = custom_pal)
  # .ggsave("spider-satisficed-panel-reference", 12, 11)

  g$radar_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_rob_human) %>%
    plot_radar_facet(custom_pal = custom_pal)
  # .ggsave("spider-satisficed-panel-robustness", 10, 5)

  g$radar_refset_avg <- pm_avg %>%
    dplyr::filter(MP %in% MPs) %>%
    plot_radar(custom_pal = custom_pal)
  # .ggsave("spider-satisficed-avg-reference", 6, 6)

  g$radar_refset_min <- pm_min %>%
    dplyr::filter(MP %in% MPs) %>%
    plot_radar(custom_pal = custom_pal)
  # .ggsave("spider-satisficed-min-reference", 6, 6)

  # d <- pm_avg %>%
  #   dplyr::inner_join(rename(mp, MP = mp), by = "MP") %>%
  #   split(.$type) %>%
  #   purrr::map(dplyr::select, -type)
  # g_temp <- d %>% purrr::map(plot_radar)
  # g$radar_refset_mptypes_avg <-
  #   cowplot::plot_grid(
  #     plotlist = g_temp, ncol = 2, labels = names(d),
  #     hjust = 0, label_size = 11, align = "hv"
  #   )
  # .ggsave("spider-all-avg-reference", 10, 10)

  # Dot plots -----------------------------------------------------------------
  progress("dot")
  g$dot_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_dots(type = "facet", custom_pal = custom_pal)

  g$dot_refset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_rob_human) %>%
    gfdlm::plot_dots(type = "facet", custom_pal = custom_pal)

  g$dot_refset_avg <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    gfdlm::plot_dots(type = "single", custom_pal = custom_pal)

  # Parallel coordinate plots -------------------------------------------------
  progress("parallel coordinate")

  # pm_groups <- list(c("LT LRP", "LT USR", "FMSY"), c("STC", "LTC", "AAVC"))

  g$parallel_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_parallel_coords(type = "facet", custom_pal = custom_pal)
  # .ggsave("parallel-coordinates", 8, 6.6)

  # g <- pm_df_list %>% map(dplyr::filter, MP %in% MPs) %>%
  #   set_names(scenarios_ref_human) %>%
  #   gfdlm::plot_parallel_coords(type = "facet", custom_pal = custom_pal,
  #     groups = pm_groups)
  # # .ggsave("parallel-coordinates-grouped", 8, 6.6)

  g$parallel_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_rob_human) %>%
    gfdlm::plot_parallel_coords(type = "facet", custom_pal = custom_pal)
  # .ggsave("parallel-coordinates-rob", 7, 3)

  g$parallel_refset_avg <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    gfdlm::plot_parallel_coords(type = "single", custom_pal = custom_pal)
  # .ggsave("parallel-coordinates-avg", 5.5, 3.5)

  # g$parallel_coords_average_grouped <- pm_df_list %>% map(dplyr::filter, MP %in% MPs) %>%
  #   gfdlm::plot_parallel_coords(type = "single", custom_pal = custom_pal,
  #     groups = pm_groups)
  # .ggsave("parallel-coordinates-avg-grouped", 5.5, 3.5)

  # FIXME: pull this into package:
  # d <- pm_avg %>%
  #   dplyr::inner_join(rename(mp, MP = mp), by = "MP") %>%
  #   split(.$type) %>%
  #   purrr::map(select, -type)
  # suppressMessages({
  #   g_temp <- names(d) %>% map(~ {
  #     gfdlm::plot_parallel_coords(d[.x], type = "single", rotate_labels = TRUE) +
  #       ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 11)) +
  #       ggplot2::guides(lty = FALSE, fill = FALSE) +
  #       ggplot2::scale_color_brewer(palette = "Set2") +
  #       ggplot2::coord_cartesian(ylim = c(-0.01, 1.01), expand = FALSE) +
  #       ggplot2::theme(plot.margin = grid::unit(c(1, .5, .5, .5), "lines")) +
  #       ggplot2::theme(
  #         panel.grid.major.y = ggplot2::element_line(colour = "grey85"),
  #         panel.grid.major.x = ggplot2::element_line(colour = "grey85"),
  #         panel.grid.minor.y = ggplot2::element_line(colour = "grey96")
  #       )
  #   })
  # })
  # g$parallel_refset_mptypes_avg <-
  #   cowplot::plot_grid(
  #     plotlist = g_temp, ncol = 2, labels = names(d),
  #     hjust = 0, label_size = 11, vjust = 1, align = "hv"
  #   ) +
  #   ggplot2::theme(plot.margin = grid::unit(c(1, 0, 1, 1), "lines"))
  # .ggsave("parallel-coordinates-all-avg-reference", 8.5, 8.5)

  # Lollipops -------------------------------------------------------------------
  progress("lollipop")

  g$lollipops_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% MPs) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_lollipop(custom_pal = custom_pal, dodge = 0.65)
  # .ggsave("lollipops-ref", 8, 7)

  g$lollipops_refset_avg <- pm_avg %>%
    dplyr::filter(MP %in% MPs) %>%
    gfdlm::plot_lollipop(custom_pal = custom_pal)
  # .ggsave("lollipops-ref-avg", 4.5, 5)

  g$lollipops_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% MPs) %>%
    gfdlm::plot_lollipop(custom_pal = custom_pal, dodge = 0.65)

  # Bivariate trade-off plots ---------------------------------------------------
  progress("bivariate trade-off")

  g$tradeoff_refset <- pm_df_list %>%
    map(dplyr::filter, MP %in% union(mp_sat, mp_ref[mp_ref != "NFref"])) %>%
    set_names(scenarios_ref_human) %>%
    gfdlm::plot_tradeoff(tradeoff[1], tradeoff[2], custom_pal = custom_pal)
  # .ggsave("bivariate-trade-off-reference", 7.5, 6.5)

  g$tradeoff_robset <- pm_df_list_rob %>%
    map(dplyr::filter, MP %in% union(mp_sat, mp_ref[mp_ref != "NFref"])) %>%
    set_names(scenarios_rob_human) %>%
    gfdlm::plot_tradeoff(tradeoff[1], tradeoff[2], custom_pal = custom_pal) +
    facet_wrap(~scenario, ncol = 2)
  # .ggsave("bivariate-trade-off-robustness", 6, 3)

  # Psychedelic pyramid worms ---------------------------------------------------
  progress(paste("psychedelic worm", clisymbols::symbol$mustache))

  MPs <- union(mp_sat, mp_ref[mp_ref != "NFref"])
  d <- purrr::map(scenarios_ref, ~ DLMtool::Sub(mse_list[[.x]], MPs = MPs)) %>%
    set_names(scenarios_ref_human)

  suppressMessages({
    g$worms_proj <-
      d %>% plot_worms_grid(include_historical = FALSE) +
      coord_fixed(xlim = c(0, 2.5), ylim = c(0, 2), expand = FALSE) +
      scale_x_continuous(breaks = c(0, 1, 2)) +
      scale_y_continuous(breaks = c(0, 1))
    # .ggsave("neon-worms-projection", 10, 8.5)

    g$worms_hist_proj <-
      d %>% plot_worms_grid(include_historical = TRUE) +
      coord_fixed(xlim = c(0, 3), ylim = c(0, 3), expand = FALSE)
    # .ggsave("neon-worms-all", 10, 8.5)
  })

  # Sensitivity plots -----------------------------------------------------------

  # slots <- c("D", "hs", "M", "ageM", "L50", "Linf", "K", "Isd")
  #
  # g$sensitivity_dots <- DLMtool::Sub(mse_list[[eg_scenario]], MPs = mp_sat) %>%
  #   gfdlm::plot_sensitivity(`LT LRP`, slots = slots,
  #     ylab = expression(Mean~B/B[MSY]~"in"~years~36-50))
  # # .ggsave("sensitivity-bbmsy-base", 12.5, 5)
  #
  # g$sensitivity_ <- DLMtool::Sub(mse_list[[eg_scenario]], MPs = mp_sat) %>%
  #   gfdlm::plot_sensitivity(`STY`, slots = slots,
  #     ylab = "Mean catch/reference catch in years 6-20")
  # # .ggsave("sensitivity-yield-base", 12.5, 5)
  #
  # g$sensitivity_ <- DLMtool::Sub(mse_list[[eg_scenario]], MPs = mp_sat) %>%
  #   gfdlm::plot_sensitivity_trajectory("B_BMSY", slots = slots) +
  #   coord_cartesian(ylim = c(0, 4))
  # # .ggsave("sensitivity-traj-bbmsy-base", 12.5, 5)
  #
  # g$sensitivity_ <- DLMtool::Sub(mse_list[[eg_scenario]], MPs = mp_sat) %>%
  #   gfdlm::plot_sensitivity_trajectory("F_FMSY", slots = slots) +
  #   coord_cartesian(ylim = c(0, 4))
  # # .ggsave("sensitivity-traj-ffmsy-base", 12.5, 5)

  g
}

progress <- function(text, before = "Creating", after = "figures") {
  cat(
    crayon::green(clisymbols::symbol$tick),
    before, text, after, "\n"
  )
}
