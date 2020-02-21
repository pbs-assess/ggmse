#' Plot index fits from a stock reduction analysis
#'
#' @param sra_list A named a list of objects from [MSEtool::SRA_scope()].
#' @param survey_names A character vector of real index names. Columns will be
#'   ordered by the order of the survey names.
#' @param sample_n The number of SRA samples to plot.
#' @param alpha Transparency
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom tibble tibble
#' @importFrom ggplot2 vars
#'
#' @examples
#' library(MSEtool)
#' om <- DLMtool::testOM
#' om@nsim <- 10
#' set.seed(1)
#' m <- SRA_scope(om,
#'   data = list(Chist = runif(10), Index = runif(10), I_sd = rep(0.1, 10))
#' )
#' fits <- list()
#' fits[[1]] <- m
#' fits[[2]] <- m
#' names(fits) <- c("Sc 1", "Sc 2")
#' plot_index_fits(fits, survey_names = c("a"), alpha = 0.3)
plot_index_fits <- function(sra_list, survey_names,
                           sample_n = min(c(100, sra_list[[1]]@OM@nsim)), alpha = 0.05) {
  surv <- purrr::map2_dfr(sra_list, names(sra_list), get_sra_survey,
    survey_names = survey_names
  )
  x <- sra_list[[1]]
  surv$year <- surv$year + x@OM@CurrentYr - x@OM@nyears

  surv_plot <- surv %>%
    dplyr::filter(iter %in% sample(unique(surv$iter), sample_n)) %>%
    group_by(scenario, survey) %>%
    mutate(geo_mean = exp(mean(log(value)))) %>%
    mutate(value = value / geo_mean)

  surv_plot_distinct <- surv_plot %>%
    select(scenario, survey, geo_mean) %>%
    dplyr::distinct()

  yrs_df <- tibble(
    real_year = seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr),
    year = seq_len(x@OM@nyears)
  )

  extract_ts <- function(x, slot) {
    out <- reshape2::melt(x@data[[slot]]) %>%
      dplyr::rename(year = Var1, survey_number = Var2, value = value) %>%
      tibble::as_tibble() %>%
      dplyr::left_join(yrs_df, by = "year")
    names(out)[3] <- slot
    out
  }

  index_sd <- purrr::map_dfr(sra_list, extract_ts,
    slot = "I_sd", .id = "scenario")
  index <-  purrr::map_dfr(sra_list, extract_ts,
    slot = "Index", .id = "scenario") %>%
    left_join(index_sd, by = c("year", "survey_number", "real_year", "scenario")) %>%
    left_join(tibble(
      survey_number = seq_len(max(index_sd$survey_number)),
      survey = survey_names
    ), by = "survey_number") %>%
    left_join(surv_plot_distinct, by = c("scenario", "survey")) %>%
    group_by(scenario, survey) %>%
    mutate(Index = Index / geo_mean)

  index$survey <- factor(index$survey, levels = survey_names)
  index$scenario <- factor(index$scenario, levels = names(sra_list))
  surv_plot$survey <- factor(surv_plot$survey, levels = survey_names)
  surv_plot$scenario <- factor(surv_plot$scenario, levels = names(sra_list))

  ggplot(surv_plot, aes_string("year", "value", group = "paste(iter, survey)")) +
    geom_line(alpha = alpha, colour = "grey40") +
    ggplot2::geom_linerange(
      data = index, mapping = aes_string(
        x = "real_year",
        ymin = "exp(log(Index) - 2 * I_sd)", ymax = "exp(log(Index) + 2 * I_sd)"
      ),
      inherit.aes = FALSE, na.rm = TRUE
    ) +
    geom_point(
      data = index, mapping = aes_string(x = "real_year", y = "Index"),
      inherit.aes = FALSE, na.rm = TRUE, pch = 21, size = 2, fill = "white"
    ) +
    facet_grid(vars(scenario), vars(survey)) +
    theme_pbs() +
    ylab("Scaled index value") + xlab("Year")
}

get_sra_survey <- function(sra, sc_name, survey_names) {
  n_surv <- dim(sra@Misc[[1]]$Ipred)[2]
  out2 <- purrr::map(seq_len(n_surv), function(i) {
    surveys <- do.call(cbind, purrr::map(sra@Misc, ~ .$Ipred[, i, drop = FALSE]))
    out <- reshape2::melt(surveys) %>%
      rename(year = Var1, iter = Var2)
    out$scenario <- sc_name
    out$survey <- survey_names[i]
    out
  })
  bind_rows(out2)
}
