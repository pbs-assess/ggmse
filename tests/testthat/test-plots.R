context("Check that figures work")

mse <- list()
mse[[1]] <- mse_example
mse[[2]] <- mse_example
mse[[3]] <- mse_example
names(mse) <- c("sc1", "sc2", "sc3")
scenario_df <- tibble::tribble(
  ~scenario, ~scenario_human, ~scenario_type,
  "sc1", "Scenario 1", "Reference",
  "sc2", "Scenario 2", "Reference",
  "sc3", "Scenario 3", "Robustness"
)
pm <- c("P40", "LTY")
custom_pal <- c(RColorBrewer::brewer.pal(3, "Set2"), "grey60")
names(custom_pal) <- c("CC1.0", "Itarget1", "Iratio2", "FMSYref75")

test_that("plot_factory() runs", {
  plots <- plot_factory(
    mse,
    pm = pm,
    scenario_df = scenario_df,
    mp_sat = c("Itarget1", "Iratio2", "FMSYref75"),
    mp_not_sat = c("CC1.0"),
    mp_not_sat2 = c("CC1.0"),
    mp_ref = c("FMSYref75"),
    custom_pal = custom_pal,
    eg_scenario = "sc1",
    tradeoff = c("P40", "LTY"),
    satisficed_criteria = c("P40" = 0.9, "LTY" = 0.8)
  )
  expect_true(length(names(plots)) > 10)
  expect_true("ggplot" %in% class(plots[[1]]))
})
