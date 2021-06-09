# context("Check that custom MPs are working.")
#
# library(DLMtool)
# om <- MSEtool::testOM
# om@nsim <- 2
# .mse <- runMSE(OM = om, MPs = "AvC", Hist = TRUE)

# oddify <- function(x) seq(1, x, 2)
# out <- ggmse:::remove_years(.mse@Data, "Ind", oddify)
# expect_true(all(is.na(out@Ind[, seq(1, ncol(out@Ind), 2)])))
# expect_true(all(!is.na(out@Ind[, seq(2, ncol(out@Ind), 2)])))
#
# out <- ggmse:::remove_years(.mse@Data, "CAL", oddify)
# expect_true(all(is.na(out@CAL[, seq(1, ncol(out@CAL), 2), ])))
# expect_true(all(!is.na(out@CAL[, seq(2, ncol(out@CAL), 2), ])))
#
# out <- ggmse:::remove_years(.mse@Data, "CAA", oddify)
# expect_true(all(is.na(out@CAA[, seq(1, ncol(out@CAA), 2), ])))
# expect_true(all(!is.na(out@CAA[, seq(2, ncol(out@CAA), 2), ])))

# temp_mp <- reduce_survey(Islope1)
# assign("temp_mp", temp_mp, envir = .GlobalEnv) # just for R CMD check and testthat::test()

# expect_identical(class(temp_mp), "MP")
# expect_true(class(temp_mp(1, .mse@Data)) == "Rec")

# mse2 <- runMSE(OM = om, MPs = "temp_mp")
# expect_true(class(mse2) == "MSE")
