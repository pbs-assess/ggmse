## This code is not exported, but the objects it creates are located in RDA files in the data directory
## Add new package objects here, and re-source this file to re-generate the RDA file.

library(DLMtool)
om <- testOM
om@nsim <- 3
mse <- runMSE(OM = om, MPs = "AvC")
P40 <- pm_factory("SBMSY", 0.4)

use_data(om, overwrite = TRUE)
use_data(mse, overwrite = TRUE)

## Add new PM functions here and include in package data below (usethis)
# P10 <- pm_factory("SBMSY", 0.1)
# P10_yrs6_20 <- pm_factory("SBMSY", 0.1, c(6, 20))
# P10_yrs21_35 <- pm_factory("SBMSY", 0.1, c(21, 35))
# P10_yrs36_50 <- pm_factory("SBMSY", 0.1, c(36, 50))
# P40 <- pm_factory("SBMSY", 0.4)
# P40_yrs6_20 <- pm_factory("SBMSY", 0.4, c(6, 20))
# P40_yrs21_35 <- pm_factory("SBMSY", 0.4, c(21, 35))
# P40_yrs36_50 <- pm_factory("SBMSY", 0.4, c(36, 50))
# P80 <- pm_factory("SBMSY", 0.8)
# P80_yrs6_20 <- pm_factory("SBMSY", 0.8, c(6, 20))
# P80_yrs21_35 <- pm_factory("SBMSY", 0.8, c(21, 35))
# P80_yrs36_50 <- pm_factory("SBMSY", 0.8, c(36, 50))
# P100 <- pm_factory("SBMSY", 1)
# P100_yrs6_20 <- pm_factory("SBMSY", 1, c(6, 20))
# P100_yrs21_35 <- pm_factory("SBMSY", 1, c(21, 35))
# P100_yrs36_50 <- pm_factory("SBMSY", 1, c(36, 50))
# PNOF <- pm_factory("PNOF", 1)
# PNOF_yrs6_20 <- pm_factory("PNOF", 1, c(6, 20))
# PNOF_yrs21_35 <- pm_factory("PNOF", 1, c(21, 35))
# PNOF_yrs36_50 <- pm_factory("PNOF", 1, c(36, 50))
# LTY <- pm_factory("LTY", 0.5)
# Yield <- pm_factory("Yield", 1)
# AAVY <- pm_factory("AAVY", 0.2)
#
