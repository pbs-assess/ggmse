## This code is not exported, but the objects it creates are located in RDA files in the data directory
## Add new package objects here, and re-source this file to re-generate the RDA file.

library(DLMtool)
library(gfdlm)
om <- testOM
om@nsim <- 10
mse <- runMSE(OM = om, MPs = c("CC1.0", "Itarget1", "Iratio2", "FMSYref75"))
# P40 <- pm_factory("SBMSY", 0.4)

mse_example <- mse

use_data(om, overwrite = TRUE)
use_data(mse_example, overwrite = TRUE)
