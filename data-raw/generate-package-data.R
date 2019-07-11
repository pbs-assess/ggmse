## This code is not exported, but the objects it creates are located in RDA files in the data directory
## Add new package objects here, and re-source this file to re-generate the RDA file.

library(DLMtool)
om <- testOM
om@nsim <- 3
mse <- runMSE(OM = om)
P40 <- pm_factory("SBMSY", 0.4)

use_data(om, overwrite = TRUE)
use_data(mse, overwrite = TRUE)
