## This code is not exported, but the constants it creates are located in RDA files in the data directory
## Add new package constants here, and re-source this file to re-generate the RDA file.

library(DLMtool)
om <- testOM
om@nsim <- 3
mse <- runMSE(OM = om, MPs = "AvC")

usethis::use_data(om)
usethis::use_data(mse)
