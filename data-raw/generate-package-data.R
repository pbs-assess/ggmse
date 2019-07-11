library(DLMtool)
om <- testOM
om@nsim <- 3
mse <- runMSE(OM = om, MPs = "AvC")
saveRDS(om, here::here("data/om.rds"))
saveRDS(mse, here::here("data/mse.rds"))
