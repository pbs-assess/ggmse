# probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
# pm <- list()
# pm[[1]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
# pm[[2]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
# names(pm) <- c("Scenario 1", "Scenario 2")
pm_list <- readRDS("~/Desktop/t.rds")
pm_df_list

x <- purrr::map_df(names(pm_df_list),
  ~dplyr::bind_cols(pm_df_list[[.x]],
    scenario = rep(.x, nrow(pm_df_list[[.x]]))))

min_skating <- function(x, ...) {
  # x <- sort(x)[-1]
  min(x, ...)
}
max_skating <- function(x, ...) {
  # x <- rev(sort(x))[-1]
  max(x, ...)
}

condense_func <- function(dat, f, label = "prob") {
  dplyr::group_by(dat, MP) %>%
    dplyr::summarise_if(is.numeric, f, na.rm = TRUE) %>%
    reshape2::melt(
      id.vars = "MP",
      value.name = label,
      variable.name = "pm"
    )
}

mps <- c(".Iratio2", ".GB_slope6_1", ".GB_slope8_1", ".Itarget1", "FMSYref75")
x <- dplyr::filter(x, MP %in% mps)
pm_avg <- condense_func(x, mean, label = "mean")
pm_min <- condense_func(x, min_skating, label = "min")
pm_max <- condense_func(x, max_skating, label = "max")

pm <- dplyr::left_join(pm_avg, pm_min, by = c("MP", "pm")) %>%
  dplyr::left_join(pm_max, by = c("MP", "pm"))

custom_pal <- c(RColorBrewer::brewer.pal(length(mps)-1, "Set2"),
  "grey40")
names(custom_pal) <- mps
custom_pal

x$MP <- factor(x$MP, levels = mps)
library(ggplot2)
ggplot(pm, aes(pm, mean, group = MP, colour = MP)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_linerange(aes(ymin = min, ymax = max), position = position_dodge(width = 0.3), alpha = 0.3) +
  geom_line(position = position_dodge(width = 0.3), alpha = 0.8) +
  theme_pbs() +
  # scale_color_brewer(palette = "Set2") +
  scale_color_manual(values = custom_pal) +
  coord_cartesian(expand = FALSE, ylim = c(.7, 1.003)) +
  xlab("Performance metric") + ylab("Probability")

dodge <- 0.6
ggplot(pm, aes(pm, mean, group = MP, colour = MP)) +
  geom_point(position = position_dodge(width = dodge), size = 2, pch = 19) +
  geom_linerange(aes(ymin = 0, ymax = mean),
    position = position_dodge(width = dodge), alpha = 0.8, lwd = 0.5) +
  # geom_line(position = position_dodge(width = dodge), alpha = 0.8) +
  theme_pbs() +
  # scale_color_brewer(palette = "Set2") +
  scale_color_manual(values = custom_pal) +
  xlab("Performance metric") + ylab("Probability") +
  coord_flip(expand = FALSE, ylim = c(0, 1.015))

xlong <- reshape2::melt(x,
  id.vars = c("MP", "scenario"),
  value.name = "prob",
  variable.name = "pm"
)


xlong$`Reference MP` <- ifelse(grepl("ref", xlong$MP), "True", "False")
ggplot(xlong, aes(pm, prob, group = MP, colour = MP)) +
  # geom_point(position = position_dodge(width = 0.3), size = 0) +
  # geom_linerange(aes(ymin = min, ymax = max), position = position_dodge(width = 0.3), alpha = 0.3) +
  # geom_vline(aes(xintercept = as.numeric(MP)), lty = 2, col = "grey90", lwd = 0.3) +
  geom_line(alpha = 1, lwd = 0.7, mapping = aes(lty = `Reference MP`)) +
  theme_pbs() +
  # scale_color_brewer(palette = "Set2") +
  scale_color_manual(values = custom_pal) +
  # coord_flip(expand = FALSE, ylim = c(.7, 1.003)) +
  coord_cartesian(expand = FALSE, ylim = c(.7, 1.003), xlim = c(0.7, length(unique(xlong$MP)) + 1.3)) +
  xlab("Performance metric") + ylab("Probability") +
  facet_wrap(~scenario) +
  theme(panel.grid.major.y = element_line(colour = "grey85"), panel.grid.major.x = element_line(colour = "grey85"), panel.grid.minor.y = element_line(colour = "grey96"), axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(col = guide_legend(reverse = TRUE))


pm$`Reference MP` <- ifelse(grepl("ref", pm$MP), TRUE, FALSE)
ggplot(pm, aes(pm, mean, group = MP, colour = MP)) +
  # geom_point(position = position_dodge(width = 0.3), size = 0) +
  geom_ribbon(aes(ymin = min, ymax = max, fill = MP), alpha = 0.1, colour = NA) +
  # geom_vline(aes(xintercept = as.numeric(MP)), lty = 2, col = "grey90", lwd = 0.3) +
  geom_line(alpha = 1, lwd = 0.85, mapping = aes(lty = `Reference MP`)) +
  theme_pbs() +
  # scale_color_brewer(palette = "Set2") +
  scale_color_manual(values = custom_pal) +
  scale_fill_manual(values = custom_pal) +
  # coord_cartesian(expand = FALSE, ylim = c(.7, 1.003)) +
  coord_flip(expand = FALSE, ylim = c(.7, 1.003)) +
  xlab("Performance metric") + ylab("Probability") +
  # facet_wrap(~scenario) +
  theme(panel.grid.major.y = element_line(colour = "grey85"), panel.grid.major.x = element_line(colour = "grey85"), panel.grid.minor.x = element_line(colour = "grey96"))

xlong %>% reshape2::dcast(MP + scenario ~ pm, value.var = "prob") %>%
  mutate(`Reference MP` = ifelse(grepl("ref", MP), "True", "False")) %>%
  ggplot(aes(`LT LRP`, STC, colour = MP, pch = `Reference MP`)) + geom_point() +
  facet_wrap(~scenario, nrow = 2) +
  theme_pbs() +
  coord_fixed(xlim = c(0.7, 1.015), ylim = c(0.7, 1.015), expand = FALSE) +
  scale_color_manual(values = custom_pal) +
  scale_shape_manual(values = c(19, 21)) +
  guides(col = guide_legend(order = 1))

xwide <- xlong %>% reshape2::dcast(MP + scenario ~ pm, value.var = "prob")

avg <- xwide %>% group_by(MP) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  select(MP, STC, `LT LRP`)
.min <- xwide %>% group_by(MP) %>%
  dplyr::summarise_if(is.numeric, min, na.rm = TRUE) %>%
  rename(STC_min = `STC`, LT_LRP_min = `LT LRP`) %>%
  select(MP, STC_min, LT_LRP_min)
.max <- xwide %>% group_by(MP) %>%
  dplyr::summarise_if(is.numeric, max, na.rm = TRUE) %>%
  rename(STC_max = `STC`, LT_LRP_max = `LT LRP`) %>%
  select(MP, STC_max, LT_LRP_max)
avg <- left_join(avg, .min) %>% left_join(.max)

mutate(avg, `Reference MP` = ifelse(grepl("ref", MP), "True", "False")) %>%
  ggplot(aes(`LT LRP`, STC, colour = MP, pch = `Reference MP`)) +
  geom_point(size = 2.5) +
  geom_segment(aes(x = `LT LRP`, xend = `LT LRP`, y = STC_min, yend = STC_max), alpha = 0.8, lwd = 0.5) +
  geom_segment(aes(x = `LT_LRP_min`, xend = `LT_LRP_max`, y = STC, yend = STC), alpha = 0.8, lwd = 0.5) +
  theme_pbs() +
  coord_fixed(xlim = c(0.7, 1.015), ylim = c(0.7, 1.015), expand = FALSE) +
  scale_color_manual(values = custom_pal) +
  scale_shape_manual(values = c(19, 21)) +
  guides(col = guide_legend(order = 1))
