#' Make a Kobe "worm" timeseries plot with uncertainty
#'
#' @param object_list A named list of MSE objects from DLMtool.
#'   Names become scenario names.
#' @param this_year This year as a numeric value.
#' @param prob Tail probability for the quantiles. E.g., 0.5 refers to an
#'   interquartile range.
#' @param include_historical Logical: include the historical time?
#'
#' @return A ggplot object
#' @importFrom ggplot2 geom_polygon scale_fill_viridis_c scale_colour_viridis_c
#'   coord_fixed scale_shape_manual
#' @export
#'
#' @examples
#' x <- list()
#' x[[1]] <- mse_example
#' x[[2]] <- mse_example
#' names(x) <- c("Scenario 1", "Scenario 2")
#' plot_worms_grid(x, this_year = 2020)
plot_worms_grid <- function(object_list, this_year, prob = 0.5,
                       include_historical = TRUE) {

  out <- purrr::map(object_list, ~{
    ts <- get_ts(.x, type = c("SSB", "FM"))
    ts_quantiles <- get_ts_quantiles(ts, probs = c(prob, prob))

    if (!include_historical) {
      d <- filter(ts_quantiles, real_year >= this_year)
    } else {
      d <- ts_quantiles
    }

    now <- filter(ts_quantiles, real_year == this_year)

    m <- reshape2::dcast(d, mp_name + real_year ~ Type, value.var = "m") %>%
      rename(b_m = B_BMSY, f_m = F_FMSY)
    l <- reshape2::dcast(d, mp_name + real_year ~ Type, value.var = "l") %>%
      rename(b_l = B_BMSY, f_l = F_FMSY)
    u <- reshape2::dcast(d, mp_name + real_year ~ Type, value.var = "u") %>%
      rename(b_u = B_BMSY, f_u = F_FMSY)
    dd <- left_join(m, l, by = c("mp_name", "real_year")) %>%
      left_join(u, by = c("mp_name", "real_year"))

    poly_df <- split(dd, paste(dd$mp_name, dd$real_year)) %>%
      map_df(~ data.frame(
        x = c(.$b_m, .$b_l, .$b_m, .$b_u, .$b_m),
        y = c(.$f_l, .$f_m, .$f_u, .$f_m, .$f_l),
        real_year = unique(.$real_year),
        mp_name = unique(.$mp_name), stringsAsFactors = FALSE
      ))

    now_m <- reshape2::dcast(now, mp_name + real_year ~ Type, value.var = "m") %>%
      rename(b_m = B_BMSY, f_m = F_FMSY)
    end <- filter(ts_quantiles, real_year == max(ts_quantiles$real_year))
    end_m <- reshape2::dcast(end, mp_name + real_year ~ Type, value.var = "m") %>%
      rename(b_m = B_BMSY, f_m = F_FMSY)
    start <- filter(ts_quantiles, real_year == min(ts_quantiles$real_year))
    start_m <- reshape2::dcast(start, mp_name + real_year ~ Type, value.var = "m") %>%
      rename(b_m = B_BMSY, f_m = F_FMSY)
    other <- bind_rows(now_m, end_m) %>%
      bind_rows(start_m)
    list(dd = dd, other = other, poly_df = poly_df)
  })

  dd <- purrr::map_dfr(out, "dd", .id = "scenario")
  other <- purrr::map_dfr(out, "other", .id = "scenario")
  poly_df <- purrr::map_dfr(out, "poly_df", .id = "scenario")
  if (!include_historical) other <- filter(other, real_year >= this_year)

  g <- dd %>%
    ggplot(aes(b_m, f_m, colour = real_year)) +
    geom_polygon(aes(x = x, y = y, fill = real_year, group = real_year),
      data = poly_df, alpha = 0.2, inherit.aes = FALSE, colour = NA
    ) +
    geom_path(lwd = 1.6, lineend = "round", linejoin = "bevel", colour = "white") +
    geom_path(lwd = 1.0, lineend = "round", linejoin = "bevel") +
    scale_color_viridis_c(option = "C", direction = -1) +
    scale_fill_viridis_c(option = "C", direction = -1) +
    theme_pbs() +
    ggplot2::facet_grid(scenario ~ mp_name) +
    coord_fixed(xlim = c(0, 3), ylim = c(0, 3), expand = FALSE) +
    geom_vline(xintercept = c(0.4, 0.8), lty = 2, alpha = 0.2, lwd = 0.5) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.2, lwd = 0.5) +
    labs(
      fill = "Year", colour = "Year", x = expression(B / B[MSY]),
      y = expression(F / F[MSY]), pch = "Year"
    ) +
    geom_point(data = other, mapping = aes(
      x = b_m, y = f_m,
      pch = as.factor(real_year)
    ), inherit.aes = FALSE, col = "white", stroke = 1.6) +
    geom_point(data = other, mapping = aes(
      x = b_m, y = f_m,
      pch = as.factor(real_year)
    ), inherit.aes = FALSE, col = "black", stroke = 1) +
    scale_shape_manual(values = c(2, 4, 21))
  g
}
