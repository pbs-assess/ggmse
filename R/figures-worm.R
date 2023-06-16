#' Make a Kobe "worm" timeseries plot with uncertainty
#'
#' @param object_list A named list of MSE objects from DLMtool.
#'   Names become scenario names.
#' @param prob Tail probability for the quantiles. E.g., 0.5 refers to an
#'   interquartile range.
#' @param include_historical Logical: include the historical time?
#' @param french French?
#'
#' @return A ggplot object
#' @importFrom ggplot2 geom_polygon scale_fill_viridis_c scale_colour_viridis_c
#'   coord_fixed scale_shape_manual
#' @importFrom rosettafish en2fr
#' @export
#'
#' @details
#' Note that if you receive an error such as
#' `Error: vector memory exhausted (limit reached?)`,
#' consider starting a fresh R session of removing
#' any large objects from memory. Also,
#' try adding `R_MAX_VSIZE=64Gb` (or pick some reasonable large value)
#' to your .Renviron file. `usethis::edit_r_environ()`
#'
#' @examples
#' x <- list()
#' x[[1]] <- mse_example
#' x[[2]] <- mse_example
#' names(x) <- c("Scenario 1", "Scenario 2")
#' plot_worms_grid(x)
plot_worms_grid <- function(object_list, prob = 0.5,
                            include_historical = TRUE,
                            french = isTRUE(getOption("french"))) {

  if (is.null(object_list[[1]]@OM$CurrentYr[[1]])) {
    warning(
      "Missing `object@OM$CurrentYr`.\n",
      "Please run the MSE with a newer GitHub DLMtool version\n",
      "or set `object@OM$CurrentYr` yourself.\n",
      "Setting CurrentYr = 0 for now.", call. = FALSE
    )
    this_year <- 0
  } else {
    this_year <- object_list[[1]]@OM$CurrentYr[[1]]
  }

  out <- purrr::map(object_list, ~{
    ts <- get_ts(.x, type = c("SSB", "FM"), this_year = this_year)
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
    list(dd = dd %>% mutate(mp_name = factor(mp_name, levels = .x@MPs)),
         other = other %>% mutate(mp_name = factor(mp_name, levels = .x@MPs)),
         poly_df = poly_df %>% mutate(mp_name = factor(mp_name, levels = .x@MPs)))
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
    ggplot2::facet_grid(mp_name ~ scenario) +
    coord_fixed(xlim = c(0, 3), ylim = c(0, 3), expand = FALSE) +
    geom_vline(xintercept = c(0.4, 0.8), lty = 2, alpha = 0.2, lwd = 0.5) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.2, lwd = 0.5) +
    labs(
      fill = en2fr("Year", french), colour = en2fr("Year", french),
      x = parse(text = en2fr("B/B[MSY]", french)),
      y = parse(text = en2fr("F/F[MSY]", french)),
      pch = en2fr("Year", french)
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
