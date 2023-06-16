#' Plot projection time series
#'
#' @param object A MSEtool object of class `mse` that was created by
#'  running [MSEtool::runMSE()].
#' @param type A character vector describing the element of `mse@` to
#'   plot. Each of these "types" will be included as a column in the
#'   final plot.
#' @param n_samples The number of timeseries samples to illustrate.
#' @param probs A numeric vector of the quantiles to plot as ribbons. Must be of length 2.
#' @param ribbon_colours A character vector of length three giving the colours for
#'   the outer uncertainty ribbon, inner uncertainty ribbon, and median.
#' @param bbmsy_zones A numeric vector of status zone lines to add to
#'   the B/Bmsy panel if it is present.
#' @param catch_reference The number of years over which to calculate the
#'   average catch and show as a horizontal line in a plot of catch.
#' @param clip_ylim An optional numeric value to set the upper ylim
#'   value. `clip_ylim` is multiplied by the highest median value
#'   across the panels. Useful if there are outlying timeseries that distort
#'   the y axis limits.
#' @param seed The seed to set before drawing samples.
#' @param french French?
#'
#' @return ggplot object
#' @importFrom rlang .data
#' @importFrom dplyr bind_cols summarise
#' @export
#' @seealso [plot_main_projections()]
#' @examples
#' plot_projection_ts(mse_example, type = "SSB")
#' plot_projection_ts(mse_example, type = c("SSB", "FM"))
plot_projection_ts <- function(object,
                               type = c("SSB", "FM", "Catch"),
                               n_samples = 3,
                               probs = c(0.1, 0.5),
                               ribbon_colours = RColorBrewer::brewer.pal(8, "Blues")[c(2, 4, 8)],
                               bbmsy_zones = c(0.4, 0.8),
                               catch_reference = 1,
                               clip_ylim = NULL,
                               seed = 42, french = FALSE) {
  type <- match.arg(type, several.ok = TRUE)
  if (length(type) == 3L)
    stop("type must not be all 3 (SSB, FM, Catch) at once. Either pick SSB and FM (or 1 of those) or Catch.", call. = FALSE)

  if (is.null(object@OM$CurrentYr[[1]])) {
    warning(
      "Missing `object@OM$CurrentYr`.\n",
      "Please run the MSE with a newer GitHub MSEtool version\n",
      "or set `object@OM$CurrentYr` yourself.\n",
      "Setting CurrentYr = 0 for now.", call. = FALSE
    )
    this_year <- 0
  } else {
    this_year <- object@OM$CurrentYr[[1]]
  }
  n_hist_years <- dim(object@SSB_hist)[2]
  .hist_years <- seq(this_year - n_hist_years + 1, this_year)
  ts_data <- get_ts(object = object, type = type, this_year = this_year)
  quantiles <- get_ts_quantiles(ts_data, probs = probs)

  set.seed(seed)
  sampled_ids <- sample(unique(ts_data$iter), size = n_samples)
  d <- dplyr::filter(ts_data, .data$iter %in% sampled_ids)

  .type_labels <- gsub("_", "/", unique(d$Type))
  .type_labels <- gsub("MSY", "[MSY]", .type_labels)
  if (french) {
    .type_labels <- gsub("Catch", "Prise", .type_labels)
  }
  # .type_labels <- gsub("B/B\\[MSY\\]", "SSB/SSB[MSY]", .type_labels)

  type_df <- data.frame(
    Type = unique(d$Type), type_labels = .type_labels,
    stringsAsFactors = FALSE
  )
  if (all(type == c("SSB", "FM"))) {
    type_df$type_labels <- factor(type_df$type_labels, levels = c("B/B[MSY]", "F/F[MSY]"))
  }

  mp_names <- sort(unique(d$mp_name))
  ref_grep <- grepl("ref", mp_names)
  if (any(ref_grep)) { # move ref MPs to end
    mp_names <- c(mp_names[!ref_grep], mp_names[ref_grep])
  }

  d <- dplyr::left_join(d, type_df, by = "Type")
  quantiles <- dplyr::left_join(quantiles, type_df, by = "Type")

  d$mp_name <- factor(d$mp_name, levels = mp_names)
  quantiles$mp_name <- factor(quantiles$mp_name, levels = mp_names)

  lines <- data.frame(value = bbmsy_zones, type_labels = "B/B[MSY]", stringsAsFactors = FALSE)
  lines <- dplyr::bind_rows(lines, data.frame(value = 1, type_labels = "F/F[MSY]", stringsAsFactors = FALSE))

  if (french) {
    lines$type_labels <- gsub("MSY", "RMD", lines$type_labels)
  }

  if (!french) {
    if (all(type == c("SSB", "FM"))) {
      lines$type_labels <- factor(lines$type_labels, levels = c("B/B[MSY]", "F/F[MSY]"))
    }
  } else {
    if (all(type == c("SSB", "FM"))) {
      lines$type_labels <- factor(lines$type_labels, levels = c("B/B[RMD]", "F/F[RMD]"))
    }
  }

  if (french && all(type == c("SSB", "FM"))) {
    d$type_labels <- gsub("MSY", en2fr("MSY"), d$type_labels)
    d$type_labels <- factor(d$type_labels, levels = c("B/B[RMD]", "F/F[RMD]"))
    lines$type_labels <- gsub("MSY", en2fr("MSY"), lines$type_labels)
    lines$type_labels <- factor(lines$type_labels, levels = c("B/B[RMD]", "F/F[RMD]"))
    quantiles$type_labels <- gsub("MSY", en2fr("MSY"), quantiles$type_labels)
    quantiles$type_labels <- factor(quantiles$type_labels, levels = c("B/B[RMD]", "F/F[RMD]"))
  }

  lines <- dplyr::filter(lines, type_labels %in% unique(d$type_labels))

  g <- ggplot(d, aes_string("real_year", "value", group = "iter"))
  g <- g + ggplot2::geom_ribbon(
    data = quantiles,
    aes_string(x = "real_year", ymin = "ll", ymax = "uu"),
    colour = NA, fill = ribbon_colours[1], inherit.aes = FALSE
  )
  g <- g + ggplot2::geom_ribbon(
    data = quantiles,
    aes_string(x = "real_year", ymin = "l", ymax = "u"),
    colour = NA, fill = ribbon_colours[2], inherit.aes = FALSE
  )
  g <- g + ggplot2::geom_line(
    data = quantiles,
    aes_string(x = "real_year", y = "m"),
    colour = ribbon_colours[3], lwd = 1, inherit.aes = FALSE
  )

  if ("SSB" %in% type || "F" %in% type) {
    g <- g + geom_hline(data = lines, mapping = aes(yintercept = value), alpha = 0.2, lty = 2, lwd = 0.5)
  }

  if ("Catch" %in% type) {
    average_catch <- dplyr::filter(d, Type == "Catch", real_year %in%
                                     .hist_years[(length(.hist_years) - (catch_reference - 1)):length(.hist_years)]) %>%
      summarize(average_catch = mean(value)) %>%
      pull(average_catch)
    g <- g + ggplot2::geom_hline(yintercept = average_catch, alpha = 0.2, lty = 2, lwd = 0.5)
  }

  g <- g + ggplot2::geom_line(alpha = 0.3, lwd = 0.4) + # sampled lines
    ggplot2::ylab(en2fr("Value", french)) +
    ggplot2::xlab(en2fr("Year", french)) +
    theme_pbs() +
    ggplot2::facet_grid(mp_name ~ type_labels, labeller = ggplot2::label_parsed) +
    ggplot2::coord_cartesian(
      expand = FALSE,
      ylim = if (is.null(clip_ylim)) NULL else c(0, clip_ylim * max(quantiles$m))
    ) +
    ggplot2::theme(panel.spacing = grid::unit(-0.1, "lines")) +
    geom_vline(xintercept = this_year, lty = 2, alpha = 0.3)
  g
}


get_ts <- function(object,
                   type = c("SSB", "FM", "Catch"),
                   this_year = 0) {
  if (!is(object, "MSE")) {
    stop(
      "`object` must be a MSEtool object of class `mse`",
      "that was created by running `MSEtool::runMSE()`."
    )
  }

  mps <- data.frame(
    mp = seq_along(object@MPs),
    mp_name = object@MPs, stringsAsFactors = FALSE
  )
  iters <- object@nsim

  ts_data <- lapply(seq_along(type), function(i) {
    slot(object, type[i]) %>%
      reshape2::melt() %>%
      dplyr::rename(
        iter = .data$Var1, mp = .data$Var2,
        value = .data$value, year = .data$Var3
      ) %>%
      dplyr::left_join(mps, by = "mp") %>%
      dplyr::mutate(Type = type[i])
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(real_year = year + this_year)

  ts_data$iter <- rep(seq_len(object@nsim), length(unique(ts_data$Type))) # parallel messed this up

  # --------------
  # historical
  n_hist_years <- object@nyears
  .hist_years <- this_year - n_hist_years + 1
  hist_data <- lapply(seq_along(type), function(i) {
    slot(object, ifelse(type[i] == "Catch", "CB_hist", paste0(type[i], "_hist"))) %>%
      #apply(c(1, 3), if (type[i] == "FM") max else sum) %>%
      reshape2::melt() %>%
      dplyr::rename(
        iter = .data$Var1,
        value = .data$value,
        year = .data$Var2
      ) %>%
      dplyr::mutate(Type = type[i])

  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      real_year = year + .hist_years - 1,
      iter = rep(seq_len(iters), n_hist_years * length(type)) # parallel messed this up
    )
  hist_data2 <- do.call(
    "rbind",
    replicate(length(mps$mp), hist_data, simplify = FALSE)
  )
  hist_data2[["mp_name"]] <- rep(mps$mp_name, each = nrow(hist_data))
  hist_data2[["mp"]] <- rep(mps$mp, each = nrow(hist_data))

  # projected and historic time series
  ts_data <- bind_rows(ts_data, hist_data2)

  # static ref pts:
  # dim(object@Misc$MSYRefs$ByYear$SSBMSY)
  all_mps <- tibble(mp = unique(ts_data$mp)[!is.na(unique(ts_data$mp))])
  all_mp_yrs <- expand.grid(mp = all_mps$mp, real_year = sort(unique(hist_data2$real_year)))
  # object@Misc$MSYRefs$Refs
  ref_ssb_hist <- data.frame(
    ref = object@RefPoint$SSBMSY[, 1, object@nyears], iter = seq_len(iters),
    Type = "SSB", stringsAsFactors = FALSE
  ) %>%
    left_join(bind_cols(all_mp_yrs, tibble(Type = rep("SSB", nrow(all_mp_yrs)))), by = "Type")
  ref_f_hist <- data.frame(
    ref = object@RefPoint$FMSY[, 1, object@nyears], iter = seq_len(iters),
    Type = "FM", stringsAsFactors = FALSE
  ) %>%
    left_join(bind_cols(all_mp_yrs, tibble(Type = rep("FM", nrow(all_mp_yrs)))), by = "Type")

  all_mp_yrs <- expand.grid(mp = seq_along(mps$mp), real_year = sort(unique(ts_data$real_year)))
  ref_msy <- data.frame(ref = 1, iter = seq_len(iters), Type = "Catch", stringsAsFactors = FALSE) %>%
    left_join(bind_cols(all_mp_yrs, tibble(Type = rep("Catch", nrow(all_mp_yrs)))), by = "Type")

  # dynamic ref pts for projections:
  ref_ssb <- object@RefPoint$SSBMSY %>%
    array(c(object@nsim, object@nMPs, object@nyears + object@proyears)) %>%
    reshape2::melt() %>%
    dplyr::filter(Var3 > object@nyears) %>%
    transmute(iter = .data$Var1,
              real_year = .data$Var3 + .hist_years - 1,
              mp = .data$Var2,
              ref = .data$value, Type = "SSB")
  ref_f <- object@RefPoint$FMSY %>%
    array(c(object@nsim, object@nMPs, object@nyears + object@proyears)) %>%
    reshape2::melt() %>%
    dplyr::filter(Var3 > object@nyears) %>%
    transmute(iter = .data$Var1,
              real_year = .data$Var3 + .hist_years - 1,
              mp = .data$Var2,
              ref = .data$value,
              Type = "FM")

  refs <- ref_ssb %>%
    bind_rows(ref_f) %>%
    bind_rows(ref_msy) %>%
    bind_rows(ref_ssb_hist) %>%
    bind_rows(ref_f_hist)

  ts_data <- left_join(ts_data, refs, by = c("iter", "mp", "Type", "real_year")) %>%
    mutate(value = value / ref)

  type[type == "SSB"] <- "B_BMSY"
  type[type == "FM"] <- "F_FMSY"
  type[type == "Catch"] <- "Catch"

  ts_data$Type[ts_data$Type == "FM"] <- "F_FMSY"
  ts_data$Type[ts_data$Type == "SSB"] <- "B_BMSY"
  ts_data$Type[ts_data$Type == "Catch"] <- "Catch"

  ts_data
}

get_ts_quantiles <- function(x, probs = c(0.1, 0.5)) {
  x %>%
    dplyr::group_by(.data$mp_name, .data$real_year, .data$Type) %>%
    dplyr::summarize(
      median_value = median(.data$value, na.rm = TRUE),
      u = quantile(.data$value, probs = 1 - probs[2] / 2, na.rm = TRUE),
      l = quantile(.data$value, probs = probs[2] / 2, na.rm = TRUE),
      m = quantile(.data$value, probs = 0.50, na.rm = TRUE),
      uu = quantile(.data$value, probs = 1 - probs[1] / 2, na.rm = TRUE),
      ll = quantile(.data$value, probs = probs[1] / 2, na.rm = TRUE)
    ) %>%
    ungroup()
}

#' Make a standard projection plot with SSB, F, and catch
#'
#' This is a wrapper for [plot_projection_ts()] that includes columns for SSB,
#' F, and catch.
#'
#' @param object An MSE object from MSEtool.
#' @param catch_breaks Optional y-axis tick locations for the catch column.
#' @param catch_labels Optional y-axis tick labels for the catch column. Helpful
#'   for dealing with large numbers.
#' @param rel_widths A numeric vector of length 2 the controls the relative
#'   width of the SSB and F columns (first element of the numeric vector) and
#'   the catch column (second element of the numeric vector). Depending on the
#'   number of years in the projection on the size of the axis labels, you may
#'   need to tweak the second element of this vector to make all 3 columns the
#'   same width. Figured it out by trial and error.
#' @param msy_ylim SSB and F column y limits.
#' @param catch_ylim Catch column y limits.
#' @param french French?
#'
#' @return A ggplot2 object
#' @export
#' @seealso [plot_projection_ts()]
#'
#' @examples
#' catch_breaks <- c(0, 1000, 2000, 3000)
#' plot_main_projections(mse_example,
#'   catch_breaks = catch_breaks,
#'   catch_labels = catch_breaks / 1000
#' )
plot_main_projections <- function(object,
                                  catch_breaks = NULL, catch_labels = NULL, rel_widths = c(2, 1.18),
                                  msy_ylim = c(0, 4.5), catch_ylim = NULL,
                                  french = isTRUE(getOption("french"))) {

  suppressMessages({
    g1 <- plot_projection_ts(object, type = c("SSB", "FM"), french = french) +
      ggplot2::coord_cartesian(expand = FALSE, ylim = msy_ylim) +
      ggplot2::theme(
        strip.text.y = ggplot2::element_blank()
      #  axis.title.y = ggplot2::element_blank()
      )
  })

  g2 <- plot_projection_ts(object,
    type = "Catch",
    catch_reference = 1, french = french
  ) #+ ggplot2::theme(axis.title.y = ggplot2::element_blank())

  if (!is.null(catch_ylim)) {
    suppressMessages({
      g2 <- g2 + ggplot2::coord_cartesian(expand = FALSE, ylim = catch_ylim)
    })
  }

  if (!is.null(catch_breaks) && is.null(catch_labels)) {
    catch_labels <- catch_breaks
  }
  if (!is.null(catch_breaks)) {
    suppressMessages({
      g2 <- g2 +
        ggplot2::scale_y_continuous(breaks = catch_breaks, labels = catch_labels)
      g2 <- g2 + ggplot2::coord_cartesian(expand = FALSE, ylim = catch_ylim)
    })
  }

  cowplot::plot_grid(g1, g2, rel_widths = rel_widths, align = "h")
}
