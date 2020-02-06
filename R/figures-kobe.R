#' Calculate a set of x-y coordinates for a contour line
#' Density values are sorted and standardized, and the critical value is
#' calculated from the alpha cutoff value and sent to [gfdlm::calc_contour_lines()]
#' as an argument.
#'
#' @param x Output from the function [MASS::kde2d()]
#' @param alpha A cutoff value between 0 and 1 for the contour line.
#'
#' @return A list of two vectors of x and y-coordinates for the contour line.
#' @importFrom grDevices contourLines
quantile_contour <- function(x, alpha = 0.8) {
  zdens <- rev(sort(x$z))
  cumu_zdens <- cumsum(zdens)
  cumu_zdens <- (cumu_zdens / cumu_zdens[length(zdens)])
  crit.val <- zdens[max(which(cumu_zdens <= alpha))]
  b.full <- contourLines(x, levels = crit.val)
  list(x = b.full[[1]]$x, y = b.full[[1]]$y)
}

#' Calculate management procedure contour lines for two quantities
#'
#' @param d A list of management procedure data frames with columns mp, mp_name, x, and y.
#' @param alpha A vector of levels between 0 and 1 for the contour lines.
#' @param n As defined in [MASS::kde2d()].
#' @importFrom MASS bandwidth.nrd kde2d
#' @importFrom purrr map_dfr map_df
#' @importFrom stats na.omit
#'
#' @return A data frame containing mp, mp_name, alpha, x, and y where x and y are the calculated
#'  coordinates for the contour lines for each alpha and mp.
calc_contour_lines <- function(d,
                               alpha = c(0.025, 0.5, 0.975),
                               n = 200) {
  d <- split(d, d$mp)
  lapply(alpha, function(j) {
    lapply(seq_along(d), function(i) {
      x <- d[[i]]$x
      y <- d[[i]]$y
      x_bw <- MASS::bandwidth.nrd(na.omit(x))
      y_bw <- MASS::bandwidth.nrd(na.omit(y))
      dens <- quantile_contour(MASS::kde2d(x,
        y,
        h = c(x_bw, y_bw),
        n = n
      ),
      alpha = j
      )
      dens$alpha <- rep(j, length(dens$x))
      dens$mp <- rep(i, length(dens$x))
      dens$mp_name <- rep(as.character(d[[i]]$mp_name[1]), length(dens$x))
      dens
    }) %>%
      purrr::map_dfr(`[`, c("mp", "x", "y", "alpha", "mp_name"))
  }) %>%
    purrr::map_df(rbind)
}

#' Make a panel of contour plots by management procedure for B/BMSY by F/FMSY
#'
#' @param object An MSE object as returned by [DLMtool::runMSE()].
#' @param yend The end year
#' @param dontshow_mp A vector of MPs to leave out of the plot.
#' @param show_ref_pt_lines Show the reference point lines at the values in *_ref_lines arguments
#' @param alpha A vector of levels between 0 and 1 for the contour lines.
#' @param n  As defined in [MASS::kde2d()].
#' @param xlim The [ggplot2::xlim()] values for x-axis limits.
#' @param ylim The [ggplot2::ylim()] values for y-axis limits.
#' @param x_ref_lines A vector of vertical lines to draw as reference point lines.
#' @param y_ref_lines A vector of horizontal lines to draw as reference point lines.
#' @param show_contours Logical: show contour lines?
#' @param return_data Logical: return the data instead of the plot?
#'
#' @return A ggplot object
#' @importFrom reshape2 melt
#' @importFrom dplyr filter select rename inner_join left_join
#' @importFrom ggplot2 geom_point geom_path scale_color_viridis_c labs facet_wrap guides geom_vline
#' @importFrom ggplot2 ggplot geom_hline aes xlim ylim
#' @export
#'
#' @examples
#' plot_kobe(mse_example)
plot_kobe <- function(object,
                      yend = max(object@proyears),
                      dontshow_mp = NULL,
                      show_ref_pt_lines = TRUE,
                      alpha = c(0.25, 0.5, 0.75),
                      n = 200,
                      xlim = c(0, 3.5),
                      ylim = c(0, 3.5),
                      x_ref_lines = c(0.4, 0.8),
                      y_ref_lines = 1,
                      show_contours = TRUE,
                      return_data = FALSE) {
  ffmsy <- object@F_FMSY[, , yend] %>%
    reshape2::melt() %>%
    rename(iter = Var1, mp = Var2, ffmsy = value)
  bbmsy <- object@B_BMSY[, , yend] %>%
    reshape2::melt() %>%
    rename(iter = Var1, mp = Var2, bbmsy = value)
  dl <- inner_join(ffmsy, bbmsy, by = c("iter", "mp"))
  dr <- data.frame(mp = seq_along(object@MPs), mp_name = object@MPs)
  d <- left_join(dl, dr, by = "mp") %>%
    filter(!mp_name %in% dontshow_mp) %>%
    select(-iter) %>%
    mutate(x = log(bbmsy), y = log(ffmsy))

  contour_lines <- calc_contour_lines(d, alpha = alpha, n = n)
  contour_lines$x <- exp(contour_lines$x)
  contour_lines$y <- exp(contour_lines$y)
  d$x <- exp(d$x)
  d$y <- exp(d$y)
  d$outside <- FALSE
  d$outside[d$x >= xlim[2]] <- TRUE
  d$outside[d$y >= ylim[2]] <- TRUE
  d$x[d$x >= xlim[2] - 0.05] <- xlim[2] - 0.05
  d$y[d$y >= ylim[2] - 0.05] <- ylim[2] - 0.05

  g <- ggplot(d, ggplot2::aes_string("x", "y")) +
    geom_point(alpha = 0.2, mapping = aes_string(shape = "outside")) +
    ggplot2::guides(shape = FALSE)

  scale <- scale_color_viridis_c(end = 0.95, option = "D", direction = -1,
    breaks = unique(contour_lines$alpha),
    guide = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  shape <- ggplot2::scale_shape_manual(values = c("TRUE" = 21, "FALSE" = 19))
  if (show_contours) {
    g <- g + geom_path(
      data = contour_lines,
      aes_string(color = "alpha", group = "as.factor(alpha)"), alpha = 0.65, lwd = 0.75
    ) + scale

  }
  g <- g + theme_pbs() +
    ggplot2::facet_wrap(~mp_name) +
    labs(
      colour = "Prob.\ndensity", x = expression(B / B[MSY]),
      y = expression(F / F[MSY])
    ) +
    ggplot2::coord_equal(xlim = xlim + c(-0.05, 0.05), ylim = ylim + c(-0.05, 0.05), expand = FALSE)
    # guides(colour = FALSE)

  if (show_ref_pt_lines) {
    g <- g +
      geom_vline(xintercept = x_ref_lines, alpha = 0.2, lty = 2) +
      geom_hline(yintercept = y_ref_lines, alpha = 0.2, lty = 2)
  }

  if (!return_data)
    g
  else
    list(df = d, show_ref_pt_lines = show_ref_pt_lines, show_contours = show_contours,
      x_ref_lines = x_ref_lines, y_ref_lines = y_ref_lines, contour_lines = contour_lines,
      xlim = xlim, ylim = ylim, scale = scale, shape = shape)
}

plot_kobe_grid <- function(object_list, ...) {
  gdat <- purrr::map(object_list, plot_kobe, return_data = TRUE, ...)
  df <- purrr::map_dfr(gdat, "df", .id = "scenario")
  contour_lines <- purrr::map_dfr(gdat, "contour_lines", .id = "scenario")
  x_ref_lines <- purrr::map_dfr(gdat, "x_ref_lines", .id = "scenario")
  y_ref_lines <- purrr::map_dfr(gdat, "y_ref_lines", .id = "scenario")

  g <- ggplot(df, ggplot2::aes_string("x", "y")) +
    geom_point(alpha = 0.2, mapping = aes_string(shape = "outside")) +
    ggplot2::guides(shape = FALSE)

  if (gdat[[1]]$show_contours) {
    g <- g + geom_path(
      data = contour_lines,
      aes(color = alpha, group = as.factor(alpha)), alpha = 0.65, lwd = 0.75
    ) + gdat[[1]]$scale + gdat[[1]]$shape
  }
  g <- g + theme_pbs() +
    ggplot2::facet_grid(mp_name~scenario) +
    labs(
      colour = "Prob.\ndensity", x = expression(B / B[MSY]),
      y = expression(F / F[MSY])
    ) +
    ggplot2::coord_equal(xlim = gdat[[1]]$xlim + c(-0.05, 0.05),
      ylim = gdat[[1]]$ylim + c(-0.05, 0.05), expand = FALSE)
    # guides(colour = FALSE)

  if (gdat[[1]]$show_ref_pt_lines) {
    g <- g +
      geom_vline(xintercept = gdat[[1]]$x_ref_lines, alpha = 0.2, lty = 2) +
      geom_hline(yintercept = gdat[[1]]$y_ref_lines, alpha = 0.2, lty = 2)
  }

  g
}