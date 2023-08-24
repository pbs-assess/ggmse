#' Summary of probabilities of performance metrics from the MSE object
#'
#' @param object MSE object, output of the [MSEtool::runMSE()] function
#' @param ... List of performace metrics
#' @param refs List containing the reference limits for each metric
#'
#' @return A tibble of the output
#' @importFrom tibble as_tibble
#' @importFrom MSEtool avail
#' @importFrom methods is
#' @export
#'
#' @examples
#' library(MSEtool)
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
get_probs <- function(object,
                      ...,
                      refs = NULL) {
  if (!is(object, "MSE")) {
    stop("object must be class `MSE`",
      call. = FALSE
    )
  }

  pm_list <- unlist(list(...))
  if (!length(pm_list)) {
    warning("No PM's included. Using defaults")
    pm_list <- c("PNOF", "P50", "AAVY", "LTY")
  }
  if (!inherits(pm_list, "character")) {
    stop("Must provide names of PM methods",
      call. = FALSE
    )
  }

  means <- names <- captions <- mps <- list()
  for (X in seq_len(length(pm_list))) {
    ref <- refs[[pm_list[X]]]
    if (is.null(ref)) {
      run_pm <- eval(call(pm_list[X], object))
    } else {
      run_pm <- eval(call(pm_list[X], object, Ref = ref))
    }
    means[[X]] <- run_pm@Mean
    names[[X]] <- run_pm@Name
    captions[[X]] <- run_pm@Caption
    mps[[X]] <- run_pm@MPs
  }

  df <- data.frame(
    "MP" = mps[[1]],
    signif(do.call("cbind", means), 2), stringsAsFactors = FALSE
  )
  colnames(df)[2:(length(pm_list) + 1)] <- pm_list

  # If the following list is returned, the second item will be a list of captions
  #  representing the columns of the data frame in item 1
  # list(as_tibble(df), captions)
  as_tibble(df)
}

#' Summary of performance metrics from MSE objects in a coloured tile table
#' format
#'
#' @param probs_dat A data frame as returned from [get_probs()].
#' @param relative_max Make the plot have each column use a relative maximum. If
#'   `scale_0_1` is used, this will be ignored
#' @param scale_0_1 Scale each column from 0 to 1, so that the colours in each
#'   column are fully represented
#' @param sort_by show values in decreasing or increasing format
#' @param mp_order Optional hardcoded MP order
#' @param digits How many decimal places to show in the tiles for the values
#' @param satisficed An optional named numeric vector. The names correspond to
#'   the performance metrics and the values correspond to the values above which
#'   (`>`) the cells will be outlined as "satisficed".
#' @param return_data Logical. If `TRUE` then the underlying data frame is
#'   returned instead of the plot.
#' @param alpha Transparency of underlying colour.
#' @param do_approx Logical. If `TRUE`, values greater than 0.99 are replaced with ">0.99" and less than 0.01 are replaced
#' "<0.01".
#' @param french French?
#'
#' @importFrom reshape2 melt
#' @importFrom gfutilities f
#' @importFrom dplyr ungroup mutate group_by
#' @importFrom ggplot2 ggplot theme geom_tile geom_text scale_fill_gradient
#'   scale_x_discrete aes
#' @importFrom ggplot2 element_blank element_text guides xlab ylab
#' @export
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' plot_tigure(probs)
#' plot_tigure(probs, alpha = 0.9)
#' plot_tigure(probs, satisficed = c("P40" = 0.9, "LTY" = 0.9))
plot_tigure <- function(probs_dat,
                        digits = 2,
                        relative_max = FALSE,
                        scale_0_1 = FALSE,
                        sort_by = "decreasing",
                        mp_order = NULL,
                        satisficed = NULL,
                        return_data = FALSE,
                        alpha = 0.6,
                        do_approx = TRUE,
                        french = isTRUE(getOption("french"))) {

  df <- probs_dat

  if (is.null(mp_order)) {
    if (sort_by == "decreasing") {
      df$MP <- factor(df$MP, levels = df$MP[do.call(order, df[-1])])
    } else if (sort_by == "increasing") {
      df$MP <- factor(df$MP, levels = df$MP[rev(do.call(order, df[-1]))])
    } else {
      stop("sort_by must be either 'increasing' or 'decreasing'",
        call. = FALSE
      )
    }
  } else {
    df$MP <- factor(df$MP, levels = mp_order)
  }

  df <- reshape2::melt(df,
    id.vars = "MP",
    variable.name = "type",
    value.name = "value"
  )

  df$txt <- vapply(df$value, function(x) {
    gfutilities::f(x, digits)
  }, FUN.VALUE = character(1L))

  if (do_approx) {
    OutDec <- options()$OutDec
    df$txt <- gsub(paste0("1\\", OutDec, "00"), paste0(">0", OutDec , "99"), df$txt)
    df$txt <- gsub(paste0("0\\", OutDec, "00"), paste0("<0", OutDec , "01"), df$txt)
  }

  if (relative_max) {
    df <- group_by(df, type) %>%
      mutate(value = value / max(value)) %>%
      ungroup()
  }
  if (scale_0_1) {
    df <- group_by(df, type) %>%
      mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
      ungroup()
  }

  df$MP <- as.factor(df$MP)

  # Reference MPs are italicized and followed with an asterisk
  lab_text <- sapply(levels(df$MP), function(x) {
    if(grepl("ref", x)) { #ifelse causes errors because it parses all MP names
      parse(text = paste0("italic(", x, "~\"*\")"))
    } else {
      x
    }
  }) %>%
    structure(names = levels(df$MP))

  padding <- 0.52

  g <- ggplot(df, ggplot2::aes_string(x = "type", y = "MP")) +
    ggplot2::geom_tile(aes(fill = value), color = "white") +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1), begin = 0.15, end = 1, alpha = alpha, option = "D", direction = 1) +
    ggplot2::guides(fill = "none") +
    xlab("") +
    ylab("") +
    ggplot2::geom_text(aes(x = type, label = txt), size = ggplot2::rel(3)) +
    ggplot2::coord_cartesian(
      expand = FALSE,
      xlim = range(as.numeric(df$type)) + c(-padding, padding),
      ylim = range(as.numeric(df$MP)) + c(-padding - 0.01, padding + 0.01)
    ) +
    theme_pbs() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = element_text(color = "grey10")
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete(labels = lab_text)

  if (!is.null(satisficed)) {
    h <- purrr::map_df(
      seq_along(satisficed),
      ~ dplyr::filter(df, value > satisficed[[.x]] & type == names(satisficed)[.x])
    )
    g <- g + geom_tile(data = h, color = "grey30", lwd = 0.45, fill = NA)
  }

  if (!return_data) {
    g
  } else {
    list(df = df, lab_text = lab_text, alpha = alpha)
  }
}

#' @param pm_df_list A named list of performance metric data frames from
#'   [get_probs()]. The names will be used as the plot labels.
#' @param ncol Optional number of columns.
#' @param ... Other arguments to pass to [plot_tigure()].
#'
#' @export
#' @rdname plot_tigure
#'
#' @examples
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm <- list()
#' pm[[1]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' pm[[2]] <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
#' names(pm) <- c("Scenario 1", "Scenario 2")
#' plot_tigure_facet(pm)
plot_tigure_facet <- function(pm_df_list, ncol = NULL, ...) {
  if (!is.list(pm_df_list)) {
    stop("`pm_df_list` must be a list of data frames from `get_probs()`.",
      call. = FALSE
    )
  }

  gdat <- purrr::map(pm_df_list, plot_tigure, return_data = TRUE, ...)
  gdat2 <- purrr::map_dfr(gdat, "df", .id = "scenario")

  # Reference MPs are italicized and followed with an asterisk
  lab_text <- sapply(levels(gdat2$MP), function(x) {
    if(grepl("ref", x)) { #ifelse causes errors because it parses all MP names
      parse(text = paste0("italic(", x, "~\"*\")"))
    } else {
      x
    }
  }) %>%
    structure(names = levels(gdat2$MP))

  g <- ggplot(gdat2, ggplot2::aes_string(x = "type", y = "MP")) +
    ggplot2::geom_tile(aes(fill = value), color = "white") +
    ggplot2::scale_fill_viridis_c(
      limits = c(0, 1), begin = 0.15, end = 1, alpha = gdat[[1]]$alpha,
      option = "D", direction = 1
    ) +
    ggplot2::guides(fill = "none") +
    xlab("") +
    ylab("") +
    ggplot2::geom_text(aes_string(x = "type", label = "txt"), size = ggplot2::rel(3)) +
    ggplot2::facet_wrap(ggplot2::vars(scenario), scales = "free_x", ncol = ncol) +
    ggplot2::coord_cartesian(expand = FALSE) +
    theme_pbs() +
    ggplot2::theme(
      # panel.border = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 11),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = element_text(color = "grey10")
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete(labels = levels(df$MP))

  g
}
