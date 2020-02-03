#' Summary of probabilities of performance metrics from the MSE object
#'
#' @param object MSE object, output of the [DLMtool::runMSE()] function
#' @param ... List of performace metrics
#' @param refs List containing the reference limits for each metric
#'
#' @return A tibble of the output
#' @importFrom tibble as_tibble
#' @importFrom DLMtool avail
#' @export
#'
#' @examples
#' library(DLMtool)
#' probs <- get_probs(mse_example, "P40", "P100", "PNOF", "LTY", "AAVY")
get_probs <- function(object,
                      ...,
                      refs = NULL) {
  if (class(object) != "MSE") {
    stop("object must be class `MSE`",
      call. = FALSE
    )
  }

  pm_list <- unlist(list(...))
  if (!length(pm_list)) {
    warning("No PM's included. Using defaults")
    pm_list <- c("PNOF", "P50", "AAVY", "LTY")
  }
  if (class(pm_list) != "character") {
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

#' Summary of probabilities of things from the MSE object in a colored tile table format
#'
#' @param probs_dat A list of length 2 - a data frame and another list of captions describing the
#'   columns of the data frame as returned from [gfdlm::get_probs()]
#' @param relative_max Make the plot have each column use a reletive maximum. If
#'  scale_0_1 is used, this will be ignored
#' @param scale_0_1 Scale each column from 0 to 1, so that the colours in each column are fully represented
#' @param sort_by show values in decreasing or increasing format
#' @param mp_order Optional hardcoded MP order
#' @param digits How many decimal places to show in the tiles for the values
#' @param satisficed TODO
#'
#' @importFrom reshape2 melt
#' @importFrom gfutilities f
#' @importFrom dplyr ungroup mutate group_by
#' @importFrom ggplot2 ggplot theme geom_tile geom_text scale_fill_gradient scale_x_discrete aes
#' @importFrom ggplot2 element_blank element_text guides xlab ylab
#' @importFrom gfplot theme_pbs
#' @export
#' @examples
#' library(ggplot2)
#' probs <- get_probs(mse, "P40", "P100", "PNOF", "LTY", "AAVY")
#' plot_prob_tigure(probs)
#' plot_prob_tigure(probs, satisficed = c("P40" = 0.9, "LTY" = 0.9))


plot_prob_tigure <- function(probs_dat,
                       digits = 2,
                       relative_max = FALSE,
                       scale_0_1 = FALSE,
                       sort_by = "decreasing",
                       mp_order = NULL,
                       satisficed = NULL
                       ) {
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

  df$txt <- gsub("1\\.00", ">0.99", df$txt)
  df$txt <- gsub("0\\.00", "<0.01", df$txt)

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

  g <- ggplot(df, aes(x = type, y = MP)) +
    geom_tile(aes(fill = value), color = "white") +
    gfplot::theme_pbs() +
    theme(
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1), alpha = 0.6, option = "D", direction = 1) +
    guides(fill = FALSE) + xlab("") + ylab("") +
    geom_text(aes(x = type, label = txt), size = ggplot2::rel(3)) +
    scale_x_discrete(position = "top")

  if (!is.null(satisficed)) {
    h <- purrr::map_df(seq_along(satisficed),
      ~ dplyr::filter(df, value > satisficed[[.x]] & type == names(satisficed)[.x]))
    g <- g +  geom_tile(data = h, color = "grey30", lwd = 0.45, fill = NA)
  }

  g
}
