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
#' probs <- get_probs(mse, "P40", "P100", "PNOF", "LTY", "AAVY")
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
#' @param digits How many decimal places to show in the tiles for the values
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
#' plot_probs(probs)
plot_probs <- function(probs_dat,
                       digits = 2,
                       relative_max = FALSE,
                       scale_0_1 = FALSE,
                       sort_by = "decreasing") {
  df <- probs_dat
  # Used if captions are to be used for top labels
  # df <- probs_dat[[1]]
  # captions <- probs_dat[[2]]

  if (sort_by == "decreasing") {
    df$MP <- factor(df$MP, levels = df$MP[do.call(order, df[-1])])
  } else if (sort_by == "increasing") {
    df$MP <- factor(df$MP, levels = df$MP[rev(do.call(order, df[-1]))])
  } else {
    stop("sort_by must be either 'increasing' or 'decreasing'",
      call. = FALSE
    )
  }

  df <- reshape2::melt(df,
    id.vars = "MP",
    variable.name = "type",
    value.name = "value"
  )

  ## Set up expressions for tick labels - only used if captions are to be used as top labels
  ## probs <- as.vector(do.call('rbind', captions))

  df$txt <- vapply(df$value, function(x) {
    gfutilities::f(x, digits)
  }, FUN.VALUE = character(1L))
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
    scale_fill_gradient(low = "white", high = "grey50", limits = c(0, 1)) +
    guides(fill = FALSE) + xlab("") + ylab("") +

    geom_text(aes(x = type, label = txt)) +
    # Used if captions are used for labelling
    # scale_x_discrete(labels = parse(text = probs), position = "left")
    scale_x_discrete(position = "top")

  g
}
