#' Arguments that are shared by multiple wrapper functions
#'
#' @name shared_plot_args
#'
#' @param data Data frame to use for plot.
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis.
#' @param y_str Character string. Name of variable in \code{data} to plot on
#'   y-axis.
#' @param color_str Character string. Name of variable in \code{data}
#'   to use as color aesthetic in plot.
#' @param fill_str Character string. Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param shape_str Character string. Name of variable in \code{data}
#'   to use as shape aesthetic in plot.
#' @param linetype_str Character string. Name of variable in \code{data}
#'   to use as linetype aesthetic in plot.
#' @param group_str Character string. Name of variable in \code{data}
#'   to use as group aesthetic in plot.
#' @param fill Fill color. Only used if \code{fill_str} is \code{NULL}.
#' @param alpha Alpha value for transparency.
#' @keywords internal
NULL

#' Bar plot wrapper for ggplot2
#'
#' @description Convenient wrapper function around [ggplot2::geom_bar()].
#'
#' @inheritParams ggplot2::geom_bar
#' @inheritParams shared_plot_args
#' @param y_str Character string. Name of variable in \code{data} to plot on
#'   y-axis. Used only if \code{stat = "identity"}.
#' @param stat See [ggplot2::geom_bar()].
#' @param ... Other arguments to pass to [ggplot2::geom_bar()].
#'
#' @return A ggplot bar plot.
#'
#' @examples
#' df <- data.frame(x = rep(letters[1:3], 2), y = rep(LETTERS[1:2], 3))
#' plot_bar(data = df, x_str = "x")
#' plot_bar(df, x_str = "x", fill_str = "y")
#'
#' @export
plot_bar <- function(data, x_str, y_str = NULL, fill_str = NULL,
                     fill = "#6FBBE3", stat = "count", position = "dodge",
                     ...) {

  if (is.null(x_str) | missing(x_str)) {
    stop("Must specify x_str argument.")
  }

  if (is.null(y_str)) {
    ylab <- "Frequency"
  } else {
    ylab <- y_str
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(
      x_str = x_str,
      y_str = y_str,
      fill_str = fill_str
    ) +
    ggplot2::labs(x = x_str, y = ylab, fill = fill_str)

  if (!is.null(fill_str)) {
    if (is.character(data[[fill_str]])) {
      data[[fill_str]] <- as.factor(data[[fill_str]])
    }
    plt <- plt +
      ggplot2::geom_bar(
        position = position, stat = stat, color = "grey98", ...
      )
  } else {
    plt <- plt +
      ggplot2::geom_bar(
        position = position, stat = stat, color = "grey98", fill = fill, ...
      )
  }
  return(plt)
}


#' Boxplot wrapper for ggplot2
#'
#' @description Convenient wrapper around [ggplot2::geom_boxplot()].
#'
#' @inheritParams shared_plot_args
#' @param x_str Character string (optional). Name of variable in \code{data} to
#'   plot on x-axis. Should be discrete (e.g., a factor variable).
#' @param y_str Character string. Name of variable in \code{data} to plot on
#'   y-axis. If \code{NULL}, plot boxplot using all values in data frame.
#' @param horizontal Logical. Whether the boxplots should be horizontal instead
#'   of vertical.
#' @param ... Other arguments to pass to [ggplot2::geom_boxplot()].
#'
#' @return A ggplot boxplot.
#'
#' @examples
#' ## plot boxplot of all data in data frame
#' plot_boxplot(as.data.frame(matrix(rnorm(1000), nrow = 100)))
#' ## plot boxplot of single column in data frame
#' plot_boxplot(iris, y_str = "Sepal.Width")
#' plot_boxplot(iris, x_str = "Species", y_str = "Sepal.Width")
#' iris2 <- data.frame(
#'   iris,
#'   z = as.factor(rep(letters[1:2], length.out = nrow(iris)))
#' )
#' plot_boxplot(iris2, x_str = "Species", y_str = "Sepal.Width", fill_str = "z")
#' plot_boxplot(iris2, y_str = "Sepal.Width", fill_str = "z")
#'
#' @export
plot_boxplot <- function(data, x_str = NULL, y_str = NULL, fill_str = NULL,
                         horizontal = FALSE, ...) {
  if (is.null(y_str)) {  # plot all data
    y_str <- "data"
    data <- data |>
      tidyr::pivot_longer(
        cols = -tidyselect::all_of(c(x_str, fill_str)),
        names_to = "variable", values_to = "data"
      )
  }
  group_str <- x_str

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(
      x_str = x_str,
      y_str = y_str,
      fill_str = fill_str,
      group_str = group_str
    ) +
    ggplot2::geom_boxplot(...) +
    ggplot2::labs(x = x_str, y = y_str, fill = fill_str)
  if (!is.null(x_str) && !is.null(fill_str)) {
    group_str <- substitute(
      interaction(x_str, fill_str),
      list(x_str = as.symbol(x_str), fill_str = as.symbol(fill_str))
    )
    plt <- plt + ggplot2::aes(group = !!group_str)
  }
  if (horizontal) {
    plt <- plt + ggplot2::coord_flip()
  }
  return(plt)
}


#' Kernel density plot wrapper for ggplot2
#'
#' @description Convenient wrapper around [ggplot2::geom_density()].
#'
#' @inheritParams shared_plot_args
#'
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis. If \code{NULL}, plot density using all values in data frame.
#' @param ... Other arguments to pass to [ggplot2::geom_density()].
#'
#' @return A ggplot density.
#'
#' @examples
#' ## plot distribution of all data in data frame
#' plot_density(as.data.frame(rnorm(1000), nrow = 100))
#' ## plot distribution of a single column in data frame
#' plot_density(iris, x_str = "Sepal.Width")
#' plot_density(iris, x_str = "Sepal.Width", fill_str = "Species")
#'
#' @export
plot_density <- function(data, x_str = NULL, fill_str = NULL,
                         fill = "#6FBBE3", alpha = 0.4, ...) {
  if (is.null(x_str)) {  # plot all data
    x_str <- "data"
    data <- data |>
      tidyr::pivot_longer(
        cols = -tidyselect::all_of(fill_str),
        names_to = "variable", values_to = "data"
      )
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(
      x_str = x_str,
      fill_str = fill_str
    ) +
    ggplot2::labs(x = x_str, y = "Density", fill = fill_str)
  if (!is.null(fill_str)) {
    if (is.character(data[[fill_str]])) {
      data[[fill_str]] <- as.factor(data[[fill_str]])
    }
    plt <- plt +
      ggplot2::geom_density(color = "black", alpha = alpha, ...)
  } else {
    plt <- plt +
      ggplot2::geom_density(color = "black", alpha = alpha, fill = fill, ...)
  }
  return(plt)
}


#' Histogram plot wrapper for ggplot2
#'
#' @description Convenient wrapper around [ggplot2::geom_histogram()].
#'
#' @inheritParams ggplot2::geom_histogram
#' @inheritParams shared_plot_args
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis. If \code{NULL}, plot density using all values in data frame.
#' @param ... Other arguments to pass to [ggplot2::geom_histogram()].
#'
#' @return A ggplot histogram.
#'
#' @examples
#' ## plot distribution of all data in data frame
#' plot_histogram(as.data.frame(rnorm(1000), nrow = 100))
#' ## plot distribution of a single column in data frame
#' plot_histogram(iris, x_str = "Sepal.Width")
#' plot_histogram(iris, x_str = "Sepal.Width", fill_str = "Species")
#'
#' @export
plot_histogram <- function(data, x_str = NULL, fill_str = NULL,
                           fill = "#6FBBE3", bins = 12, ...) {
  if (is.null(x_str)) {  # plot all data
    x_str <- "data"
    data <- data |>
      tidyr::pivot_longer(
        cols = -tidyselect::all_of(fill_str),
        names_to = "variable", values_to = "data"
      )
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(
      x_str = x_str,
      fill_str = fill_str
    ) +
    ggplot2::labs(x = x_str, y = "Frequency", fill = fill_str)
  if (!is.null(fill_str)) {
    if (is.character(data[[fill_str]])) {
      data[[fill_str]] <- as.factor(data[[fill_str]])
    }
    plt <- plt +
      ggplot2::geom_histogram(color = "grey98", bins = bins, ...)
  } else {
    plt <- plt +
      ggplot2::geom_histogram(color = "grey98", bins = bins, fill = fill, ...)
  }
  return(plt)
}


#' Line plot wrapper for ggplot2
#'
#' @description Convenient wrapper around [ggplot2::geom_line()].
#'
#' @inheritParams shared_plot_args
#' @param ... Other arguments to pass to [ggplot2::geom_line()].
#'
#' @return A ggplot line plot.
#'
#' @examples
#' df <- data.frame(time = 1:5, value = 5:9)
#' plot_line(df, x_str = "time", y_str = "value")
#' df2 <- data.frame(
#'   time = rep(1:5, 2),
#'   value = 1:10,
#'   group = rep(letters[1:2], each = 5)
#' )
#' plot_line(df2, x_str = "time", y_str = "value", color_str = "group")
#' plot_line(
#'   df2,
#'   x_str = "time", y_str = "value",
#'   color_str = "group", linetype_str = "group"
#' )
#'
#' @export
plot_line <- function(data, x_str, y_str, color_str = NULL, linetype_str = NULL,
                      ...) {
  if (is.null(x_str) | is.null(y_str)) {
    stop("Must specify x_str and y_str argument.")
  }
  group_str <- color_str

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(
      x_str = x_str,
      y_str = y_str,
      color_str = color_str,
      group_str = group_str,
      linetype_str = linetype_str
    ) +
    ggplot2::geom_line(...) +
    ggplot2::labs(
      x = x_str, y = y_str, color = color_str, linetype = linetype_str
    )
  if (!is.null(color_str)) {
    if (is.character(data[[color_str]])) {
      data[[color_str]] <- as.factor(data[[color_str]])
    }
  }
  if (!is.null(linetype_str)) {
    if (is.character(data[[linetype_str]])) {
      data[[linetype_str]] <- as.factor(data[[linetype_str]])
    }
  }
  if (!is.null(color_str) && !is.null(linetype_str)) {
    group_str <- substitute(
      interaction(color_str, linetype_str),
      list(
        color_str = as.symbol(color_str),
        linetype_str = as.symbol(linetype_str)
      )
    )
    plt <- plt + ggplot2::aes(group = !!group_str)
  }
  return(plt)
}


#' Scatter plot wrapper for ggplot2.
#'
#' @description Convenient wrapper around [ggplot2::geom_point()].
#'
#' @inheritParams shared_plot_args
#' @param ... Other arguments to pass to [ggplot2::geom_point()].
#'
#' @return A ggplot scatter plot.
#'
#' @examples
#' plot_point(iris, x_str = "Sepal.Width", y_str = "Sepal.Length")
#' plot_point(
#'   iris,
#'   x_str = "Sepal.Width",
#'   y_str = "Sepal.Length",
#'   color_str = "Species"
#' )
#' plot_point(
#'   iris,
#'   x_str = "Sepal.Width",
#'   y_str = "Sepal.Length",
#'   color_str = "Species",
#'   shape_str = "Species"
#' )
#'
#' @export
plot_point <- function(data, x_str, y_str, color_str = NULL, shape_str = NULL,
                       ...) {
  if (is.null(x_str) | is.null(y_str)) {
    stop("Must specify x_str and y_str argument.")
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(
      x_str = x_str,
      y_str = y_str,
      color_str = color_str,
      shape_str = shape_str
    ) +
    ggplot2::geom_point(...) +
    ggplot2::labs(x = x_str, y = y_str, color = color_str, shape = shape_str)
  if (!is.null(color_str)) {
    if (is.character(data[[color_str]])) {
      data[[color_str]] <- as.factor(data[[color_str]])
    }
  }
  if (!is.null(shape_str)) {
    if (is.character(data[[shape_str]])) {
      data[[shape_str]] <- as.factor(data[[shape_str]])
    }
  }
  return(plt)
}

