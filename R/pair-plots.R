#' Custom ggpair plots
#'
#' @description Creates pair plots, extending functionality beyond
#'   [GGally::ggpairs()] such as the ability to add two different color schemes
#'   (one for the lower triangular subpanels and another for the upper
#'   triangular subpanels).
#'
#' @inheritParams shared_plot_args
#' @param columns Vector of column indicies or column names to plot.
#' @param column_labels (Optional) Label names to be displayed on strips.
#' @param color_lower (Optional) Data vector to use as colors for lower ggplot
#'   panels.
#' @param color_upper (Optional) Data vector to use as colors for upper ggplot
#'   panels.
#' @param color_label_lower Character string. Label for color legend title (used
#'   in lower ggplot panels).
#' @param color_label_upper Character string for color_upper legend title (used
#'   in upper ggplot panels).
#' @param color_scheme_lower (Optional) Vector of colors to set manual color
#'   scheme corresponding to color_lower argument (i.e., the color scheme in the
#'   lower panels). If \code{NULL} (default), viridis color scheme is used.
#' @param color_scheme_upper (Optional) Vector of colors to set manual color
#'   scheme corresponding to color_upper argument (i.e., the color scheme in the
#'   upper panels). If \code{NULL} (default), viridis color scheme is used.
#' @param point_size Point size for [ggplot2::geom_point()].
#' @param point_alpha Alpha value for [ggplot2::geom_point()].
#' @param cor_text_size Size of correlation text.
#' @param show_upper Logical. Should we show subplots in upper panels?
#' @param subsample Proportion of rows to subsample and plot.
#' @param title Character string. Title of plot.
#' @param drop Logical. Whether or not to drop factors with no observations.
#' @param ... Additional arguments to pass to [vthemes::theme_vmodern()].
#'
#' @return A [GGally::ggpairs] object.
#'
#' @examples
#' plot_pairs(data = iris, columns = 1:ncol(iris), color_lower = iris$Species)
#' plot_pairs(
#'   data = iris, columns = 1:4,
#'   color_lower = iris$Species, color_upper = rnorm(nrow(iris))
#' )
#'
#' @importFrom rlang .data
#' @export
plot_pairs <- function(data, columns, column_labels = NULL,
                       color_lower = NULL, color_upper = NULL,
                       color_label_lower = "", color_label_upper = "",
                       color_scheme_lower = NULL, color_scheme_upper = NULL,
                       title = "",
                       point_size = .5, point_alpha = .5, cor_text_size = 3.5,
                       subsample = 1, show_upper = TRUE, drop = TRUE, ...) {

  # convert character color vectors to factor for ggplot color/fill aesthetic
  if (!is.null(color_lower)) {
    if (is.character(color_lower)) {
      color_lower <- as.factor(color_lower)
    }
  }
  if (!is.null(color_upper)) {
    if (is.character(color_upper)) {
      color_upper <- as.factor(color_upper)
    }
  }
  # check if color_lower and color_upper are identical
  if (!is.null(color_lower) & !is.null(color_upper)) {
    if (identical(color_lower, color_upper)) {
      color_upper <- NULL
    }
  }

  # adding labels for colors
  plt_df <- as.data.frame(data) |>
    dplyr::mutate(
      .color_lower = color_lower,
      .color_upper = color_upper
    )

  # subsample points
  if (subsample != 1) {
    plt_df <- dplyr::slice_sample(plt_df, prop = subsample, replace = FALSE)
  }

  # check if show correlations in upper panel
  if (show_upper) {
    plot_cor <- GGally::wrap("cor", size = cor_text_size, stars = FALSE)
  } else {
    plot_cor <- "blank"
  }

  # get columns for plotting
  if (!is.numeric(columns)) {
    columns <- which(colnames(data) %in% columns)
  }
  # get column labels for plotting
  if (is.null(column_labels)) {
    column_labels <- colnames(data[, columns, drop = FALSE])
  }

  # helper variables
  plot_points <- GGally::wrap("points", size = point_size, alpha = point_alpha)
  plot_density <- GGally::wrap("densityDiag", alpha = .5)

  if (is.null(color_lower) & is.null(color_upper)) {  # no colors
    plt <- GGally::ggpairs(
      data = plt_df,
      columns = columns,
      diag = list(continuous = plot_density),
      lower = list(continuous = plot_points),
      upper = list(continuous = plot_cor),
      title = title,
      columnLabels = column_labels
    )
  } else if (is.null(color_upper)) {  # one color
    # grab subplot for legend
    if (length(columns) == 1) {
      if (is.factor(color_lower)) {
        legend <- c(1, 1)
      } else {
        legend <- NULL
      }
    } else {
      legend_plt <- data.frame(.color = color_lower, legend_x = 1) |>
        ggplot2::ggplot() +
        ggplot2::aes(x = legend_x, y = legend_x, color = .color) +
        ggplot2::geom_point() +
        ggplot2::labs(color = color_label_lower)
      if (is.null(color_scheme_lower)) {
        legend_plt <- legend_plt +
          vthemes::scale_color_vmodern(
            discrete = !is.numeric(color_lower), drop = drop
          )
      } else {
        legend_plt <- legend_plt +
          ggplot2::scale_color_manual(values = color_scheme_lower, drop = drop)
      }
      legend <- GGally::grab_legend(legend_plt)
    }

    if (is.factor(color_lower)) {
      upper_ls <- list(continuous = plot_cor)
    } else {
      if (show_upper) {
        upper_ls <- list(continuous = plot_points)
      } else {
        upper_ls <- list(continuous = "blank")
      }
    }
    plt <- GGally::ggpairs(
      data = plt_df,
      columns = columns,
      mapping = ggplot2::aes(color = .color_lower),
      diag = list(continuous = plot_density),
      lower = list(continuous = plot_points),
      upper = upper_ls,
      title = title,
      legend = legend,
      columnLabels = column_labels
    )

    # change color palette for all panels
    for (i in 1:plt$nrow) {
      for (j in 1:plt$ncol) {
        if (is.null(color_scheme_lower)) {
          plt[i, j] <- plt[i, j] +
            vthemes::scale_color_vmodern(
              discrete = !is.numeric(color_lower), drop = drop
            ) +
            vthemes::scale_fill_vmodern(
              discrete = !is.numeric(color_lower), drop = drop
            )
        } else {
          plt[i, j] <- plt[i, j] +
            ggplot2::scale_color_manual(
              values = color_scheme_lower, drop = drop
            ) +
            ggplot2::scale_fill_manual(
              values = color_scheme_lower, drop = drop
            )
        }
      }
    }

    plt <- plt +
      ggplot2::labs(color = color_label_lower, fill = color_label_lower)
  } else {
    # make lower scatter plots and color by color for the ggpairs plots
    lowerContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(
          x = .data[[x_str]], y = .data[[y_str]], color = .color_lower
        ) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha)
      return(p)
    }

    # make upper scatter plots and color by color_upper for the ggpairs plots
    upperContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(
          x = .data[[x_str]], y = .data[[y_str]], color = .color_upper
        ) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha)
      return(p)
    }

    # make upper boxplots and color by color_upper for the ggpairs plots
    upperCombo <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      if (is.factor(data[[x_str]])) {
        p <- ggplot2::ggplot(data) +
          ggplot2::aes(
            x = .data[[x_str]], y = .data[[y_str]], fill = .color_upper
          ) +
          ggplot2::geom_boxplot()
      } else {
        p <- ggplot2::ggplot(data) +
          ggplot2::aes(
            x = .data[[y_str]], y = .data[[x_str]], fill = .color_upper
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::coord_flip()
      }
      return(p)
    }

    # make upper bar plots and color by color_upper for the ggpairs plots
    upperDiscrete <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(x = .data[[x_str]], fill = .color_upper) +
        ggplot2::facet_grid(.data[[y_str]] ~ .) +
        ggplot2::geom_bar()
      return(p)
    }

    # number of color factors
    nfactors <- is.factor(color_lower) + is.factor(color_upper)

    if (length(columns) == 1) {  # in this case, plot density
      if (nfactors == 0) {
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          title = title,
          legend = c(1, 1),
          columnLabels = column_labels
        ) +
          ggplot2::labs(color = color_label_lower, fill = color_label_lower)
      } else {
        if (is.factor(color_lower)) {
          plt_df$.plt_color <- color_lower
          legend_lab <- color_label_lower
          color_scheme <- color_scheme_lower
        } else {
          plt_df$.plt_color <- color_upper
          legend_lab <- color_label_upper
          color_scheme <- color_scheme_upper
        }
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = .plt_color),
          diag = list(continuous = plot_density),
          title = title,
          legend = c(1, 1),
          columnLabels = column_labels
        ) +
          ggplot2::labs(color = legend_lab, fill = legend_lab)
        if (is.null(color_scheme)) {
          discrete <- !is.numeric(plt_df$.plt_color)
          plt[1, 1] <- plt[1, 1] +
            vthemes::scale_color_vmodern(discrete = discrete, drop = drop) +
            vthemes::scale_fill_vmodern(discrete = discrete, drop = drop)
        } else {
          plt[1, 1] <- plt[1, 1] +
            ggplot2::scale_color_manual(values = color_scheme, drop = drop) +
            ggplot2::scale_fill_manual(values = color_scheme, drop = drop)
        }
      }

    } else {
      # grab color and color_upper legends
      legend_plt_df <- plt_df |>
        dplyr::mutate(
          legend_x = data[, columns[1]],
          legend_y = data[, columns[2]]
        )
      legend_plt1 <- ggplot2::ggplot(legend_plt_df) +
        ggplot2::geom_point(
          ggplot2::aes(x = legend_x, y = legend_y, color = .color_lower)
        ) +
        ggplot2::labs(color = color_label_lower, fill = color_label_lower)
      legend_plt2 <- ggplot2::ggplot(legend_plt_df) +
        ggplot2::geom_point(
          ggplot2::aes(x = legend_x, y = legend_y, color = .color_upper)
        ) +
        ggplot2::labs(color = color_label_upper, fill = color_label_upper)
      if (is.null(color_scheme_lower)) {
        legend_plt1 <- legend_plt1 +
          vthemes::scale_color_vmodern(
            discrete = !is.numeric(color_lower), drop = drop
          )
      } else {
        legend_plt1 <- legend_plt1 +
          ggplot2::scale_color_manual(values = color_scheme_lower, drop = drop)
      }
      if (is.null(color_scheme_upper)) {
        legend_plt2 <- legend_plt2 +
          vthemes::scale_color_vmodern(
            discrete = !is.numeric(color_upper),
            palette = "viridis", viridis_option = "D",
            drop = drop
          )
      } else {
        legend_plt2 <- legend_plt2 +
          ggplot2::scale_color_manual(values = color_scheme_upper, drop = drop)
      }
      legend1 <- GGally::grab_legend(
        legend_plt1 + ggplot2::theme(legend.position = "bottom")
      )
      legend2 <- GGally::grab_legend(
        legend_plt2 + ggplot2::theme(legend.position = "bottom")
      )

      # make ggpairs
      if (nfactors == 0) {
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = .color_lower),
          diag = list(continuous = plot_density),
          lower = list(continuous = plot_points),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = column_labels
        )
      } else if (nfactors == 2) {
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = .color_lower),
          diag = list(continuous = plot_density),
          lower = list(continuous = plot_points, combo = "box_no_facet"),
          upper = list(
            continuous = upperContinuous,
            combo = upperCombo,
            discrete = upperDiscrete
          ),
          title = title,
          columnLabels = column_labels
        )
      } else {
        if (is.factor(color_lower)) {
          plt_df$.plt_color <- color_lower
        } else {
          plt_df$.plt_color <- color_upper
        }
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = .plt_color),
          diag = list(continuous = plot_density),
          lower = list(continuous = lowerContinuous),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = column_labels
        )
      }

      # change color scheme in all panels
      for (i in 1:plt$nrow) {
        for (j in 1:plt$ncol) {
          plt_fill <- plt[i, j]$labels$fill
          plt_col <- plt[i, j]$labels$colour
          if (!is.null(plt_fill)) {
            if (plt_fill %in% names(plt_df)) {
              if (all(as.character(plt_df[, plt_fill]) ==
                      as.character(color_lower))) {
                if (is.null(color_scheme_lower)) {
                  plt[i, j] <- plt[i, j] +
                    vthemes::scale_fill_vmodern(
                      discrete = !is.numeric(color_lower), drop = drop
                    )
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_fill_manual(
                      values = color_scheme_lower, drop = drop
                    )
                }
              } else {
                if (is.null(color_scheme_upper)) {
                  plt[i, j] <- plt[i, j] +
                    vthemes::scale_fill_vmodern(
                      discrete = !is.numeric(color_upper),
                      palette = "viridis", viridis_option = "D", drop = drop
                    )
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_fill_manual(
                      values = color_scheme_upper, drop = drop
                    )
                }
              }
            }
          }

          if (!is.null(plt_col)) {
            if (plt_col %in% names(plt_df)) {
              ptr <- FALSE
              if (is.numeric(plt_df[, plt_col]) & is.numeric(color_lower)) {
                ptr <- all(plt_df[, plt_col] == color_lower)
              } else if (is.factor(plt_df[, plt_col]) & is.factor(color_lower)) {
                ptr <- all(as.character(plt_df[, plt_col]) ==
                             as.character(color_lower))
              }
              if (ptr) {
                if (is.null(color_scheme_lower)) {
                  plt[i, j] <- plt[i, j] +
                    vthemes::scale_color_vmodern(
                      discrete = !is.numeric(color_lower), drop = drop
                    )
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_color_manual(
                      values = color_scheme_lower, drop = drop
                    )
                }
              } else {
                if (is.null(color_scheme_upper)) {
                  plt[i, j] <- plt[i, j] +
                    vthemes::scale_color_vmodern(
                      discrete = !is.numeric(color_upper),
                      palette = "viridis", viridis_option = "D", drop = drop
                    )
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_color_manual(
                      values = color_scheme_upper, drop = drop
                    )
                }
              }
            }
          }
        }
      }
    }

    if (length(columns) != 1) {
      plt <- cowplot::plot_grid(
        GGally::ggmatrix_gtable(plt + vthemes::theme_vmodern(...)),
        legend1, legend2,
        nrow = 3, rel_heights = c(10, 1, 1)
      )
    } else {
      plt <- plt + vthemes::theme_vmodern(...)
    }
  }
  return(plt)
}
