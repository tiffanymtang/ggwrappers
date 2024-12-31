#' Get aesthetics layer to add to ggplot.
#'
#' @description Helper function to ignore \code{NULL} inputs when adding
#'   aesthetics to a ggplot.
#'
#' @param ... Named arguments where the name of the argument is formatted as
#'   \{aesthetic_name\}_str and the value is the name of the variable in the data
#'   frame to use for that aesthetic. For example, to specify the x-axis
#'   variable, use \code{x_str = "variable_name"}.
#'
#' @return A [ggplot2::aes()] object.
#'
#' @export
get_aesthetics <- function(...) {
  arg_list <- rlang::list2(...) |>
    purrr::compact()
  aes_list <- list()
  for (arg_name in names(arg_list)) {
    aes_name <- stringr::str_remove(arg_name, "\\_str$")
    aes_value <- arg_list[[arg_name]]
    aes_list[[aes_name]] <- substitute(.data[[val]], list(val = aes_value))
  }
  return(do.call(ggplot2::aes, aes_list))
}


#' Add color and plot theme
#'
#' @param plot A ggplot object
#' @param aesthetic One of "color" or "fill" indicating which aesthetic to add
#'   color to
#' @param discrete Logical indicating whether to use the discrete or continuous
#'   color/fill scale
#' @param palette One of 1 or 2 indicating which default color palette to use
#' @param ... Additional arguments to pass to the color/fill scale functions
#'
#' @return A ggplot object with color/plot theme added
#'
#' @keywords internal
add_theme <- function(plot, aesthetic = c("color", "fill"), discrete = TRUE,
                      palette = c("1", "2"), ...) {
  palette <- match.arg(palette)
  aesthetic <- match.arg(aesthetic, several.ok = TRUE)
  plot_theme <- getOption("ggwrappers.theme")
  if (palette == "1") {
    color_discrete_theme <- getOption("ggwrappers.color_palette_discrete")
    color_continuous_theme <- getOption("ggwrappers.color_palette_continuous")
    fill_discrete_theme <- getOption("ggwrappers.fill_palette_discrete")
    fill_continuous_theme <- getOption("ggwrappers.fill_palette_continuous")
  } else if (palette == "2") {
    color_discrete_theme <- getOption("ggwrappers.color_palette2_discrete")
    color_continuous_theme <- getOption("ggwrappers.color_palette2_continuous")
    fill_discrete_theme <- getOption("ggwrappers.fill_palette2_discrete")
    fill_continuous_theme <- getOption("ggwrappers.fill_palette2_continuous")
  }

  if (!is.null(discrete)) {
    if (discrete) {
      if ("color" %in% aesthetic) {
        if (color_discrete_theme == "default") {
          if (rlang::is_installed("vthemes")) {
            if (palette == "1") {
              plot <- plot +
                vthemes::scale_color_vmodern(discrete = TRUE, ...)
            } else {
              plot <- plot +
                vthemes::scale_color_vmodern(
                  discrete = TRUE, palette = "viridis", viridis_option = "D", ...
                )
            }
          }
        } else {
          plot <- plot + color_discrete_theme
        }
      }
      if ("fill" %in% aesthetic) {
        if (fill_discrete_theme == "default") {
          if (rlang::is_installed("vthemes")) {
            if (palette == "1") {
              plot <- plot +
                vthemes::scale_fill_vmodern(discrete = TRUE, ...)
            } else {
              plot <- plot +
                vthemes::scale_fill_vmodern(
                  discrete = TRUE, palette = "viridis", viridis_option = "D", ...
                )
            }
          }
        } else {
          plot <- plot + fill_discrete_theme
        }
      }
    } else {
      if ("color" %in% aesthetic) {
        if (color_continuous_theme == "default") {
          if (rlang::is_installed("vthemes")) {
            if (palette == "1") {
              plot <- plot +
                vthemes::scale_color_vmodern(discrete = FALSE, ...)
            } else {
              plot <- plot +
                vthemes::scale_color_vmodern(
                  discrete = FALSE, palette = "viridis", viridis_option = "D", ...
                )
            }
          }
        } else {
          plot <- plot + color_continuous_theme
        }
      }
      if ("fill" %in% aesthetic) {
        if (fill_continuous_theme == "default") {
          if (rlang::is_installed("vthemes")) {
            if (palette == "1") {
              plot <- plot +
                vthemes::scale_fill_vmodern(discrete = FALSE, ...)
            } else {
              plot <- plot +
                vthemes::scale_fill_vmodern(
                  discrete = FALSE, palette = "viridis", viridis_option = "D", ...
                )
            }
          }
        } else {
          plot <- plot + fill_continuous_theme
        }
      }
    }
  }

  if (identical(plot_theme, "default")) {
    if (rlang::is_installed("vthemes")) {
      plot_theme <- vthemes::theme_vmodern()
    } else {
      plot_theme <- ggplot2::theme_classic()
    }
  }
  plot <- plot + plot_theme

  return(plot)
}


#' Add color to axis text labels in ggplot
#'
#' @description Helper function to add color to axis text labels in a ggplot
#'   object.
#'
#' @param plot A ggplot object
#' @param labels Axis text labels
#' @param label_colors Data vector to use for coloring axis text labels
#' @param axis One of "x" or "y" indicating which axis.
#'
#' @return A ggplot object with the axis text labels colored.
#'
#' @keywords internal
add_axis_text_colors <- function(plot, labels, label_colors = NULL,
                                 axis = c("x", "y")) {
  lab_x <- NULL  # to fix no visible binding for global variable error
  lab_y <- NULL
  lab_color <- NULL

  axis <- match.arg(axis)
  if (!is.null(label_colors)) {
    if (length(ggplot2::ggplot_build(plot)$layout$panel_params) > 1) {
      stop("Colors cannot be added to axis text labels when facets are used.")
    }
    if (identical(axis, "y")) {
      axis_labs <- lapply(
        rev(ggplot2::ggplot_build(plot)$layout$panel_params),
        function(x) x$y$get_labels()
      ) |>
        purrr::reduce(c)  # y axis plot labels from bottom to top
    } else if (identical(axis, "x")) {
      axis_labs <- lapply(
        ggplot2::ggplot_build(plot)$layout$panel_params,
        function(x) x$x$get_labels()
      ) |>
        purrr::reduce(c)  # x axis plot labels from left to right
    }
    labs_df <- dplyr::left_join(
      x = data.frame(
        lab = axis_labs
      ),
      y = data.frame(
        lab_x = 1,
        lab_y = 1,
        lab_color = label_colors,
        lab = labels
      ),
      by = "lab"
    )
    plot <- plot +
      ggplot2::geom_point(
        ggplot2::aes(x = lab_x, y = lab_y, color = lab_color),
        size = -1,
        data = labs_df
      )
    # get color and add color legend
    lab_colors <- get_custom_color_palette(labs_df$lab_color)
    if (is.factor(label_colors)) {
      if (identical(axis, "y")) {
        scale_values <- rev(unique(lab_colors))
        scale_labels <- rev(unique(labs_df$lab_color))
      } else {
        scale_values <- unique(lab_colors)
        scale_labels <- unique(labs_df$lab_color)
      }
      plot <- plot +
        ggplot2::guides(
          color = ggplot2::guide_legend(override.aes = list(size = 3))
        ) +
        ggplot2::scale_color_manual(
          values = scale_values, labels = scale_labels
        )
    } else {
      plot <- plot + viridis::scale_colour_viridis(discrete = FALSE)
    }
    if (identical(axis, "x")) {
      plot <- plot +
        ggplot2::theme(axis.text.x = ggplot2::element_text(color = lab_colors))
    } else if (identical(axis, "y")) {
      plot <- plot +
        ggplot2::theme(axis.text.y = ggplot2::element_text(color = lab_colors))
    }
  }
  return(plot)
}


#' Converts data vector to color vector
#'
#' @param color_labels Vector of labels to convert to colors.
#' @param return_palette Logical indicating whether or not to return the
#'   color palette used to convert data to colors.
#'
#' @returns If \code{return_palette = FALSE}, returns a vector of colors. If
#'   \code{return_palette = TRUE}, returns a list of two:
#'   \describe{
#'   \item{colors}{Vector of colors.}
#'   \item{palette}{Color palette used to convert data to colors.}
#'   }
#'
#' @keywords internal
get_custom_color_palette <- function(color_labels, return_palette = FALSE) {
  if (is.factor(color_labels)) {
    custom_colors <- scales::hue_pal()(length(unique(color_labels)))
    colors_out <- custom_colors[color_labels]
  } else {
    custom_colors <- scales::col_numeric(
      palette = "viridis", domain = c(min(color_labels), max(color_labels))
    )
    colors_out <- custom_colors(color_labels)
  }
  if (return_palette) {
    return(list(colors = colors_out, palette = custom_colors))
  } else {
    return(colors_out)
  }
}
