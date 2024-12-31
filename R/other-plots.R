#' Plot horizontal dot plot.
#'
#' @description Creates a horizontal dot plot, as an alternative to grouped bar
#'   charts.
#'
#' @inheritParams plot_point
#' @param line_size Size of line, representing range of values.
#' @param line_color Color of line, representing range of values.
#' @param point_size Size of point.
#'
#' @return A ggplot object
#'
#' @examples
#' df <- data.frame(ID = rep(paste0("ID", 1:3), 2),
#'                  Group = rep(paste0("Group", LETTERS[1:2]), each = 3),
#'                  Value = c(1, 2, 3, -1, 0, 4))
#' plot_horizontal_dotplot(df, x_str = "Value", y_str = "ID", color_str = "Group")
#'
#' @export
plot_horizontal_dotplot <- function(data, x_str, y_str, color_str = NULL,
                                    line_size = 1, line_color = "darkgray",
                                    point_size = 8, ...) {

  if (is.factor(data[[y_str]])) {
    y_order <- levels(data[[y_str]])
  } else {
    y_order <- levels(factor(data[[y_str]]))
  }

  range_df <- data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(y_str))) |>
    dplyr::summarise(
      min = min(.data[[x_str]]),
      max = max(.data[[x_str]])
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(y_str),
        ~ factor(as.character(.x), levels = y_order)
      )
    )

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(
      x_str = x_str, y_str = y_str, color_str = color_str
    ) +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = min, xend = max, y = .data[[y_str]], yend = .data[[y_str]]
      ),
      data = range_df,
      inherit.aes = FALSE,
      linewidth = line_size,
      color = line_color
    ) +
    ggplot2::geom_point(size = point_size, ...) +
    ggplot2::labs(x = x_str, y = y_str, color = color_str)
  if (!is.null(color_str)) {
    if (is.character(data[[color_str]])) {
      data[[color_str]] <- as.factor(data[[color_str]])
    }
    plt <- add_theme(plt, discrete = !is.numeric(data[[color_str]]))
  } else {
    plt <- add_theme(plt)
  }
  plt <- plt +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major.y = ggplot2::element_line(color = "grey80"),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    )
  return(plt)
}
