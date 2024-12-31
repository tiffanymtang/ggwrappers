#' Plot hierarchical clustering dendrograms
#'
#' @description Run hierarchical clustering on the given data matrix and
#'   plot the resulting dendrogram using a pretty theme. Also allows for
#'   coloring of the leaf nodes.
#'
#' @param data Data matrix or data.frame on which to perform hierarchical
#'   clustering.
#' @param leaf_labels (Optional) Text labels for leaf nodes (e.g.,
#'   class/outcome labels).
#' @param leaf_colors (Optional) Data vector to use for coloring leaf nodes.
#' @param dist_metric Distance metric for clustering (see [stats::dist()]).
#' @param dist_matrix (Optional) Distance matrix for clustering. Must provide
#'   either \code{dist_metric} or \code{dist_matrix}.
#' @param linkage Type of linkage for hierarchical clustering (see
#'   [stats::hclust()]).
#' @param text_size Numeric; size of text for leaf node labels.
#' @param title Character string. Title of plot.
#'
#' @returns A list of three:
#' \describe{
#' \item{plot}{A ggplot object of the dendrogram.}
#' \item{hclust}{Output of [stats::hclust()]}
#' \item{dend}{Hierarchical clustering dendrogram data. See output of
#'   [ggdendro::dendro_data()].}
#' }
#'
#' @examples
#' out <- plot_hclust(data = iris[, -5], leaf_labels = iris$Species,
#'                   leaf_colors = iris$Species)
#' out$plot
#'
#' @export
plot_hclust <- function(data,
                        leaf_labels = rownames(data), leaf_colors = NULL,
                        dist_metric = "euclidean", dist_matrix = NULL,
                        linkage = "ward.D", text_size = 10, title = NULL) {
  y <- NULL  # to fix no visible binding for global variable error
  x <- NULL
  color <- NULL

  leaf_labels <- leaf_labels
  data <- as.matrix(data)
  if (sum(is.na(data)) > 0) {
    stop("NAs found in data")
  }

  # distance matrix
  if (is.null(dist_matrix)) {
    Dmat <- stats::dist(data, method = dist_metric)
  } else {
    if (!("dist" %in% class(dist_matrix))) {
      Dmat <- stats::as.dist(dist_matrix)
    } else {
      Dmat <- dist_matrix
    }
  }

  # hierarchical clustering
  hclust_out <- stats::hclust(Dmat, method = linkage)
  hclust_dend <- stats::as.dendrogram(hclust_out)

  if (!is.null(leaf_colors)) {  # annotate tree leaves
    palette_out <- get_custom_color_palette(leaf_colors, TRUE)
    lab_colors <- palette_out$colors
    color_palette <- palette_out$palette
    lab_colors <- lab_colors[stats::order.dendrogram(hclust_dend)]
    lab_df <- data.frame(x = 0, y = 0, color = leaf_colors)
  } else {
    lab_colors <- "black"
  }

  # get leaf labels
  if (is.null(leaf_labels)) {
    hclust_dend <- hclust_dend |>
      dendextend::set_labels(
        rep("------", ncol(as.matrix(Dmat)))
      )
  } else {
    hclust_dend <- hclust_dend |>
      dendextend::set_labels(
        leaf_labels[stats::order.dendrogram(hclust_dend)]
      )
  }

  if (is.null(title)) {
    title <- sprintf(
      "Hierarchical Clustering: %s Linkage, %s Distance", linkage, dist_metric
    )
  }

  # convert to ggplot object
  hclust_dend <- ggdendro::dendro_data(hclust_dend)
  hclust_plt <- suppressWarnings(
    suppressMessages(
      ggdendro::ggdendrogram(hclust_dend) +
        ggplot2::labs(title = title, color = "Label") +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(hclust_dend$labels$label),
          labels = hclust_dend$labels$label,
          expand = c(0, 0)
        ) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(
            color = lab_colors, size = text_size
          )
        )
    )
  )

  # add legend
  if (!is.null(leaf_colors)) {
    if (is.factor(leaf_colors)) {
      hclust_plt <- hclust_plt +
        ggplot2::geom_point(
          ggplot2::aes(x = x, y = y, color = color),
          data = lab_df,
          size = -1
        ) +
        ggplot2::guides(
          color = ggplot2::guide_legend(override.aes = list(size = 3))
        ) +
        ggplot2::scale_color_manual(values = color_palette)
    } else {
      hclust_plt <- hclust_plt +
        ggplot2::geom_point(
          ggplot2::aes(x = x, y = y, color = color),
          data = lab_df,
          size = -1
        ) +
        viridis::scale_colour_viridis(discrete = F)
    }
  }
  return(list(plot = hclust_plt, hclust = hclust_out, dend = hclust_dend))
}
