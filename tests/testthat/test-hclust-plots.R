test_that("plot_hclust works properly", {
  data(iris)
  X <- iris |>
    dplyr::select(-Species)

  out <- plot_hclust(data = X)
  expect_equal(names(out), c("plot", "hclust", "dend"))
  vdiffr::expect_doppelganger("plot_hclust1", out$plot)

  out <- plot_hclust(data = X, leaf_colors = iris$Species)
  vdiffr::expect_doppelganger("plot_hclust2", out$plot)
})
