test_that("plot_pairs works properly", {
  data(iris)
  X <- iris |>
    dplyr::mutate(
      Species2 = as.factor(rep(levels(Species), length.out = nrow(iris)))
    )

  expect_error(plot_pairs(data = iris))
  expect_error(
    plot_pairs(
      data = iris, columns = 1:2, color_lower = as.character(iris$Species)
    ),
    NA
  )

  plt <- plot_pairs(data = X, columns = 1:6)
  vdiffr::expect_doppelganger("plot_pairs1", plt)
  plt <- plot_pairs(data = X, columns = colnames(X))
  vdiffr::expect_doppelganger("plot_pairs1", plt)

  plt <- plot_pairs(data = X, columns = 1:6, color_lower = X$Species)
  vdiffr::expect_doppelganger("plot_pairs2", plt)
  plt <- plot_pairs(data = X, columns = 1:6, color_lower = X$Sepal.Length)
  vdiffr::expect_doppelganger("plot_pairs3", plt)

  plt <- plot_pairs(
    data = X, columns = 1:6,
    color_lower = X$Species, color_upper = X$Sepal.Length
  )
  vdiffr::expect_doppelganger("plot_pairs4", plt)
  plt <- plot_pairs(
    data = X, columns = 1:6,
    color_lower = X$Species, color_upper = X$Species2
  )
  vdiffr::expect_doppelganger("plot_pairs5", plt)
  plt <- plot_pairs(
    data = X, columns = 1:6,
    color_lower = X$Species, color_upper = X$Species
  )
  vdiffr::expect_doppelganger("plot_pairs2", plt)

  # check colors match when a factor is missing
  plt <- plot_pairs(
    data = X |> dplyr::filter(Species != "setosa"),
    columns = 1:6, color_lower = X$Species[X$Species != "setosa"]
  )
  vdiffr::expect_doppelganger("plot_pairs6", plt)
})
