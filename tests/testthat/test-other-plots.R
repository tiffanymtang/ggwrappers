test_that("plot_horizontal_dotplot works properly", {
  df <- data.frame(ID = rep(paste0("ID", 1:3), 2),
                   Group = rep(paste0("Group", LETTERS[1:2]), each = 3),
                   Value = c(1, 2, 3, -1, 0, 4))
  out <- plot_horizontal_dotplot(df, x_str = "Value", y_str = "ID", color_str = "Group")
  vdiffr::expect_doppelganger("plot_horizontal_dotplot1", out)
})
