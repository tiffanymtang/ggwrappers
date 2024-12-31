.onLoad <- function(libname, pkgname) {

  op <- options()
  op.ggwrappers <- list(
    ggwrappers.theme = "default",
    ggwrappers.color_palette_discrete = "default",
    ggwrappers.color_palette_continuous = "default",
    ggwrappers.color_palette2_discrete = "default",
    ggwrappers.color_palette2_continuous = "default",
    ggwrappers.fill_palette_discrete = "default",
    ggwrappers.fill_palette_continuous = "default",
    ggwrappers.fill_palette2_discrete = "default",
    ggwrappers.fill_palette2_continuous = "default"
  )
  toset <- !(names(op.ggwrappers) %in% names(op))
  if(any(toset)) options(op.ggwrappers[toset])

  invisible()
}
