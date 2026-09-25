# helper-files.R -- Temporary paths that are removed when the test ends.

withr_tempfile <- function(fileext = "", env = parent.frame()) {
  path <- tempfile(fileext = fileext)
  do.call(on.exit, list(bquote(unlink(.(path), recursive = TRUE)), add = TRUE),
          envir = env)
  path
}
