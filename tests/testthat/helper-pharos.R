local_fixture_dir <- function(subdir = "onecmt", env = parent.frame()) {
  path <- system.file(
    "extdata", "models", subdir,
    package = "hyperion.tables", mustWork = TRUE
  )
  withr::local_dir(path, .local_envir = env)
}
