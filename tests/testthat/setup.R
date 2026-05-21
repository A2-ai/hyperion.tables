old_opts <- options(
  hyperion.config_dir = system.file(package = "hyperion.tables")
)
withr::defer(options(old_opts), testthat::teardown_env())
