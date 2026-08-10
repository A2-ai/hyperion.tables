# Regression tests for name resolution (F3, F9), covering the behavior that
# moved upstream in 0.5.0. These build a temporary model whose $THETA comments
# are edited, so the config root is pointed at a writable copy of the fixtures.

# Replicate the package config tree under a temp root and edit run001.mod's
# $THETA block. Returns the path to the edited model; restores options on exit.
with_edited_run001 <- function(edit_theta_line, code) {
  pkg_root <- system.file(package = "hyperion.tables")
  root <- withr::local_tempdir()
  file.copy(file.path(pkg_root, "extdata"), root, recursive = TRUE)
  for (f in c("lookup.toml", "pharos.toml")) {
    src <- file.path(pkg_root, f)
    if (file.exists(src)) file.copy(src, root)
  }
  withr::local_options(hyperion.config_dir = root)

  mp <- file.path(root, "extdata", "models", "onecmt", "run001.mod")
  lines <- readLines(mp)
  # The TVV theta line: "(0, 30)   ; 2. TVV (L)"
  lines <- sub("^\\(0, 30\\).*$", edit_theta_line, lines)
  writeLines(lines, mp)

  mod <- hyperion::read_model(mp)
  code(mod)
}

test_that("F3: nonmem_name resolves correctly when all parameters are commented", {
  skip_if_not(nzchar(system.file(package = "hyperion.tables")))
  with_edited_run001("(0, 30)   ; 2. TVV (L)", function(mod) {
    info <- hyperion::get_model_parameter_info(mod)
    p <- hyperion::get_parameters(mod)
    e <- apply_table_spec(
      p, TableSpec(parameter_names = ParameterNameOptions(source = "display")), info
    )
    thetas <- e[e$kind == "THETA", ]
    expect_equal(thetas$nonmem_name, c("THETA1", "THETA2", "THETA3"))
  })
})

test_that("F3: one uncommented parameter does not corrupt the others' nonmem_name", {
  skip_if_not(nzchar(system.file(package = "hyperion.tables")))
  # Strip the TVV comment, leaving the bare "(0, 30)"
  with_edited_run001("(0, 30)", function(mod) {
    info <- hyperion::get_model_parameter_info(mod)
    p <- hyperion::get_parameters(mod)
    e <- apply_table_spec(
      p, TableSpec(parameter_names = ParameterNameOptions(source = "display")), info
    )
    # Every THETA still resolves to its NONMEM name, not its display name.
    thetas <- e[e$kind == "THETA", ]
    expect_equal(thetas$nonmem_name, c("THETA1", "THETA2", "THETA3"))
    omegas <- e[e$kind == "OMEGA", ]
    expect_true(all(grepl("^OMEGA", omegas$nonmem_name)))
  })
})

test_that("F9: a THETA sharing a name with a SIGMA does not crash the pipeline", {
  skip_if_not(nzchar(system.file(package = "hyperion.tables")))
  # Name the second theta "Additive", matching the SIGMA(2,2) comment.
  with_edited_run001("(0, 30)   ; 2. Additive", function(mod) {
    info <- hyperion::get_model_parameter_info(mod)
    p <- hyperion::get_parameters(mod)
    # Should not crash with a raw "NA transform at index" error; instead it
    # warns and displays untransformed values.
    expect_warning(
      e <- apply_table_spec(p, TableSpec(), info),
      "No transform found"
    )
    expect_s3_class(e, "data.frame")
    expect_equal(nrow(e), nrow(p))
  })
})
