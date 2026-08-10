# ==============================================================================
# Tests for spec presets
# ==============================================================================

# ==============================================================================
# parameter_table_spec() - spec object
# ==============================================================================

test_that("parameter_table_spec returns a configured TableSpec", {
  spec <- parameter_table_spec(title = "Model Parameters")

  expect_true(S7::S7_inherits(spec, TableSpec))
  expect_equal(get_spec_transforms(spec)$omega, "cv")
  expect_equal(get_spec_parameter_names(spec)@source, "display")
  expect_length(spec@sections@rules, 4)
  expect_equal(spec@title, "Model Parameters")
})

test_that("parameter_table_spec assigns default section labels in order", {
  spec <- parameter_table_spec(title = "Model Parameters")

  labels <- unname(vapply(
    spec@sections@rules,
    function(q) rlang::quo_get_expr(q)[[3]],
    character(1)
  ))
  expect_equal(
    labels,
    c(
      "Structural model parameters",
      "Interindividual variability",
      "Residual error",
      "Other"
    )
  )
})

test_that("parameter_table_spec injects custom section labels into rules", {
  spec <- parameter_table_spec(
    theta_section_label = "Fixed effects",
    omega_section_label = "Random effects",
    sigma_section_label = "Residual variability",
    other_section_label = "Misc",
    title = "Model Parameters"
  )

  labels <- unname(vapply(
    spec@sections@rules,
    function(q) rlang::quo_get_expr(q)[[3]],
    character(1)
  ))
  expect_equal(
    labels,
    c("Fixed effects", "Random effects", "Residual variability", "Misc")
  )
})

test_that("parameter_table_spec result can be customized with modifiers", {
  spec <- parameter_table_spec(title = "Model Parameters") |>
    set_spec_sigfig(4) |>
    set_spec_sections(kind == "OMEGA" & !diagonal ~ "Covariances")

  expect_equal(get_spec_sigfig(spec), 4)
  expect_length(spec@sections@rules, 5)
})

test_that("parameter_table_spec defaults the title to the TableSpec default", {
  spec <- parameter_table_spec()

  expect_true(S7::S7_inherits(spec, TableSpec))
  expect_equal(spec@title, "Model Parameters")
})

test_that("parameter_table_spec empty title gives a titleless spec", {
  expect_equal(parameter_table_spec(title = "")@title, "")
})

test_that("parameter_table_spec rejects non-string section labels", {
  expect_error(
    parameter_table_spec(theta_section_label = 1),
    "`theta_section_label` must be a single character string"
  )
  expect_error(
    parameter_table_spec(omega_section_label = c("a", "b")),
    "`omega_section_label` must be a single character string"
  )
  expect_error(
    parameter_table_spec(sigma_section_label = NULL),
    "`sigma_section_label` must be a single character string"
  )
  expect_error(
    parameter_table_spec(other_section_label = NA_character_),
    "`other_section_label` must be a single character string"
  )
})

test_that("parameter_table_spec rejects invalid titles", {
  expect_error(
    parameter_table_spec(title = 5),
    "`title` must be a single character string or NULL"
  )
  expect_error(
    parameter_table_spec(title = c("a", "b")),
    "`title` must be a single character string or NULL"
  )
})

# ==============================================================================
# parameter_table_spec() - rendered output
# ==============================================================================

test_that("parameter table: parameter_table_spec defaults", {
  local_fixture_dir()
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))

  params <- hyperion::get_parameters(mod)
  mod_info <- hyperion::get_model_parameter_info(mod)
  mod_sum <- summary(mod)

  spec <- parameter_table_spec(title = "Model Parameters")

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-preset-default-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-preset-default-ft")
})

test_that("parameter table: parameter_table_spec custom sections", {
  local_fixture_dir()
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))

  params <- hyperion::get_parameters(mod)
  mod_info <- hyperion::get_model_parameter_info(mod)
  mod_sum <- summary(mod)

  spec <- parameter_table_spec(
    theta_section_label = "Fixed effects",
    omega_section_label = "Between-subject variability",
    sigma_section_label = "Residual variability",
    title = "PK Parameter Estimates"
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-preset-custom-gt")
})
