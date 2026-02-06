test_that("variability_rules warn when referencing dropped columns", {
  expect_warning(
    TableSpec(
      drop_columns = "cv",
      variability_rules = variability_rules(
        !is.na(cv) ~ "CV"
      )
    ),
    "variability_rules reference dropped columns"
  )
})

test_that("apply_formatting uses variability_rules", {
  testthat::skip_if_not_installed("gt")
  testthat::skip_if_not_installed("webshot2")

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

  spec <- TableSpec(
    display_transforms = list(omega = "cv"),
    parameter_names = ParameterNameOptions(source = "display"),
    title = "Model Parameters",
    variability_rules = variability_rules(
      fixed ~ "(Fixed)",
      !is.na(cv) & cv != 0 ~ sprintf("(CV = %s%%)", cv),
      TRUE ~ NA_character_
    )
  )

  table_data <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "data")

  gt_tbl <- render_to_gt(table_data)
  snapshot_gt(gt_tbl, "variability-rules-gt")
  ft_tbl <- render_to_flextable(table_data)
  snapshot_flextable(ft_tbl, "variability-rules-ft")
})
