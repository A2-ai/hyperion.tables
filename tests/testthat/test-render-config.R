test_that("renderers accept TableSpec render overrides", {
  testthat::skip_if_not_installed("gt")
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("webshot2")
  testthat::skip_if_not_installed("htmltools")

  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    "run001",
    package = "hyperion"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  params <- hyperion::get_parameters(model_dir)
  mod_info <- hyperion::get_model_parameter_info(model_dir)
  mod_sum <- hyperion::get_model_summary(model_dir)

  spec <- TableSpec(
    display_transforms = list(omega = "cv"),
    name_source = "display",
    title = "Model Parameters",
    n_sigfig = 4,
    ci = CIOptions(
      merge = TRUE,
      pattern = "(%s; %s)",
      missing_text = "NA"
    ),
    missing_text = "NA",
    missing_apply_to = "numeric",
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    )
  )

  table_data <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "data")

  gt_tbl <- render_gt(table_data)
  ft_tbl <- render_flextable(table_data)

  snapshot_gt(gt_tbl, "render-config-gt")
  snapshot_flextable(ft_tbl, "render-config-ft")
})
