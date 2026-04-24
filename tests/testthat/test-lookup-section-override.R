test_that("lookup TOML overrides per-parameter section assignments", {
  testthat::skip_if_not_installed("tomledit")
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
    title = "Lookup Section Override",
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    )
  ) |>
    set_spec_lookup(testthat::test_path("lookup-section.toml"))

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-lookup-section-override-gt")
})
