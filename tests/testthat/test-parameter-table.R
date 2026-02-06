test_that("parameter table: run001 basic spec", {
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
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    )
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run001-basic-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run001-ft")
})

test_that("parameter table: run001 shows fixed", {
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
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    add_columns = "fixed"
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run001-fixed-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run001-fixed-ft")
})

test_that("parameter table: run002 shows empty fixed", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run002.mod"))

  params <- hyperion::get_parameters(mod)
  mod_info <- hyperion::get_model_parameter_info(mod)
  mod_sum <- summary(mod)

  spec <- TableSpec(
    display_transforms = list(omega = "cv"),
    parameter_names = ParameterNameOptions(source = "display"),
    title = "Model Parameters",
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    )
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run002-no-fixed-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run002-no-fixed-ft")

  spec@add_columns <- "fixed"
  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run002-fixed-gt")

  spec@add_columns <- "fixed"
  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run002-fixed-ft")
})

test_that("parameter table: run003 drop ci column", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run003.mod"))

  params <- hyperion::get_parameters(mod)
  mod_info <- hyperion::get_model_parameter_info(mod)
  mod_sum <- summary(mod)

  spec <- TableSpec(
    display_transforms = list(omega = "cv"),
    parameter_names = ParameterNameOptions(source = "display"),
    title = "Model Parameters",
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    drop_columns = "ci"
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run003-drop-ci-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run003-drop-ci-ft")
})

test_that("parameter table: run003 drop ci_low column", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run003.mod"))

  params <- hyperion::get_parameters(mod)
  mod_info <- hyperion::get_model_parameter_info(mod)
  mod_sum <- summary(mod)

  spec <- TableSpec(
    display_transforms = list(omega = "cv"),
    parameter_names = ParameterNameOptions(source = "display"),
    title = "Model Parameters",
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    drop_columns = "ci_low"
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run003-drop-ci_low-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run003-drop-ci_low-ft")
})

test_that("parameter table: run003 drop ci_high column", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run003.mod"))

  params <- hyperion::get_parameters(mod)
  mod_info <- hyperion::get_model_parameter_info(mod)
  mod_sum <- summary(mod)

  spec <- TableSpec(
    display_transforms = list(omega = "cv"),
    parameter_names = ParameterNameOptions(source = "display"),
    title = "Model Parameters",
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    drop_columns = "ci_high"
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run003-drop-ci_high-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run003-drop-ci_high-ft")
})

test_that("parameter table: run003 summary footnote only", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run003.mod"))

  params <- hyperion::get_parameters(mod)
  mod_info <- hyperion::get_model_parameter_info(mod)
  mod_sum <- summary(mod)

  spec <- TableSpec(
    display_transforms = list(omega = "cv"),
    parameter_names = ParameterNameOptions(source = "display"),
    title = "Model Parameters",
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    drop_columns = "ci",
    footnote_order = "summary_info"
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run003-summary-fn-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run003-summary-fn-ft")
})

test_that("parameter table: run003 drop footnotes", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run003.mod"))

  params <- hyperion::get_parameters(mod)
  mod_info <- hyperion::get_model_parameter_info(mod)
  mod_sum <- summary(mod)

  spec <- TableSpec(
    display_transforms = list(omega = "cv"),
    parameter_names = ParameterNameOptions(source = "display"),
    title = "Model Parameters",
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    drop_columns = "ci",
    footnote_order = NULL
  )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-run003-no-fn-gt")

  table_ft <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")

  snapshot_flextable(table_ft, "param-run003-no-fn-ft")
})

test_that("parameter table: run001 no spec", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))

  params <- hyperion::get_parameters(mod)

  expect_error(
    make_parameter_table(params),
    "TableSpec not found. Run apply_table_spec"
  )
})

test_that("parameter table: base display", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    parameter_names = ParameterNameOptions(source = "display"),
    drop_columns = "rse",
    title = paste(model_run, "Parameters")
  )

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"

  mod_sum <- summary(mod)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()
  snapshot_gt(table, "param-base-gt")
})

test_that("parameter table: display name source", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    parameter_names = ParameterNameOptions(source = "display"),
    drop_columns = "rse",
    title = paste(model_run, "Parameters")
  )

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"
  info@sigma$`SIGMA(1,1)`@display <- "Proportional Error"
  info@sigma$`SIGMA(2,2)`@display <- "Additive Error"

  mod_sum <- summary(mod)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()
  snapshot_gt(table, "param-display-gt")
})

test_that("parameter table: nonmem name source", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    parameter_names = ParameterNameOptions(source = "nonmem"),
    drop_columns = "rse",
    title = paste(model_run, "Parameters")
  )

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"

  mod_sum <- summary(mod)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()
  snapshot_gt(table, "param-nonmem-name-gt")
})

test_that("parameter table: nonmem source without theta append", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    drop_columns = "rse",
    title = paste(model_run, "Parameters")
  ) |>
    set_spec_parameter_names(source = "nonmem", append_omega_with_theta = FALSE)

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"

  mod_sum <- summary(mod)

  table_gt <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()
  snapshot_gt(table_gt, "param-nonmem-no-theta-gt")

  table_ft <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table(output = "flextable")
  snapshot_flextable(table_ft, "param-nonmem-no-theta-ft")
})

test_that("parameter table: description column", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    parameter_names = ParameterNameOptions(source = "display"),
    add_columns = "description",
    drop_columns = "rse",
    title = paste(model_run, "Parameters")
  )

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"

  mod_sum <- summary(mod)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()
  snapshot_gt(table, "param-description-gt")
})

test_that("parameter table: drop unit column", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    parameter_names = ParameterNameOptions(source = "display"),
    drop_columns = "unit",
    title = paste(model_run, "Parameters")
  )

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"

  mod_sum <- summary(mod)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()
  snapshot_gt(table, "param-drop-unit-gt")
})

test_that("parameter table: drop unit and shrinkage columns", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    ),
    parameter_names = ParameterNameOptions(source = "display"),
    drop_columns = c("unit", "shrinkage"),
    title = paste(model_run, "Parameters")
  )

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"

  mod_sum <- summary(mod)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()
  snapshot_gt(table, "param-drop-unit-shrink-gt")
})

test_that("parameter table: structural-only filter", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"

  sp_spec <- TableSpec(
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      TRUE ~ "Other"
    ),
    row_filter = filter_rules(
      kind == "THETA"
    ),
    drop_columns = "shrinkage"
  )

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(sp_spec, info) |>
    make_parameter_table()
  snapshot_gt(table, "param-structural-only-gt")
})

test_that("parameter table: random effects only", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  info <- hyperion::get_model_parameter_info(mod, lookup_path)
  info@sigma$`SIGMA(1,1)`@parameterization <- "Proportional"

  re_spec <- TableSpec(
    sections = section_rules(
      kind == "OMEGA" ~ "Random Effect Parameters",
      kind == "SIGMA" ~ "Residual Error",
      TRUE ~ "Other"
    ),
    row_filter = filter_rules(
      kind != "THETA"
    ),
    drop_columns = "unit"
  )

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(re_spec, info) |>
    make_parameter_table()
  snapshot_gt(table, "param-random-effects-gt")
})

test_that("parameter table: 70% CI", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run003"
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    ci = CIOptions(level = 0.7),
    n_sigfig = 3
  )

  mod_sum <- summary(mod)
  info <- hyperion::get_model_parameter_info(mod)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()
  snapshot_gt(table, "param-ci-70-gt")
})

test_that("parameter table: summary info without condition number", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    ci = CIOptions(level = 0.7),
    n_sigfig = 3,
    parameter_names = ParameterNameOptions(source = "display")
  )

  mod_sum <- summary(mod)
  info <- hyperion::get_model_parameter_info(mod, lookup_path)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum, show_cond_num = FALSE) |>
    make_parameter_table()
  snapshot_gt(table, "param-sum-no-cond-gt")
})

test_that("parameter table: summary info without condition number or OFV", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    ci = CIOptions(level = 0.7),
    n_sigfig = 3,
    parameter_names = ParameterNameOptions(source = "display")
  )

  mod_sum <- summary(mod)
  info <- hyperion::get_model_parameter_info(mod, lookup_path)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum, show_cond_num = FALSE, show_ofv = FALSE) |>
    make_parameter_table()
  snapshot_gt(table, "param-sum-no-cond-ofv-gt")
})

test_that("parameter table: summary info without method", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run003"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    ci = CIOptions(level = 0.7),
    n_sigfig = 3,
    parameter_names = ParameterNameOptions(source = "display")
  )

  mod_sum <- summary(mod)
  info <- hyperion::get_model_parameter_info(mod, lookup_path)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum, show_method = FALSE) |>
    make_parameter_table()
  snapshot_gt(table, "param-sum-no-method-gt")
})

test_that("symbol + fixed shows nicely", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  model_run <- "run001"
  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  lookup_path <- system.file("lookup.toml", package = "hyperion.tables")
  mod <- hyperion::read_model(file.path(model_dir, paste0(model_run, ".mod")))

  spec <- TableSpec(
    columns = c("name", "symbol", "fixed"),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other"
    )
  )

  mod_sum <- summary(mod)
  info <- hyperion::get_model_parameter_info(mod, lookup_path)

  table <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table, "param-symbol-fixed-gt")
})
