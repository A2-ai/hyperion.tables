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
    title = "Lookup Section Override"
  ) |>
    set_spec_sections(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual error",
      TRUE ~ "Other",
      file = testthat::test_path("lookup-section.toml")
    )

  table_gt <- params |>
    apply_table_spec(spec, mod_info) |>
    add_summary_info(mod_sum) |>
    make_parameter_table()

  snapshot_gt(table_gt, "param-lookup-section-override-gt")
})

test_that("inline parameters override warns on conflict with file", {
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

  # Lookup TOML assigns TVCL -> "Custom: Clearance Group" (see fixture).
  # Inline overrides TVCL with a different value -> expect a warning, and
  # inline wins.
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "Structural",
      kind == "OMEGA" ~ "Variability",
      kind == "SIGMA" ~ "Residual",
      file = testthat::test_path("lookup-section.toml"),
      parameters = c(TVCL = "Inline Override")
    )

  expect_warning(
    df <- apply_table_spec(params, spec, mod_info),
    "conflict.*TVCL"
  )
  expect_equal(
    df$section[df$user_name == "TVCL"],
    "Inline Override"
  )
  # TVV came from file only — unchanged
  expect_equal(
    df$section[df$user_name == "TVV"],
    "Custom: Volume Group"
  )
  # TVKA: spec rule applies (no file/inline entry)
  expect_equal(
    df$section[df$user_name == "TVKA"],
    "Structural"
  )
})

test_that("set_spec_sections rejects parameters/file on SummarySpec", {
  expect_error(
    SummarySpec() |>
      set_spec_sections(
        TRUE ~ "All",
        parameters = c(run001.mod = "Custom")
      ),
    "TableSpec"
  )
  expect_error(
    SummarySpec() |>
      set_spec_sections(file = "anything.toml"),
    "TableSpec"
  )
})

test_that("get_spec_parameter_sections returns inline + file pair", {
  spec <- TableSpec() |>
    set_spec_sections(
      file = testthat::test_path("lookup-section.toml"),
      parameters = c(TVCL = "X")
    )
  result <- get_spec_parameter_sections(spec)
  expect_equal(result$parameters, c(TVCL = "X"))
  expect_equal(basename(result$file), "lookup-section.toml")
})

# T1
test_that("inline parameters work without rules or file", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  params <- hyperion::get_parameters(mod)
  info <- hyperion::get_model_parameter_info(mod)

  spec <- TableSpec() |>
    set_spec_sections(parameters = c(TVCL = "Inline Only"))

  df <- apply_table_spec(params, spec, info)
  expect_equal(df$section[df$user_name == "TVCL"], "Inline Only")
  # other rows: no rule matched, no override → NA
  expect_true(all(is.na(df$section[df$user_name != "TVCL"])))
})

# T2
test_that("file + parameters that agree do not warn", {
  testthat::skip_if_not_installed("tomledit")
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  params <- hyperion::get_parameters(mod)
  info <- hyperion::get_model_parameter_info(mod)

  # Lookup TOML assigns TVCL -> "Custom: Clearance Group"
  # Pass identical inline override; expect no conflict warning.
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "OMEGA" ~ "Variability",
      file = testthat::test_path("lookup-section.toml"),
      parameters = c(TVCL = "Custom: Clearance Group")
    )

  expect_warning(
    df <- apply_table_spec(params, spec, info),
    NA  # no warning expected
  )
  expect_equal(df$section[df$user_name == "TVCL"], "Custom: Clearance Group")
})

# T3
test_that("section_order keep_only drops TOML-injected sections when omitted", {
  testthat::skip_if_not_installed("tomledit")
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  params <- hyperion::get_parameters(mod)
  info <- hyperion::get_model_parameter_info(mod)

  # File adds "Custom: Clearance Group"; order omits it with keep_only=TRUE.
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "Structural",
      kind == "OMEGA" ~ "Variability",
      kind == "SIGMA" ~ "Residual",
      file = testthat::test_path("lookup-section.toml")
    ) |>
    set_spec_section_order(
      c("Structural", "Variability", "Residual"),
      keep_only = TRUE
    )

  htable <- params |>
    apply_table_spec(spec, info) |>
    add_summary_info(summary(mod)) |>
    make_parameter_table(output = "data")
  # TVCL was reassigned to "Custom: Clearance Group" which is not in the
  # order list, so keep_only drops it.
  expect_false("TVCL" %in% htable@data$user_name)
  expect_true(all(
    htable@data$section %in% c("Structural", "Variability", "Residual")
  ))
})

# T4
test_that("inline parameters warn when no row matches", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  params <- hyperion::get_parameters(mod)
  info <- hyperion::get_model_parameter_info(mod)

  spec <- TableSpec() |>
    set_spec_sections(parameters = c(NOT_A_PARAM = "X"))

  expect_warning(
    apply_table_spec(params, spec, info),
    "Inline `parameters` section override.*NOT_A_PARAM"
  )
})

# T5
test_that("SummarySpec section_filter without rules runs (and warns on typo)", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  tree <- hyperion::get_model_lineage(model_dir)
  spec <- SummarySpec() |>
    set_spec_section_filter(exclude = "Foo")

  # No rules -> section column populated as NA -> filter has nothing to
  # match. We expect a warning that the label wasn't present in the data.
  expect_warning(
    apply_summary_spec(tree, spec),
    "section_filter exclude label.*not present"
  )
})

# Issue 2 follow-up
test_that("set_spec_sections rejects duplicate parameter names", {
  expect_error(
    TableSpec() |>
      set_spec_sections(parameters = c(TVCL = "A", TVCL = "B")),
    "duplicate name"
  )
})

# Issue 7
test_that("set_spec_sections(file=) errors on missing file", {
  expect_error(
    TableSpec() |>
      set_spec_sections(file = tempfile(fileext = ".toml")),
    "No such file|cannot|does not exist"
  )
})

# Issue 8 — NA-asymmetric conflict detection
test_that("inline NA conflicting with file value still warns", {
  testthat::skip_if_not_installed("tomledit")
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  params <- hyperion::get_parameters(mod)
  info <- hyperion::get_model_parameter_info(mod)

  # File assigns TVCL → "Custom: Clearance Group"; inline overrides with NA.
  # `(NA, "x")` must trip the conflict warning.
  spec <- TableSpec() |>
    set_spec_sections(
      file = testthat::test_path("lookup-section.toml"),
      parameters = c(TVCL = NA_character_)
    )

  expect_warning(
    apply_table_spec(params, spec, info),
    "conflict.*TVCL"
  )
})

# Issue 9 — partial-match warning names only the unmatched key
test_that("partial-match override applies what it can and warns about the rest", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  params <- hyperion::get_parameters(mod)
  info <- hyperion::get_model_parameter_info(mod)

  spec <- TableSpec() |>
    set_spec_sections(parameters = c(TVCL = "A", NOT_A_PARAM = "B"))

  expect_warning(
    df <- apply_table_spec(params, spec, info),
    "NOT_A_PARAM"
  )
  expect_equal(df$section[df$user_name == "TVCL"], "A")
})
