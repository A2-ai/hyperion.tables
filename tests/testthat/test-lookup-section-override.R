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

  snapshot_gt(table_gt, "lookup-gt")
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
  # Inline overrides TVCL with a different value -> expect a warning at
  # setter time, and inline wins.
  expect_warning(
    spec <- TableSpec() |>
      set_spec_sections(
        kind == "THETA" ~ "Structural",
        kind == "OMEGA" ~ "Variability",
        kind == "SIGMA" ~ "Residual",
        file = testthat::test_path("lookup-section.toml"),
        parameters = list("Inline Override" = "TVCL")
      ),
    "conflict.*TVCL"
  )

  df <- apply_table_spec(params, spec, mod_info)
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

test_that("set_spec_sections ignores parameters/file on SummarySpec", {
  expect_no_error(
    SummarySpec() |>
      set_spec_sections(
        TRUE ~ "All",
        parameters = list("Custom" = "run001.mod")
      )
  )
  expect_no_error(
    SummarySpec() |>
      set_spec_sections(file = "anything.toml")
  )
})

test_that("get_spec_parameter_sections returns merged assignments", {
  expect_warning(
    spec <- TableSpec() |>
      set_spec_sections(
        file = testthat::test_path("lookup-section.toml"),
        parameters = list("X" = "TVCL")
      ),
    "conflict.*TVCL"
  )
  result <- get_spec_parameter_sections(spec)
  # Inline wins for TVCL; file entries for TVV and "OM1 (TVCL)" remain
  expect_equal(result$X, "TVCL")
  expect_equal(result$`Custom: Volume Group`, "TVV")
  expect_equal(result$`Custom: IIV-CL Group`, "OM1 (TVCL)")
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
    set_spec_sections(parameters = list("Inline Only" = "TVCL"))

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
      parameters = list("Custom: Clearance Group" = "TVCL")
    )

  expect_warning(
    df <- apply_table_spec(params, spec, info),
    NA  # no warning expected
  )
  expect_equal(df$section[df$user_name == "TVCL"], "Custom: Clearance Group")
})

# T3
test_that("filter_keep matching order drops TOML-injected sections", {
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

  # File adds "Custom: Clearance Group"; filter keeps only the listed sections.
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "Structural",
      kind == "OMEGA" ~ "Variability",
      kind == "SIGMA" ~ "Residual",
      file = testthat::test_path("lookup-section.toml"),
      order = c("Structural", "Variability", "Residual"),
      keep = c("Structural", "Variability", "Residual")
    )

  htable <- params |>
    apply_table_spec(spec, info) |>
    add_summary_info(summary(mod)) |>
    make_parameter_table(output = "data")
  # TVCL was reassigned to "Custom: Clearance Group" which is not in the
  # keep filter list, so it is dropped.
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
    set_spec_sections(parameters = list("X" = "NOT_A_PARAM"))

  expect_warning(
    apply_table_spec(params, spec, info),
    "Inline `parameters` section override.*NOT_A_PARAM"
  )
})

# T5
test_that("SummarySpec section_filter warns when label not in data", {
  local_fixture_dir()
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  tree <- hyperion::get_model_lineage()
  spec <- SummarySpec() |>
    set_spec_sections(
      "nonexistent_tag" %in% tags ~ "Foo",
      exclude = "Foo"
    )

  # Rule introduces "Foo" but no models match -> data has no "Foo" rows.
  # Apply-time warns that the label wasn't present in the data.
  expect_warning(
    apply_summary_spec(tree, spec),
    "section_filter exclude label.*not present"
  )
})

# Issue 2 follow-up — same parameter listed under multiple sections
test_that("set_spec_sections rejects parameter assigned to multiple sections", {
  expect_error(
    TableSpec() |>
      set_spec_sections(
        parameters = list("A" = "TVCL", "B" = "TVCL")
      ),
    "multiple sections|TVCL"
  )
})

# Same parameter listed twice within one section
test_that("set_spec_sections rejects duplicate parameter within one section", {
  expect_error(
    TableSpec() |>
      set_spec_sections(parameters = list("A" = c("TVCL", "TVCL"))),
    "multiple sections|TVCL"
  )
})

# Validation of bad shapes
test_that("set_spec_sections rejects malformed parameters arg", {
  # not a list
  expect_error(
    TableSpec() |>
      set_spec_sections(parameters = c(TVCL = "A")),
    "Invalid `parameters`"
  )
  # unnamed list
  expect_error(
    TableSpec() |>
      set_spec_sections(parameters = list("TVCL")),
    "named"
  )
  # empty character vector value
  expect_error(
    TableSpec() |>
      set_spec_sections(parameters = list("A" = character(0))),
    "non-empty"
  )
  # NA inside parameter list
  expect_error(
    TableSpec() |>
      set_spec_sections(parameters = list("A" = NA_character_)),
    "non-empty"
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
    set_spec_sections(parameters = list("A" = c("TVCL", "NOT_A_PARAM")))

  expect_warning(
    df <- apply_table_spec(params, spec, info),
    "NOT_A_PARAM"
  )
  expect_equal(df$section[df$user_name == "TVCL"], "A")
})
