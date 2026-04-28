# ==============================================================================
# Tests for spec getter functions
# ==============================================================================

# ==============================================================================
# Common Getters
# ==============================================================================

test_that("get_spec_columns returns columns for TableSpec", {
  spec <- TableSpec()
  cols <- get_spec_columns(spec)

  expect_true("name" %in% cols)
  expect_true("estimate" %in% cols)
  expect_true(is.character(cols))
})

test_that("get_spec_columns merges add_columns for TableSpec", {
  spec <- TableSpec() |> add_spec_columns("cv", "corr")
  cols <- get_spec_columns(spec)

  expect_true("cv" %in% cols)
  expect_true("corr" %in% cols)
})

test_that("get_spec_columns returns columns for SummarySpec", {
  spec <- SummarySpec()
  cols <- get_spec_columns(spec)

  expect_true("ofv" %in% cols)
  expect_true("pvalue" %in% cols)
  expect_true(is.character(cols))
})

test_that("get_spec_title works for both specs", {
  table_spec <- TableSpec()
  expect_equal(get_spec_title(table_spec), "Model Parameters")

  sum_spec <- SummarySpec()
  expect_equal(get_spec_title(sum_spec), "Run Summary")
})

test_that("get_spec_title returns modified title", {
  spec <- TableSpec() |> set_spec_title("Custom Title")
  expect_equal(get_spec_title(spec), "Custom Title")
})

test_that("get_spec_sigfig works for both specs", {
  table_spec <- TableSpec()
  expect_equal(get_spec_sigfig(table_spec), 3)

  sum_spec <- SummarySpec()
  expect_equal(get_spec_sigfig(sum_spec), 3)

  modified <- TableSpec() |> set_spec_sigfig(5)
  expect_equal(get_spec_sigfig(modified), 5)
})

# ==============================================================================
# TableSpec-Only Getters
# ==============================================================================

test_that("get_spec_parameter_names works", {
  spec <- TableSpec()
  opts <- get_spec_parameter_names(spec)

  expect_true(S7::S7_inherits(opts, ParameterNameOptions))
  expect_equal(opts@source, "name")

  modified <- spec |>
    set_spec_parameter_names(source = "nonmem")
  opts2 <- get_spec_parameter_names(modified)
  expect_equal(opts2@source, "nonmem")
})

test_that("get_spec_ci returns CIOptions", {
  spec <- TableSpec()
  ci <- get_spec_ci(spec)

  expect_true(S7::S7_inherits(ci, CIOptions))
  expect_equal(ci@level, 0.95)
  expect_true(ci@merge)
})

test_that("get_spec_ci returns modified CI", {
  spec <- TableSpec() |> set_spec_ci(level = 0.90)
  ci <- get_spec_ci(spec)

  expect_equal(ci@level, 0.90)
})

test_that("get_spec_sections returns a SectionOptions object", {
  spec <- TableSpec()
  sections <- get_spec_sections(spec)

  expect_true(S7::S7_inherits(sections, SectionOptions))
})

test_that("get_spec_sections returns added rules on the SectionOptions object", {
  spec <- TableSpec() |>
    set_spec_sections(kind == "THETA" ~ "Structural")

  sections <- get_spec_sections(spec)
  expect_length(sections@rules, 1)
})

test_that("get_spec_section_filter returns empty list by default", {
  expect_identical(get_spec_section_filter(TableSpec()), list())
  expect_identical(get_spec_section_filter(SummarySpec()), list())
})

test_that("get_spec_section_filter returns set exclude value", {
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "Other",
      exclude = c("Other", NA)
    )
  expect_identical(
    get_spec_section_filter(spec),
    list(exclude = c("Other", NA_character_))
  )
})

test_that("get_spec_section_filter returns set keep value", {
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "Structural",
      kind == "OMEGA" ~ "Variability",
      keep = c("Structural", "Variability")
    )
  expect_identical(
    get_spec_section_filter(spec),
    list(keep = c("Structural", "Variability"))
  )
})

test_that("section order on spec is empty by default", {
  expect_length(TableSpec()@sections@order, 0L)
})

test_that("set_spec_sections(order=) populates sections@order", {
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "A",
      kind == "OMEGA" ~ "B",
      order = c("B", "A")
    )
  expect_identical(spec@sections@order, c("B", "A"))
})

test_that("get_spec_filter returns list", {
  spec <- TableSpec()
  filters <- get_spec_filter(spec)

  expect_true(is.list(filters))
})

test_that("get_spec_filter returns added filters", {
  spec <- TableSpec() |> set_spec_filter(!fixed, diagonal)
  filters <- get_spec_filter(spec)

  expect_length(filters, 2)
})

test_that("get_spec_transforms returns named list", {
  spec <- TableSpec()
  transforms <- get_spec_transforms(spec)

  expect_true(is.list(transforms))
  expect_true("theta" %in% names(transforms))
  expect_true("omega" %in% names(transforms))
  expect_true("sigma" %in% names(transforms))
})

test_that("get_spec_variability returns list", {
  spec <- TableSpec()
  rules <- get_spec_variability(spec)

  expect_true(is.list(rules))
  expect_true(length(rules) > 0) # Has default rules
})

# ==============================================================================
# SummarySpec-Only Getters
# ==============================================================================

test_that("get_spec_time_format works", {
  spec <- SummarySpec()
  expect_equal(get_spec_time_format(spec), "seconds")

  modified <- spec |> set_spec_time_format("auto")
  expect_equal(get_spec_time_format(modified), "auto")
})

# ==============================================================================
# Type Validation
# ==============================================================================

# ==============================================================================
# Round-trip consistency
# ==============================================================================

test_that("get returns what set configured for TableSpec", {
  spec <- TableSpec() |>
    set_spec_title("Custom") |>
    set_spec_sigfig(5) |>
    set_spec_parameter_names(source = "display")

  expect_equal(get_spec_title(spec), "Custom")
  expect_equal(get_spec_sigfig(spec), 5)
  expect_equal(get_spec_parameter_names(spec)@source, "display")
})

test_that("get returns what set configured for SummarySpec", {
  spec <- SummarySpec() |>
    set_spec_title("Summary") |>
    set_spec_sigfig(4) |>
    set_spec_time_format("hours")

  expect_equal(get_spec_title(spec), "Summary")
  expect_equal(get_spec_sigfig(spec), 4)
  expect_equal(get_spec_time_format(spec), "hours")
})
