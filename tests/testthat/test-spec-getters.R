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

test_that("get_spec_name_source works", {
  spec <- TableSpec()
  expect_equal(get_spec_name_source(spec), "name")

  modified <- spec |> set_spec_name_source("nonmem_name")
  expect_equal(get_spec_name_source(modified), "nonmem_name")
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

test_that("get_spec_sections returns list", {
  spec <- TableSpec()
  sections <- get_spec_sections(spec)

  expect_true(is.list(sections))
})

test_that("get_spec_sections returns added sections", {
  spec <- TableSpec() |>
    set_spec_sections(kind == "THETA" ~ "Structural")

  sections <- get_spec_sections(spec)
  expect_length(sections, 1)
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

test_that("common getters reject non-spec objects", {
  expect_error(get_spec_columns(list()), "must be a TableSpec or SummarySpec")
  expect_error(get_spec_title(NULL), "must be a TableSpec or SummarySpec")
  expect_error(get_spec_sigfig(42), "must be a TableSpec or SummarySpec")
})

test_that("TableSpec-only getters reject SummarySpec", {
  sum_spec <- SummarySpec()

  expect_error(get_spec_name_source(sum_spec), "must be a TableSpec")
  expect_error(get_spec_ci(sum_spec), "must be a TableSpec")
  expect_error(get_spec_sections(sum_spec), "must be a TableSpec")
  expect_error(get_spec_filter(sum_spec), "must be a TableSpec")
  expect_error(get_spec_transforms(sum_spec), "must be a TableSpec")
  expect_error(get_spec_variability(sum_spec), "must be a TableSpec")
})

test_that("SummarySpec-only getters reject TableSpec", {
  table_spec <- TableSpec()

  expect_error(get_spec_time_format(table_spec), "must be a SummarySpec")
})

# ==============================================================================
# Round-trip consistency
# ==============================================================================

test_that("get returns what set configured for TableSpec", {
  spec <- TableSpec() |>
    set_spec_title("Custom") |>
    set_spec_sigfig(5) |>
    set_spec_name_source("display")

  expect_equal(get_spec_title(spec), "Custom")
  expect_equal(get_spec_sigfig(spec), 5)
  expect_equal(get_spec_name_source(spec), "display")
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
