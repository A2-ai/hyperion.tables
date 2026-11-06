# ==============================================================================
# Tests for spec modifier functions
# ==============================================================================

# ==============================================================================
# Column Operations
# ==============================================================================

test_that("add_spec_columns appends to TableSpec", {
  spec <- TableSpec()
  modified <- spec |> add_spec_columns("cv", "corr")

  expect_true("cv" %in% modified@add_columns)
  expect_true("corr" %in% modified@add_columns)
  # Original unchanged
  expect_null(spec@add_columns)
})

test_that("add_spec_columns expands ci alias for TableSpec", {
  spec <- TableSpec()
  modified <- spec |> add_spec_columns("ci")

  expect_equal(modified@add_columns, c("ci_low", "ci_high"))
})

test_that("add_spec_columns appends to SummarySpec", {
  spec <- SummarySpec()
  modified <- spec |> add_spec_columns("estimation_time")

  expect_true("estimation_time" %in% get_spec_columns(modified))
  # Original unchanged
  expect_false("estimation_time" %in% get_spec_columns(spec))
})

test_that("add_spec_columns rejects invalid columns", {
  spec <- TableSpec()
  expect_error(
    add_spec_columns(spec, "bogus_column"),
    "@add_columns must be in"
  )

  sum_spec <- SummarySpec()
  expect_error(
    add_spec_columns(sum_spec, "invalid"),
    "@add_columns must be in"
  )
})

test_that("drop_spec_columns adds to drop list", {
  spec <- TableSpec()
  modified <- spec |> drop_spec_columns("unit", "symbol")

  expect_true("unit" %in% modified@drop_columns)
  expect_true("symbol" %in% modified@drop_columns)
  expect_null(spec@drop_columns)
})

test_that("drop_spec_columns allows comparison suffixes for TableSpec", {
  spec <- TableSpec()
  modified <- spec |> drop_spec_columns("estimate_1", "estimate_2", "ci_left")

  expect_equal(modified@drop_columns, c("estimate_1", "estimate_2", "ci_left"))
})

test_that("set_spec_columns replaces columns", {
  spec <- TableSpec()
  modified <- spec |> set_spec_columns("name", "estimate", "rse")

  expect_equal(modified@columns, c("name", "estimate", "rse"))
})

test_that("set_spec_columns expands ci alias for TableSpec", {
  spec <- TableSpec()
  modified <- spec |> set_spec_columns("name", "estimate", "ci")

  expect_equal(modified@columns, c("name", "estimate", "ci_low", "ci_high"))
})

# ==============================================================================
# Common Setters
# ==============================================================================

test_that("set_spec_title works for both specs", {
  table_spec <- TableSpec() |> set_spec_title("Test Title")
  expect_equal(table_spec@title, "Test Title")

  sum_spec <- SummarySpec() |> set_spec_title("Summary Title")
  expect_equal(sum_spec@title, "Summary Title")
})

test_that("set_spec_sigfig works for both specs", {
  table_spec <- TableSpec() |> set_spec_sigfig(4)
  expect_equal(table_spec@n_sigfig, 4)

  sum_spec <- SummarySpec() |> set_spec_sigfig(5)
  expect_equal(sum_spec@n_sigfig, 5)
})

test_that("set_spec_sigfig validates input", {
  spec <- TableSpec()
  expect_error(set_spec_sigfig(spec, 2.5), "positive whole number")
  expect_error(set_spec_sigfig(spec, 0), "positive whole number")
  expect_error(set_spec_sigfig(spec, -1), "positive whole number")
})

test_that("set_spec_ofv_decimals works for both specs", {
  table_spec <- TableSpec() |> set_spec_ofv_decimals(2)
  expect_equal(table_spec@n_decimals_ofv, 2)

  sum_spec <- SummarySpec() |> set_spec_ofv_decimals(1)
  expect_equal(sum_spec@n_decimals_ofv, 1)
})

test_that("set_spec_ofv_decimals validates input", {
  spec <- TableSpec()
  expect_error(set_spec_ofv_decimals(spec, -1), "non-negative whole number")
  expect_error(set_spec_ofv_decimals(spec, 1.5), "non-negative whole number")
})

test_that("set_spec_hide_empty works", {
  spec <- TableSpec() |> set_spec_hide_empty(FALSE)
  expect_false(spec@hide_empty_columns)
})

test_that("set_spec_pvalue works", {
  spec <- TableSpec() |> set_spec_pvalue(threshold = 0.001, scientific = TRUE)
  expect_equal(spec@pvalue_threshold, 0.001)
  expect_true(spec@pvalue_scientific)
})

test_that("set_spec_pvalue allows clearing threshold", {
  spec <- TableSpec() |>
    set_spec_pvalue(threshold = 0.001, scientific = TRUE) |>
    set_spec_pvalue(threshold = NULL)

  expect_null(spec@pvalue_threshold)
})

test_that("set_spec_mutate stores named rules and makes them selectable", {
  spec <- SummarySpec() |>
    set_spec_mutate(stage = if ("final" %in% tags) "Final" else NA)
  expect_named(get_spec_mutate(spec), "stage")
  expect_true(rlang::is_quosure(get_spec_mutate(spec)$stage))

  # A computed column passes column validation like a built-in one.
  spec <- set_spec_columns(spec, "based_on", "ofv", "stage")
  expect_equal(get_spec_columns(spec), c("based_on", "ofv", "stage"))
  expect_error(set_spec_columns(SummarySpec(), "stage"), "@columns must be in")

  # Rules add by name; the same name replaces; overwrite drops the rest.
  spec <- set_spec_mutate(spec, flag = TRUE)
  expect_named(get_spec_mutate(spec), c("stage", "flag"))
  spec <- set_spec_mutate(spec, stage = "x")
  expect_named(get_spec_mutate(spec), c("flag", "stage"))
  expect_equal(rlang::quo_get_expr(get_spec_mutate(spec)$stage), "x")
  # Dropping a rule a selected column depends on fails validation.
  expect_error(set_spec_mutate(spec, only = 1, overwrite = TRUE), "@columns must be in")
  fresh <- SummarySpec() |>
    set_spec_mutate(a = 1, b = 2) |>
    set_spec_mutate(only = 1, overwrite = TRUE)
  expect_named(get_spec_mutate(fresh), "only")

  expect_error(set_spec_mutate(SummarySpec(), "final" %in% tags), "must be named")
  expect_error(set_spec_mutate(TableSpec(), a = 1), "<SummarySpec>")
  expect_error(get_spec_mutate(TableSpec()), "<SummarySpec>")
})

test_that("set_spec_section_header stores a function or NULL", {
  expect_null(get_spec_section_header(SummarySpec()))
  fn <- function(section, rows) data.frame(model = section)
  spec <- set_spec_section_header(SummarySpec(), fn)
  expect_identical(get_spec_section_header(spec), fn)
  expect_null(get_spec_section_header(set_spec_section_header(spec, NULL)))
  expect_error(set_spec_section_header(SummarySpec(), "x"), "must be a function")
  expect_error(set_spec_section_header(TableSpec(), fn), "<SummarySpec>")
  expect_error(get_spec_section_header(TableSpec()), "<SummarySpec>")
})

test_that("set_spec_pvalue sets direction on a SummarySpec only", {
  expect_equal(SummarySpec()@pvalue_direction, "forward")
  spec <- SummarySpec() |> set_spec_pvalue(direction = "both")
  expect_equal(spec@pvalue_direction, "both")
  expect_equal(get_spec_pvalue(spec)$direction, "both")
  expect_null(get_spec_pvalue(TableSpec())$direction)

  expect_error(
    SummarySpec() |> set_spec_pvalue(direction = "backward"),
    "@pvalue_direction must be"
  )
  expect_error(
    TableSpec() |> set_spec_pvalue(direction = "both"),
    "applies to a <SummarySpec> only"
  )
})

test_that("set_spec_footnotes works", {
  spec <- TableSpec() |> set_spec_footnotes(c("abbreviations", "equations"))
  expect_equal(spec@footnote_order, c("abbreviations", "equations"))

  spec_null <- TableSpec() |> set_spec_footnotes(NULL)
  expect_null(spec_null@footnote_order)
})

test_that("set_spec_footnotes validates sections", {
  expect_error(
    TableSpec() |> set_spec_footnotes(c("invalid")),
    "footnote_order must be in"
  )
})

test_that("set_spec_sections exclude mode works for both specs", {
  table_spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "Other",
      exclude = "Other"
    )
  expect_identical(table_spec@sections@filter, list(exclude = "Other"))

  sum_spec <- SummarySpec() |>
    set_spec_sections(
      "base" %in% tags ~ "Other",
      exclude = c("Other", NA)
    )
  expect_identical(
    sum_spec@sections@filter,
    list(exclude = c("Other", NA_character_))
  )
})

test_that("set_spec_sections keep mode stores positive list", {
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "Structural",
      kind == "OMEGA" ~ "Variability",
      keep = c("Structural", "Variability")
    )
  expect_identical(
    spec@sections@filter,
    list(keep = c("Structural", "Variability"))
  )
})

test_that("set_spec_sections rejects exclude + keep together", {
  expect_error(
    TableSpec() |>
      set_spec_sections(
        kind == "THETA" ~ "Other",
        kind == "OMEGA" ~ "Structural",
        exclude = "Other",
        keep = "Structural"
      ),
    "mutually exclusive"
  )
})

test_that("set_spec_sections clears section filter with character(0)", {
  spec <- SummarySpec() |>
    set_spec_sections(
      "base" %in% tags ~ "Other",
      exclude = "Other"
    ) |>
    set_spec_sections(keep = character(0))
  expect_length(spec@sections@filter, 0L)
})

test_that("set_spec_section_filter is defunct", {
  expect_error(
    set_spec_section_filter(TableSpec()),
    class = "lifecycle_error_deprecated"
  )
})

test_that("set_spec_sections(order=) stores order config", {
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "A",
      kind == "OMEGA" ~ "B",
      order = c("B", "A")
    )
  expect_identical(spec@sections@order, c("B", "A"))
})

test_that("set_spec_sections(order = character(0)) clears the order", {
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "A",
      kind == "OMEGA" ~ "B",
      order = c("B", "A")
    ) |>
    set_spec_sections(order = character(0))
  expect_length(spec@sections@order, 0L)
})

# ==============================================================================
# TableSpec-Only Setters
# ==============================================================================

test_that("set_spec_parameter_names sets source", {
  spec <- TableSpec() |> set_spec_parameter_names(source = "nonmem")
  expect_equal(spec@parameter_names@source, "nonmem")
})

test_that("set_spec_parameter_names validates source", {
  expect_error(
    TableSpec() |> set_spec_parameter_names(source = "invalid"),
    "@source must be"
  )
})

test_that("set_spec_ci works", {
  spec <- TableSpec() |> set_spec_ci(level = 0.90, pattern = "(%s, %s)")
  expect_equal(spec@ci@level, 0.90)
  expect_equal(spec@ci@pattern, "(%s, %s)")
})

test_that("set_spec_missing works", {
  spec <- TableSpec() |> set_spec_missing("-", apply_to = "numeric")
  expect_equal(spec@missing_text, "-")
  expect_equal(spec@missing_apply_to, "numeric")
})

test_that("set_spec_transforms works", {
  spec <- TableSpec() |>
    set_spec_transforms(theta = "estimate", omega = c("cv", "rse"))
  expect_equal(spec@display_transforms$theta, "estimate")
  expect_equal(spec@display_transforms$omega, c("cv", "rse"))
})

test_that("set_spec_transforms validates columns", {
  expect_error(
    TableSpec() |> set_spec_transforms(theta = "invalid"),
    "@display_transforms values must be in"
  )
})

# ==============================================================================
# TableSpec Rule Modifiers
# ==============================================================================

test_that("set_spec_sections appends by default", {
  spec <- TableSpec() |>
    set_spec_sections(kind == "THETA" ~ "Structural") |>
    set_spec_sections(kind == "OMEGA" ~ "IIV")

  expect_length(spec@sections@rules, 2)
})

test_that("set_spec_sections overwrites when specified", {
  spec <- TableSpec() |>
    set_spec_sections(kind == "THETA" ~ "First") |>
    set_spec_sections(kind == "OMEGA" ~ "Second", overwrite = TRUE)

  expect_length(spec@sections@rules, 1)
})

test_that("set_spec_sections accepts `sections =` for both spec types", {
  table_spec <- TableSpec() |>
    set_spec_sections(
      sections = section_rules(
        kind == "THETA" ~ "Structural",
        kind == "OMEGA" ~ "IIV"
      )
    )
  expect_length(table_spec@sections@rules, 2)

  summary_spec <- SummarySpec() |>
    set_spec_sections(
      sections = section_rules("base" %in% tags ~ "Base Models")
    )
  expect_length(summary_spec@sections@rules, 1)
})

test_that("TableSpec()/SummarySpec() require SectionOptions objects at construction", {
  table_from_sections <- TableSpec(
    sections = SectionOptions(
      rules = section_rules(kind == "THETA" ~ "Structural")
    )
  )
  expect_length(table_from_sections@sections@rules, 1)

  summary_from_sections <- SummarySpec(
    sections = SectionOptions(
      rules = section_rules("base" %in% tags ~ "Base Models")
    )
  )
  expect_length(summary_from_sections@sections@rules, 1)

  expect_error(
    TableSpec(sections = section_rules(kind == "THETA" ~ "Structural")),
    "sections.*SectionOptions"
  )
  expect_error(
    SummarySpec(sections = section_rules("base" %in% tags ~ "Base Models")),
    "sections.*SectionOptions"
  )
})

test_that("set_spec_sections combines `sections =` and `...` (sections first)", {
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "SIGMA" ~ "Residual",
      sections = section_rules(
        kind == "THETA" ~ "Structural",
        kind == "OMEGA" ~ "IIV"
      )
    )
  expect_length(spec@sections@rules, 3)
  labels <- unname(vapply(
    spec@sections@rules,
    function(q) rlang::quo_get_expr(q)[[3]],
    character(1)
  ))
  expect_equal(labels, c("Structural", "IIV", "Residual"))
})

test_that("set_spec_filter works", {
  spec <- TableSpec() |> set_spec_filter(!fixed, diagonal)
  expect_length(spec@row_filter, 2)
})

test_that("set_spec_variability overwrites when specified", {
  spec <- TableSpec() |>
    set_spec_variability(
      fixed ~ "(Fixed)",
      TRUE ~ NA_character_,
      overwrite = TRUE
    )

  expect_length(spec@variability_rules, 2)
})

test_that("set_spec_variability appends by default", {
  spec <- TableSpec()
  modified <- spec |>
    set_spec_variability(
      fixed ~ "(Fixed)"
    )

  expect_length(
    get_spec_variability(modified),
    length(get_spec_variability(spec)) + 1
  )
})


# ==============================================================================
# SummarySpec-Only Setters
# ==============================================================================

test_that("set_spec_time_format works", {
  spec <- SummarySpec() |> set_spec_time_format("minutes")
  expect_equal(spec@time_format, "minutes")
})

test_that("set_spec_time_format validates input", {
  expect_error(
    SummarySpec() |> set_spec_time_format("invalid"),
    "@time_format must be"
  )
})

test_that("set_spec_models works", {
  spec <- SummarySpec() |> set_spec_models(c("run001", "run002"))
  expect_equal(spec@models_to_include, c("run001", "run002"))

  spec_null <- SummarySpec() |> set_spec_models(NULL)
  expect_null(spec_null@models_to_include)
})

test_that("set_spec_tag_filter works", {
  spec <- SummarySpec() |> set_spec_tag_filter(include = c("final", "approved"))
  expect_equal(spec@tag_filter, c("final", "approved"))
})

test_that("set_spec_tag_filter exclude works", {
  spec <- SummarySpec() |> set_spec_tag_filter(exclude = "failed")
  expect_null(spec@tag_filter)
  expect_equal(spec@tag_exclude, "failed")
})

test_that("set_spec_tag_filter sets both include and exclude", {
  spec <- SummarySpec() |>
    set_spec_tag_filter(include = c("final", "approved"), exclude = "failed")
  expect_equal(spec@tag_filter, c("final", "approved"))
  expect_equal(spec@tag_exclude, "failed")
})

test_that("set_spec_remove_unrun works", {
  spec <- SummarySpec() |> set_spec_remove_unrun(FALSE)
  expect_false(spec@remove_unrun_models)
})

test_that("set_spec_summary_filter works", {
  spec <- SummarySpec() |> set_spec_summary_filter(ofv < 1000)
  expect_length(spec@summary_filter, 1)
})

# ==============================================================================
# Immutability
# ==============================================================================

test_that("modifiers do not change original spec", {
  original <- TableSpec()
  original_title <- original@title

  modified <- original |>
    set_spec_title("Changed") |>
    add_spec_columns("cv") |>
    drop_spec_columns("unit")

  # Original should be unchanged

  expect_equal(original@title, original_title)
  expect_null(original@add_columns)
  expect_null(original@drop_columns)

  # Modified should have changes
  expect_equal(modified@title, "Changed")
  expect_true("cv" %in% modified@add_columns)
  expect_true("unit" %in% modified@drop_columns)
})

test_that("SummarySpec modifiers do not change original", {
  original <- SummarySpec()
  original_title <- original@title

  modified <- original |>
    set_spec_title("Changed") |>
    set_spec_time_format("hours")

  expect_equal(original@title, original_title)
  expect_equal(original@time_format, "seconds")
  expect_equal(modified@title, "Changed")
  expect_equal(modified@time_format, "hours")
})

# ==============================================================================
# Pipe Chain Integration
# ==============================================================================

test_that("TableSpec pipe chain works", {
  spec <- TableSpec() |>
    add_spec_columns("cv") |>
    drop_spec_columns("unit") |>
    set_spec_title("Test Table") |>
    set_spec_sigfig(4) |>
    set_spec_parameter_names(source = "display")

  expect_true("cv" %in% spec@add_columns)
  expect_true("unit" %in% spec@drop_columns)
  expect_equal(spec@title, "Test Table")
  expect_equal(spec@n_sigfig, 4)
  expect_equal(spec@parameter_names@source, "display")
})

test_that("SummarySpec pipe chain works", {
  spec <- SummarySpec() |>
    add_spec_columns("estimation_time") |>
    drop_spec_columns("description") |>
    set_spec_title("Run Summary") |>
    set_spec_time_format("auto") |>
    set_spec_models(c("run001", "run002"))

  expect_true("estimation_time" %in% get_spec_columns(spec))
  expect_true("description" %in% spec@drop_columns)
  expect_equal(spec@title, "Run Summary")
  expect_equal(spec@time_format, "auto")
  expect_equal(spec@models_to_include, c("run001", "run002"))
})

# ==============================================================================
# Type Validation
# ==============================================================================
