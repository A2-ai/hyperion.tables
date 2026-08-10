# Regression tests for issues found in the 2026-07-06 architecture review.

model_dir <- function() {
  system.file("extdata", "models", "onecmt", package = "hyperion.tables")
}

enrich_model <- function(name, spec = TableSpec(display_transforms = list(omega = "cv"))) {
  mod <- hyperion::read_model(file.path(model_dir(), paste0(name, ".mod")))
  hyperion::get_parameters(mod) |>
    apply_table_spec(spec, hyperion::get_model_parameter_info(mod)) |>
    add_summary_info(summary(mod))
}

# ---- F1: format_sigfig_pad -------------------------------------------------

test_that("F1: format_sigfig_pad uses fixed notation, never scientific", {
  expect_equal(format_sigfig_pad(999.5, 3), "1000")
  expect_equal(format_sigfig_pad(3497.66, 3), "3500")
  expect_equal(format_sigfig_pad(4085.65, 3), "4090")
  expect_equal(format_sigfig_pad(1.9e-6, 3), "0.00000190")
  # never emits scientific notation across a wide magnitude range
  for (x in c(1234567, 999.5, 1234, 1.9e-6, 1e-9, -1234, -0.00004)) {
    expect_false(
      grepl("e", format_sigfig_pad(x, 3), ignore.case = TRUE),
      info = paste("x =", x)
    )
  }
})

test_that("F1: format_sigfig_pad handles non-finite values", {
  expect_equal(format_sigfig_pad(Inf, 4), "Inf")
  expect_equal(format_sigfig_pad(-Inf, 4), "-Inf")
  expect_equal(format_sigfig_pad(NA_real_, 3), NA_character_)
})

# ---- F2: variability rules evaluate conditions numerically -----------------

test_that("F2: variability rule conditions are numeric, not lexicographic", {
  spec <- TableSpec(n_sigfig = 3) |>
    set_spec_variability(
      cv > 30 ~ "(HIGH)",
      !is.na(cv) ~ "(normal)",
      TRUE ~ NA_character_,
      overwrite = TRUE
    )
  out <- build_variability_parameter(
    data.frame(kind = "OMEGA", cv = c(9.5, 40, 120, 200)),
    spec
  )
  expect_equal(out, c("(normal)", "(HIGH)", "(HIGH)", "(HIGH)"))
})

test_that("F2: default variability rule blanks cv == 0", {
  out <- build_variability_parameter(
    data.frame(kind = "OMEGA", cv = c(0, 25)),
    TableSpec(n_sigfig = 3)
  )
  expect_true(is.na(out[1]))
  expect_match(out[2], "25")
})

test_that("F2: rule labels still use formatted values", {
  spec <- TableSpec(n_sigfig = 3) |>
    set_spec_variability(
      !is.na(cv) ~ sprintf("(CV = %s%%)", cv),
      TRUE ~ NA_character_,
      overwrite = TRUE
    )
  out <- build_variability_parameter(
    data.frame(kind = "OMEGA", cv = 37.40183),
    spec
  )
  expect_equal(out, "(CV = 37.4%)")
})

# ---- F4: per-column display_transforms -------------------------------------

test_that("F4: per-column transforms do not transform unlisted columns", {
  local_fixture_dir()
  mod <- hyperion::read_model(file.path(model_dir(), "run001.mod"))
  info <- hyperion::get_model_parameter_info(mod)
  p <- hyperion::get_parameters(mod)

  only_estimate <- apply_table_spec(
    p,
    TableSpec(display_transforms = list(
      theta = "estimate", omega = "estimate", sigma = "estimate"
    )),
    info
  )
  om <- only_estimate$kind == "OMEGA"
  # cv was not requested for transformation -> stays NA (identity CV undefined)
  expect_true(all(is.na(only_estimate$cv[om])))

  # requesting cv transforms cv but leaves estimate raw
  only_cv <- apply_table_spec(p, TableSpec(display_transforms = list(omega = "cv")), info)
  om2 <- only_cv$kind == "OMEGA"
  expect_true(any(!is.na(only_cv$cv[om2])))
  expect_equal(only_cv$estimate[om2], p$estimate[p$kind == "OMEGA"])
})

test_that("F4: ci_low/ci_high are rejected as transform targets", {
  expect_error(
    TableSpec(display_transforms = list(omega = c("estimate", "ci_low"))),
    "display_transforms"
  )
})

# ---- F9: NA transform coalesces with a warning -----------------------------

test_that("F9: missing transforms coalesce to identity with a warning", {
  expect_warning(
    out <- coalesce_missing_transforms(c("LogNormal", NA, "Identity"), c("A", "B", "C")),
    "No transform found for parameter\\(s\\) B"
  )
  expect_equal(out, c("LogNormal", "identity", "Identity"))
})

# ---- apply_table_spec input validation -------------------------------------

test_that("apply_table_spec validates the params data frame", {
  expect_error(apply_table_spec(data.frame(x = 1:3), TableSpec(), NULL), "missing required column")
  expect_error(apply_table_spec("not a df", TableSpec(), NULL), "must be a data frame")
})

# ---- Dynamic section labels ------------------------------------------------

test_that("section rule labels may be variables, not just literals", {
  local_fixture_dir()
  lbl <- "Structural model parameters"
  spec <- TableSpec() |>
    set_spec_sections(kind == "THETA" ~ lbl, TRUE ~ "Other")
  mod <- hyperion::read_model(file.path(model_dir(), "run001.mod"))
  enriched <- apply_table_spec(
    hyperion::get_parameters(mod),
    spec,
    hyperion::get_model_parameter_info(mod)
  )
  expect_true(lbl %in% enriched$section)
})

# ---- n_decimals_ofv = NA ----------------------------------------------------

test_that("n_decimals_ofv = NA does not crash summary rendering", {
  local_fixture_dir()
  tree <- hyperion::get_model_lineage()
  spec <- SummarySpec(n_decimals_ofv = NA_real_)
  expect_no_error({
    st <- apply_summary_spec(tree, spec)
    tbl <- make_summary_table(st, output = "data")
    apply_formatting(tbl)
  })
})

# ---- F10: chaining preserves lineage ---------------------------------------

test_that("F10: compare_with chaining after add_model_lineage preserves lineage", {
  local_fixture_dir()
  e1 <- enrich_model("run001")
  e2 <- enrich_model("run002")
  e3 <- enrich_model("run003")

  base <- compare_with(e1, e2, labels = c("run001", "run002"))
  with_lineage <- add_model_lineage(base, hyperion::get_model_lineage())
  expect_true("lineage" %in% names(get_comparison_meta(with_lineage)))

  chained <- compare_with(with_lineage, e3, labels = "run003")
  expect_true("lineage" %in% names(get_comparison_meta(chained)))
})

# ---- Duplicate comparison labels -------------------------------------------

test_that("duplicate comparison labels abort with an actionable error", {
  local_fixture_dir()
  e1 <- enrich_model("run001")
  e2 <- enrich_model("run002")
  expect_error(
    compare_with(e1, e2, labels = c("run001", "run001")),
    "labels must be unique"
  )
})

# ---- signed zero -----------------------------------------------------------

test_that("strip_negative_zero removes redundant sign", {
  expect_equal(strip_negative_zero("-0.000"), "0.000")
  expect_equal(strip_negative_zero("-0"), "0")
  expect_equal(strip_negative_zero("-1.23"), "-1.23")
  expect_equal(strip_negative_zero("0.000"), "0.000")
})

# ---- p-value formatting ----------------------------------------------------

test_that("format_pvalue_string stays in fixed notation when not scientific", {
  expect_false(grepl("e", format_pvalue_string(1.23e-5, 3, scientific = FALSE), ignore.case = TRUE))
  expect_equal(
    format_pvalue_string(0.5, 3, scientific = FALSE, threshold = 1e-4),
    "0.5"
  )
  expect_false(grepl(
    "e",
    format_pvalue_string(1e-8, 3, scientific = FALSE, threshold = 1e-4),
    ignore.case = TRUE
  ))
})
