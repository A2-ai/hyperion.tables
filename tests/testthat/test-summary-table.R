test_that("summary table gt snapshot from vignettes data", {
  testthat::skip_if_not_installed("gt")

  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)
  spec <- SummarySpec()

  table <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table()

  snapshot_gt(table, "sum-base-gt")

  table_ft <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table(output = "flextable")

  snapshot_flextable(table_ft, "sum-base-ft")
})

test_that("tag_filter selects tagged models", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)
  for (name in names(tree$nodes)) {
    tree$nodes[[name]]$tags <- "draft"
  }
  tree$nodes[["run001.mod"]]$tags <- "final"

  spec <- SummarySpec(tag_filter = "final", columns = "ofv")

  data <- apply_summary_spec(tree, spec)

  expect_true(nrow(data) > 0)
  expect_true(all(data$model == "run001"))
})

test_that("tag_exclude removes tagged models", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)
  # Tag one model as "failed"
  tree$nodes[["run001.mod"]]$tags <- c(
    tree$nodes[["run001.mod"]]$tags, "failed"
  )

  spec <- SummarySpec(tag_exclude = "failed", columns = "ofv")

  data <- apply_summary_spec(tree, spec)

  expect_true(nrow(data) > 0)
  expect_false("run001" %in% data$model)
})

test_that("tag_exclude works with tag_filter", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)
  # Tag all models with "candidate", then mark one as "failed"
  for (name in names(tree$nodes)) {
    tree$nodes[[name]]$tags <- "candidate"
  }
  tree$nodes[["run001.mod"]]$tags <- c("candidate", "failed")

  spec <- SummarySpec(
    tag_filter = "candidate",
    tag_exclude = "failed",
    columns = "ofv"
  )

  data <- apply_summary_spec(tree, spec)

  expect_true(nrow(data) > 0)
  expect_false("run001" %in% data$model)
})

test_that("summary table snapshot filtered to selected models", {
  testthat::skip_if_not_installed("gt")
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(
    models_to_include = c("run001", "run002", "run003")
  )

  table <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table()

  snapshot_gt(table, "sum-filtered-gt")

  table_ft <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table(output = "flextable")

  snapshot_flextable(table_ft, "sum-filtered-ft")
})

test_that("summary_filter applies to summary columns", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(
    columns = "ofv",
    summary_filter = summary_filter_rules(ofv == min(ofv, na.rm = TRUE))
  )

  data <- apply_summary_spec(tree, spec)

  expect_true(nrow(data) > 0)
  expect_true(all(data$ofv == min(data$ofv, na.rm = TRUE)))
})

test_that("models_to_include matches stems and extensions", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(
    columns = "ofv",
    models_to_include = c("run001", "run002.mod")
  )

  data <- apply_summary_spec(tree, spec)

  expect_true(nrow(data) > 0)
  expect_true(all(sort(unique(data$model)) == c("run001", "run002")))
})

test_that("remove_unrun_models snapshot", {
  testthat::skip_if_not_installed("gt")
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(remove_unrun_models = FALSE)

  table <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table()

  snapshot_gt(table, "sum-include-unrun-gt")

  table_ft <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table(output = "flextable")

  snapshot_flextable(table_ft, "sum-include-unrun-ft")
})

test_that("drop_columns removes description (snapshot)", {
  testthat::skip_if_not_installed("gt")
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(
    columns = c("description", "ofv"),
    drop_columns = "description"
  )

  table <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table()

  snapshot_gt(table, "sum-drop-desc-gt")

  table_ft <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table(output = "flextable")

  snapshot_flextable(table_ft, "sum-drop-desc-ft")
})

test_that("time_format auto uses seconds label", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(
    add_columns = c("estimation_time", "covariance_time"),
    time_format = "auto"
  )

  data <- apply_summary_spec(tree, spec)

  expect_equal(attr(data, "summary_time_unit"), "s")

  table <- data |> make_summary_table()
  snapshot_gt(table, "sum-seconds-label-gt")

  table_ft <- data |> make_summary_table(output = "flextable")
  snapshot_flextable(table_ft, "sum-seconds-label-ft")
})

test_that("time_format auto uses minutes label", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(
    columns = c("estimation_time", "covariance_time"),
    time_format = "auto"
  )

  data <- apply_summary_spec(tree, spec)

  data$estimation_time <- rep(120, nrow(data))
  data$covariance_time <- rep(90, nrow(data))
  data <- format_time_columns(data, spec)

  expect_equal(attr(data, "summary_time_unit"), "min")
  expect_true(all(data$estimation_time == 2))
})

test_that("time_format auto uses hours label", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(
    columns = c("estimation_time", "covariance_time"),
    time_format = "auto"
  )

  data <- apply_summary_spec(tree, spec)

  data$estimation_time <- rep(7200, nrow(data))
  data$covariance_time <- rep(5400, nrow(data))
  data <- format_time_columns(data, spec)

  expect_equal(attr(data, "summary_time_unit"), "h")
  expect_true(all(data$estimation_time == 2))
})

test_that("footnote_order NULL disables footnotes", {
  testthat::skip_if_not_installed("gt")
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(footnote_order = NULL)

  table <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table()

  snapshot_gt(table, "sum-no-footnotes-gt")

  table_ft <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table(output = "flextable")

  snapshot_flextable(table_ft, "sum-no-footnotes-ft")
})

test_that("pvalue_threshold formats small p-values", {
  testthat::skip_if_not_installed("gt")
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(pvalue_threshold = 0.05)

  table <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table()

  snapshot_gt(table, "sum-pval-thresh-gt")

  table_ft <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table(output = "flextable")

  snapshot_flextable(table_ft, "sum-pval-thresh-ft")
})

test_that("summary rendering merges df unless explicitly dropped", {
  data <- data.frame(
    model = "run001",
    pvalue = 0.12,
    df = 1L,
    stringsAsFactors = FALSE
  )
  spec <- SummarySpec(columns = c("pvalue", "df"))

  htable <- hyperion.tables:::hyperion_summary_table(data, spec)
  overrides <- hyperion.tables:::apply_summary_render_overrides(
    htable,
    data,
    htable@numeric_cols
  )

  expect_true("pvalue" %in% names(overrides$data))
  expect_false("df" %in% names(overrides$data))
})

test_that("SummarySpec validates n_decimals_ofv", {
  expect_error(
    SummarySpec(n_decimals_ofv = -1),
    "@n_decimals_ofv"
  )
})

test_that("summary tables handle .ctl model names", {
  model_dir <- system.file(
    "extdata",
    "models",
    "ctls",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(columns = "ofv")
  data <- apply_summary_spec(tree, spec)

  expect_true(nrow(data) > 0)
})

test_that("apply_summary_spec keeps df when pvalue requested", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  tree <- hyperion::get_model_lineage(model_dir)
  spec <- SummarySpec(columns = c("ofv", "dofv", "pvalue"))

  data <- apply_summary_spec(tree, spec)

  expect_true("df" %in% names(data))
})

test_that("load_models preserves list entries for missing outputs", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  model_names <- c("run001.mod", "missing.mod")
  expect_warning(
    expect_warning(
      hyperion.tables:::load_models(model_names, model_dir),
      "could not load model:"
    ),
    "1 of 2 model\\(s\\) failed to load"
  )
  models <- suppressWarnings(
    hyperion.tables:::load_models(model_names, model_dir)
  )

  # Use [<- with NULL to preserve list entry for missing models.
  expect_true(all(model_names %in% names(models)))
  expect_false(is.null(models[["run001.mod"]]))
  expect_true(is.null(models[["missing.mod"]]))
})


test_that("format_time_columns(auto) does not warn on all-NA data", {
  spec <- SummarySpec(columns = "estimation_time", time_format = "auto")
  df <- data.frame(
    model = "run001",
    estimation_time = NA_real_,
    stringsAsFactors = FALSE
  )

  expect_warning(
    format_time_columns(df, spec),
    NA
  )
})

test_that("get_time_suffix(auto) returns minutes for mid-range values when attribute missing", {
  data <- data.frame(
    estimation_time = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  suffix <- hyperion.tables:::get_time_suffix("auto", data)

  expect_equal(suffix, "min")
})

test_that("dofv calculation includes n_parameters even when not in spec columns", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  tree <- hyperion::get_model_lineage(model_dir)
  # Requesting dofv forces n_parameters to be computed internally,
  # even though it's not in spec@columns
  spec <- SummarySpec(columns = c("ofv", "dofv"))

  data <- apply_summary_spec(tree, spec)

  # dofv should be present (computed from n_parameters internally)
  expect_true("dofv" %in% names(data))
})

test_that("format_time_value treats auto as seconds", {
  expect_equal(
    hyperion.tables:::format_time_value(c(3600), "auto"),
    3600
  )
})

test_that("filter_metadata errors with message when tag_filter matches nothing", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)
  spec <- SummarySpec(tag_filter = "nonexistent_tag_xyz")

  expect_error(
    apply_summary_spec(tree, spec),
    "No models remain after filtering.*tag_filter: nonexistent_tag_xyz"
  )
})

test_that("filter_metadata errors with message when models_to_include matches nothing", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)
  spec <- SummarySpec(models_to_include = c("no_such_model", "also_missing"))

  expect_error(
    apply_summary_spec(tree, spec),
    "No models remain after filtering.*models_to_include: no_such_model, also_missing"
  )
})

test_that("filter_metadata errors with both filters in message", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)
  spec <- SummarySpec(
    tag_filter = "fake_tag",
    models_to_include = "fake_model"
  )

  expect_error(
    apply_summary_spec(tree, spec),
    "No models remain after filtering"
  )
})

test_that("filter_metadata errors with tag_exclude in message", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)
  # Tag all models so exclusion removes everything
  for (name in names(tree$nodes)) {
    tree$nodes[[name]]$tags <- "doomed"
  }
  spec <- SummarySpec(tag_exclude = "doomed")

  expect_error(
    apply_summary_spec(tree, spec),
    "No models remain after filtering.*tag_exclude: doomed"
  )
})
