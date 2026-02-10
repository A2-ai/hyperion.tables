test_that("parameter comparison table: run002 vs run003b1", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c("variability", "shrinkage")
  )

  mod1 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  mod_sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run003b1.mod"))
  mod_sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(mod_sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(mod_sum2),
      labels = c("run002", "run003b1")
    )

  snapshot_gt(make_comparison_table(comp), "cmp-grandparent-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-grandparent-ft"
  )
})

test_that("parameter comparison table: run003 vs run003b1", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c("variability", "shrinkage")
  )

  mod <- hyperion::read_model(file.path(model_dir, "run003.mod"))
  mod_sum <- summary(mod)
  info <- hyperion::get_model_parameter_info(mod)

  child_mod <- hyperion::read_model(file.path(model_dir, "run003b1.mod"))
  child_sum <- summary(child_mod)
  child_info <- hyperion::get_model_parameter_info(child_mod)

  comp <- hyperion::get_parameters(mod) |>
    apply_table_spec(spec, info) |>
    add_summary_info(mod_sum) |>
    compare_with(
      hyperion::get_parameters(child_mod) |>
        apply_table_spec(spec, child_info) |>
        add_summary_info(child_sum),
      labels = c("run003", "run003b1")
    )

  snapshot_gt(make_comparison_table(comp), "cmp-child-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-child-ft"
  )
})

test_that("parameter comparison table: run002 vs run003b1 drop symbol", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c("symbol", "variability", "shrinkage")
  )

  mod1 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  mod_sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run003b1.mod"))
  mod_sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(mod_sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(mod_sum2),
      labels = c("run002", "run003b1")
    )

  snapshot_gt(make_comparison_table(comp), "cmp-no-symbol-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-no-symbol-ft"
  )
})

test_that("parameter comparison table: run002 vs run003 drop ci has correct footnotes", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c("symbol", "ci", "shrinkage")
  )

  mod1 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  mod_sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run003.mod"))
  mod_sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(mod_sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(mod_sum2),
      labels = c("run002", "run003")
    )

  snapshot_gt(make_comparison_table(comp), "cmp-ci-fn-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-ci-fn-ft"
  )
})

test_that("parameter comparison table: run002 vs run003b1 drop configurable", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c(
      "variability",
      "shrinkage",
      "pct_change",
      "symbol_left",
      "rse_right"
    )
  )

  mod1 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  mod_sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run003b1.mod"))
  mod_sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(mod_sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(mod_sum2),
      labels = c("run002", "run003b1")
    )

  snapshot_gt(make_comparison_table(comp), "cmp-drop-cols-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-drop-cols-ft"
  )
})

test_that("comparison LRT p-value respects direction of delta OFV", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  testthat::skip_if_not(nzchar(model_dir), "Test data directory not found")

  lineage <- hyperion::get_model_lineage(model_dir)

  comparison <- data.frame(
    estimate_1 = c(1, 2, 3),
    estimate_2 = c(1, 2, 3),
    fixed_1 = c(FALSE, FALSE, NA),
    fixed_2 = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  class(comparison) <- c("hyperion_comparison", class(comparison))
  attr(comparison, "labels") <- c("run001", "run002")
  attr(comparison, "summaries") <- list(
    list(run_name = "run001", ofv = 100, number_obs = 10, condition_number = 1),
    list(run_name = "run002", ofv = 110, number_obs = 10, condition_number = 1)
  )
  attr(comparison, "lineage") <- lineage
  attr(comparison, "table_spec") <- TableSpec(columns = c("estimate", "fixed"))

  lines <- hyperion.tables:::build_comparison_footnote(
    comparison,
    n_sigfig = 3,
    pvalue_scientific = FALSE
  )
  lrt_line <- lines[grep("LRT p-value", lines)]

  expect_true(length(lrt_line) == 1)

  pval <- as.numeric(
    sub(".*LRT p-value = ([0-9.eE+-]+).*", "\\1", lrt_line)
  )

  expect_true(pval > 0.9)
})

test_that("parameter comparison table: three models with reference_model", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c("variability", "shrinkage")
  )

  # Get model summaries and parameter info for all three models
  mod1 <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  mod3 <- hyperion::read_model(file.path(model_dir, "run003.mod"))
  sum3 <- summary(mod3)
  info3 <- hyperion::get_model_parameter_info(mod3)

  # Build comparison: run001 + run002 (vs run001) + run003 (vs run001)
  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(sum2),
      labels = c("run001", "run002")
    ) |>
    compare_with(
      hyperion::get_parameters(mod3) |>
        apply_table_spec(spec, info3) |>
        add_summary_info(sum3),
      labels = "run003",
      reference_model = "run001"
    )

  snapshot_gt(make_comparison_table(comp), "cmp-ref-mod-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-ref-mod-ft"
  )
})

test_that("parameter comparison table: three models with lineage shows LRT", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c("variability", "shrinkage")
  )

  # Get model summaries and parameter info for all three models
  mod1 <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  mod3 <- hyperion::read_model(file.path(model_dir, "run003.mod"))
  sum3 <- summary(mod3)
  info3 <- hyperion::get_model_parameter_info(mod3)

  # Get real lineage from model directory
  lineage <- hyperion::get_model_lineage(model_dir)

  # Build comparison: run001 -> run002 -> run003 (all in lineage)
  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(sum2),
      labels = c("run001", "run002")
    ) |>
    compare_with(
      hyperion::get_parameters(mod3) |>
        apply_table_spec(spec, info3) |>
        add_summary_info(sum3),
      labels = "run003"
    ) |>
    add_model_lineage(lineage)

  snapshot_gt(make_comparison_table(comp), "cmp-lineage-lrt-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-lineage-lrt-ft"
  )
})

test_that("parameter comparison table: broken lineage suppresses LRT", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c("variability", "shrinkage")
  )

  # Get model summaries and parameter info for all three models
  mod1 <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  mod3 <- hyperion::read_model(file.path(model_dir, "run003.mod"))
  sum3 <- summary(mod3)
  info3 <- hyperion::get_model_parameter_info(mod3)

  # Get real lineage and break run003's based_on relationship
  # This makes run003 not in lineage with run002
  lineage <- hyperion::get_model_lineage(model_dir)
  lineage$nodes$`run003.mod`$based_on <- NULL

  # Build comparison: run001 -> run002 -> run003
  # LRT should only show for run002 vs run001 (in lineage)
  # LRT should NOT show for run003 vs run002 (lineage broken)
  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(sum2),
      labels = c("run001", "run002")
    ) |>
    compare_with(
      hyperion::get_parameters(mod3) |>
        apply_table_spec(spec, info3) |>
        add_summary_info(sum3),
      labels = "run003"
    ) |>
    add_model_lineage(lineage)

  snapshot_gt(make_comparison_table(comp), "cmp-lineage-brk-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-lineage-brk-ft"
  )
})

test_that("parameter comparison table: pvalue_threshold formats small p-values", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(
    display_transforms = list(omega = c("cv")),
    sections = section_rules(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "OMEGA" & !diagonal ~ "Interindividual covariance parameters",
      kind == "SIGMA" ~ "Residual variance",
      TRUE ~ "Other"
    ),
    n_sigfig = 3,
    drop_columns = c("variability", "shrinkage"),
    pvalue_threshold = 0.05
  )

  # Get model summaries and parameter info for all three models
  mod1 <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  mod3 <- hyperion::read_model(file.path(model_dir, "run003.mod"))
  sum3 <- summary(mod3)
  info3 <- hyperion::get_model_parameter_info(mod3)

  # Get lineage for LRT calculation
  lineage <- hyperion::get_model_lineage(model_dir)

  # Build comparison: run001 -> run002 -> run003 with pairwise comparisons
  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(sum2),
      labels = c("run001", "run002")
    ) |>
    compare_with(
      hyperion::get_parameters(mod3) |>
        apply_table_spec(spec, info3) |>
        add_summary_info(sum3),
      labels = "run003"
    ) |>
    add_model_lineage(lineage)

  snapshot_gt(make_comparison_table(comp), "cmp-pval-thresh-gt")
  snapshot_flextable(
    make_comparison_table(comp, output = "flextable"),
    "cmp-pval-thresh-ft"
  )
})

test_that("can_show_lrt returns reason for each suppression condition", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  lineage <- hyperion::get_model_lineage(model_dir)

  make_comp <- function(
    left_sum,
    right_sum,
    fixed1 = c(FALSE, FALSE, NA),
    fixed2 = c(FALSE, FALSE, FALSE),
    use_lineage = TRUE
  ) {
    comp <- data.frame(
      estimate_1 = c(1, 2, 3),
      estimate_2 = c(1, 2, 3),
      fixed_1 = fixed1,
      fixed_2 = fixed2,
      stringsAsFactors = FALSE
    )
    class(comp) <- c("hyperion_comparison", class(comp))
    attr(comp, "labels") <- c("run001", "run002")
    attr(comp, "summaries") <- list(left_sum, right_sum)
    if (use_lineage) attr(comp, "lineage") <- lineage
    comp
  }

  good_left <- list(run_name = "run001", ofv = 100, number_obs = 10)
  good_right <- list(run_name = "run002", ofv = 110, number_obs = 10)

  # no lineage
  comp <- make_comp(good_left, good_right, use_lineage = FALSE)
  res <- hyperion.tables:::can_show_lrt(comp, 1, 2, good_left, good_right)
  expect_false(res$show)
  expect_match(res$reason, "no lineage")

  # missing summary
  comp <- make_comp(good_left, good_right)
  res <- hyperion.tables:::can_show_lrt(comp, 1, 2, NULL, good_right)
  expect_false(res$show)
  expect_match(res$reason, "summaries are missing")

  # missing OFV
  no_ofv <- list(run_name = "run001", number_obs = 10)
  comp <- make_comp(no_ofv, good_right)
  res <- hyperion.tables:::can_show_lrt(comp, 1, 2, no_ofv, good_right)
  expect_false(res$show)
  expect_match(res$reason, "OFV")

  # different nobs
  diff_nobs <- list(run_name = "run002", ofv = 110, number_obs = 20)
  comp <- make_comp(good_left, diff_nobs)
  res <- hyperion.tables:::can_show_lrt(comp, 1, 2, good_left, diff_nobs)
  expect_false(res$show)
  expect_match(res$reason, "observation counts differ")

  # df == 0 (same fixed columns)
  comp <- make_comp(
    good_left,
    good_right,
    fixed1 = c(FALSE, FALSE, FALSE),
    fixed2 = c(FALSE, FALSE, FALSE)
  )
  res <- hyperion.tables:::can_show_lrt(comp, 1, 2, good_left, good_right)
  expect_false(res$show)
  expect_match(res$reason, "degrees of freedom")

  # missing run_name
  no_name <- list(ofv = 100, number_obs = 10)
  comp <- make_comp(no_name, good_right)
  res <- hyperion.tables:::can_show_lrt(comp, 1, 2, no_name, good_right)
  expect_false(res$show)
  expect_match(res$reason, "run names")

  # not in lineage (break run002's based_on so run001 vs run002 aren't related)
  broken_lineage <- lineage
  broken_lineage$nodes$`run002.mod`$based_on <- NULL
  not_in_lineage_comp <- make_comp(good_left, good_right)
  attr(not_in_lineage_comp, "lineage") <- broken_lineage
  res <- hyperion.tables:::can_show_lrt(
    not_in_lineage_comp,
    1,
    2,
    good_left,
    good_right
  )
  expect_false(res$show)
  expect_match(res$reason, "not in direct lineage")

  # success case
  comp <- make_comp(good_left, good_right)
  res <- hyperion.tables:::can_show_lrt(comp, 1, 2, good_left, good_right)
  expect_true(res$show)
  expect_null(res$reason)
})

test_that("format_condition_number_footnote returns correct string or NULL", {
  left <- list(condition_number = 12.3)
  right <- list(condition_number = 45.6)

  result <- hyperion.tables:::format_condition_number_footnote(
    left, right, "A", "B", 3
  )
  expect_match(result, "Condition Number")
  expect_match(result, "\\(A\\)")
  expect_match(result, "\\(B\\)")

  # Both NULL summaries
  expect_null(hyperion.tables:::format_condition_number_footnote(
    NULL, NULL, "A", "B", 3
  ))

  # One NULL
  result2 <- hyperion.tables:::format_condition_number_footnote(
    left, NULL, "A", "B", 3
  )
  expect_match(result2, "N/A")
  expect_match(result2, "\\(B\\)")
})

test_that("format_nobs_footnote returns correct string or NULL", {
  left <- list(number_obs = 100)
  right <- list(number_obs = 200)

  result <- hyperion.tables:::format_nobs_footnote(left, right, "A", "B")
  expect_match(result, "No. of Observations")
  expect_match(result, "100")
  expect_match(result, "200")

  # Both NULL
  expect_null(hyperion.tables:::format_nobs_footnote(NULL, NULL, "A", "B"))

  # One missing
  result2 <- hyperion.tables:::format_nobs_footnote(left, NULL, "A", "B")
  expect_match(result2, "N/A")
})

test_that("format_ofv_lrt_footnote returns OFV line or NULL", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  lineage <- hyperion::get_model_lineage(model_dir)

  comp <- data.frame(
    estimate_1 = c(1, 2, 3),
    estimate_2 = c(1, 2, 3),
    fixed_1 = c(FALSE, FALSE, NA),
    fixed_2 = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  class(comp) <- c("hyperion_comparison", class(comp))
  attr(comp, "lineage") <- lineage

  left_sum <- list(run_name = "run001", ofv = 100, number_obs = 10)
  right_sum <- list(run_name = "run002", ofv = 90, number_obs = 10)

  result <- hyperion.tables:::format_ofv_lrt_footnote(
    comp, 1, 2, left_sum, right_sum, "A", "B",
    10, 10, 3, 3, FALSE, NULL
  )
  expect_match(result, "OFV")
  expect_match(result, "LRT p-value")

  # Both NULL summaries -> NULL
  expect_null(hyperion.tables:::format_ofv_lrt_footnote(
    comp, 1, 2, NULL, NULL, "A", "B",
    NA, NA, 3, 3, FALSE, NULL
  ))
})

test_that("footnotes show 'N/A (no summary)' when model_summary is NULL", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(n_sigfig = 3)

  mod1 <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  info2 <- hyperion::get_model_parameter_info(mod2)

  # Build comparison where params2 has no model_summary
  expect_warning(
    comp <- hyperion::get_parameters(mod1) |>
      apply_table_spec(spec, info1) |>
      add_summary_info(sum1) |>
      compare_with(
        hyperion::get_parameters(mod2) |>
          apply_table_spec(spec, info2),
        labels = c("run001", "run002")
      ),
    "missing model_summary"
  )

  footnote <- hyperion.tables:::build_comparison_footnote(comp, n_sigfig = 3)
  no_summary_lines <- grep("no summary", footnote, value = TRUE)
  expect_true(length(no_summary_lines) > 0)
})

test_that("na_label returns correct string based on summary presence", {
  expect_equal(hyperion.tables:::na_label(NULL), "N/A (no summary)")
  expect_equal(
    hyperion.tables:::na_label(list(ofv = 100)),
    "N/A"
  )
})

test_that("compare_with warns on disjoint parameter names", {
  spec <- TableSpec(n_sigfig = 3)

  # Create two data frames with no overlapping parameter names
  p1 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  attr(p1, "table_spec") <- spec
  attr(p1, "model_summary") <- list(run_name = "A")

  p2 <- data.frame(
    name = c("OMEGA1", "OMEGA2"),
    estimate = c(3.0, 4.0),
    stringsAsFactors = FALSE
  )
  attr(p2, "model_summary") <- list(run_name = "B")

  expect_warning(
    compare_with(p1, p2, labels = c("A", "B")),
    "No shared parameters"
  )
})

test_that("compare_with aborts on unresolvable reference_model", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  spec <- TableSpec(n_sigfig = 3)

  mod1 <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  sum1 <- summary(mod1)
  info1 <- hyperion::get_model_parameter_info(mod1)

  mod2 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  sum2 <- summary(mod2)
  info2 <- hyperion::get_model_parameter_info(mod2)

  mod3 <- hyperion::read_model(file.path(model_dir, "run003.mod"))
  sum3 <- summary(mod3)
  info3 <- hyperion::get_model_parameter_info(mod3)

  comp <- hyperion::get_parameters(mod1) |>
    apply_table_spec(spec, info1) |>
    add_summary_info(sum1) |>
    compare_with(
      hyperion::get_parameters(mod2) |>
        apply_table_spec(spec, info2) |>
        add_summary_info(sum2),
      labels = c("run001", "run002")
    )

  expect_error(
    compare_with(
      comp,
      hyperion::get_parameters(mod3) |>
        apply_table_spec(spec, info3) |>
        add_summary_info(sum3),
      labels = "run003",
      reference_model = "nonexistent"
    ),
    "reference_model 'nonexistent' not found"
  )
})

test_that("validate_comparison_renderable aborts on zero rows", {
  comp <- data.frame(name = character(0), stringsAsFactors = FALSE)
  expect_error(
    hyperion.tables:::validate_comparison_renderable(
      comp,
      list(model1 = "estimate_1")
    ),
    "no rows"
  )
})

test_that("resolve_comparison_labels handles all edge cases", {
  resolve <- hyperion.tables:::resolve_comparison_labels

  # Initial comparison: must be length 2
  expect_equal(resolve(FALSE, c("A", "B"), NULL), c("A", "B"))
  expect_error(resolve(FALSE, c("A"), NULL), "length 2")
  expect_error(resolve(FALSE, c("A", "B", "C"), NULL), "length 2")

  # Chained comparison: single label appends
  expect_equal(resolve(TRUE, c("C"), c("A", "B")), c("A", "B", "C"))

  # Chained comparison: two labels replaces last existing + appends
  expect_equal(resolve(TRUE, c("B2", "C"), c("A", "B")), c("A", "B2", "C"))

  # Chained comparison: 3 labels errors
  expect_error(
    resolve(TRUE, c("A", "B", "C"), c("X")),
    "length 1 or 2"
  )
})

test_that("resolve_reference_index finds by run_name and label", {
  resolve <- hyperion.tables:::resolve_reference_index

  summaries <- list(
    list(run_name = "run001"),
    list(run_name = "run002")
  )
  labels <- c("A", "B")
  indices <- c(1L, 2L)

  # NULL reference_model returns default

  expect_equal(resolve(NULL, summaries, labels, indices, 1L), 1L)

  # Match by run_name
  expect_equal(resolve("run002", summaries, labels, indices, 1L), 2L)

  # Match by label fallback
  expect_equal(resolve("A", summaries, labels, indices, 1L), 1L)

  # Strip .mod suffix
  expect_equal(resolve("run001.mod", summaries, labels, indices, 1L), 1L)

  # Not found aborts
  expect_error(resolve("nope", summaries, labels, indices, 1L), "not found")
})

test_that("compare_with resolves reference_model against renamed labels", {
  spec <- TableSpec(n_sigfig = 3)

  p1 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  attr(p1, "table_spec") <- spec
  attr(p1, "model_summary") <- list(run_name = "run001")

  p2 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.5, 2.5),
    stringsAsFactors = FALSE
  )
  attr(p2, "model_summary") <- list(run_name = "run002")

  p3 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.8, 2.8),
    stringsAsFactors = FALSE
  )
  attr(p3, "model_summary") <- list(run_name = "run003")

  comp2 <- compare_with(p1, p2, labels = c("run001", "run002"))

  # Rename "run002" -> "BaseModel" via labels[1], add "run003" via labels[2],
  # and set reference_model to the renamed label
  comp3 <- compare_with(
    comp2,
    p3,
    labels = c("BaseModel", "run003"),
    reference_model = "BaseModel"
  )

  # Should resolve: "BaseModel" matches renamed label for model index 2
  # pct_change_3 should be relative to model 2 (the renamed one)
  pct_refs <- attr(comp3, "pct_change_refs")
  expect_equal(pct_refs[["pct_change_3"]], 2L)

  # Labels should reflect the rename
  expect_equal(attr(comp3, "labels"), c("run001", "BaseModel", "run003"))
})

test_that("compute_pct_change adds correct columns", {
  comp <- data.frame(
    estimate_1 = c(10, 20, 0),
    estimate_2 = c(15, 20, 5),
    stringsAsFactors = FALSE
  )

  result <- hyperion.tables:::compute_pct_change(comp, 1, 2)
  expect_true("pct_change_2" %in% names(result))
  expect_true("pct_change" %in% names(result))
  expect_equal(result$pct_change_2[1], 50)
  expect_equal(result$pct_change_2[2], 0)
  expect_true(is.na(result$pct_change_2[3]))  # ref == 0
})

test_that("validate_comparison_renderable aborts on empty model_cols", {
  comp <- data.frame(name = "THETA1", stringsAsFactors = FALSE)
  expect_error(
    hyperion.tables:::validate_comparison_renderable(comp, list()),
    "No model-specific columns"
  )
})

test_that("resolve_suffix_cols_for_comparison includes variability cols with transforms", {
  spec_with_transforms <- TableSpec(
    display_transforms = list(omega = c("cv")),
    n_sigfig = 3
  )
  p1 <- data.frame(
    name = c("THETA1", "OMEGA1"),
    estimate = c(1.0, 0.04),
    cv = c(NA, 20),
    fixed = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  attr(p1, "table_spec") <- spec_with_transforms

  cols <- hyperion.tables:::resolve_suffix_cols_for_comparison(p1)
  expect_true("cv" %in% cols)
  expect_true("fixed" %in% cols)
})

test_that("resolve_suffix_cols_for_comparison includes pct_change when no explicit columns", {
  spec_default <- TableSpec(n_sigfig = 3)
  p1 <- data.frame(
    name = "THETA1",
    estimate = 1.0,
    stringsAsFactors = FALSE
  )
  attr(p1, "table_spec") <- spec_default

  cols <- hyperion.tables:::resolve_suffix_cols_for_comparison(p1)
  expect_true("pct_change" %in% cols)
})

test_that("compute_model_positions returns correct indices for initial case", {
  spec <- TableSpec(n_sigfig = 3)
  p1 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  attr(p1, "table_spec") <- spec
  attr(p1, "model_summary") <- list(run_name = "run001", ofv = 100)

  p2 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.5, 2.5),
    stringsAsFactors = FALSE
  )
  attr(p2, "model_summary") <- list(run_name = "run002", ofv = 110)

  suffix_cols <- hyperion.tables:::resolve_suffix_cols_for_comparison(p1)
  pos <- hyperion.tables:::compute_model_positions(p1, p2, suffix_cols)

  expect_false(pos$is_comparison)
  expect_null(pos$existing_labels)
  expect_null(pos$existing_summaries)
  expect_equal(pos$next_index, 2)
  expect_equal(pos$prev_idx, 1)
  expect_equal(pos$last_idx, 2)
  expect_equal(pos$sum1$run_name, "run001")
  expect_equal(pos$sum2$run_name, "run002")
})

test_that("compute_model_positions returns correct indices for chained case", {
  spec <- TableSpec(n_sigfig = 3)

  # Build a real comparison to chain from
  p1 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  attr(p1, "table_spec") <- spec
  attr(p1, "model_summary") <- list(run_name = "run001", ofv = 100)

  p2 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.5, 2.5),
    stringsAsFactors = FALSE
  )
  attr(p2, "model_summary") <- list(run_name = "run002", ofv = 110)

  comp <- compare_with(p1, p2, labels = c("run001", "run002"))

  p3 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.8, 2.8),
    stringsAsFactors = FALSE
  )
  attr(p3, "model_summary") <- list(run_name = "run003", ofv = 120)

  suffix_cols <- hyperion.tables:::resolve_suffix_cols_for_comparison(comp)
  pos <- hyperion.tables:::compute_model_positions(comp, p3, suffix_cols)

  expect_true(pos$is_comparison)
  expect_equal(pos$existing_labels, c("run001", "run002"))
  expect_equal(pos$next_index, 3)
  expect_equal(pos$prev_idx, 2)
  expect_equal(pos$last_idx, 3)
  expect_equal(pos$sum2$run_name, "run003")
})

test_that("join_comparison_params produces correct columns for initial join", {
  spec <- TableSpec(n_sigfig = 3)

  p1 <- data.frame(
    name = c("THETA1", "THETA2", "THETA3"),
    estimate = c(1.0, 2.0, 3.0),
    rse = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  attr(p1, "table_spec") <- spec
  attr(p1, "model_summary") <- list(run_name = "run001")

  p2 <- data.frame(
    name = c("THETA1", "THETA2", "THETA4"),
    estimate = c(1.5, 2.5, 4.0),
    rse = c(15, 25, 35),
    stringsAsFactors = FALSE
  )
  attr(p2, "model_summary") <- list(run_name = "run002")

  suffix_cols <- hyperion.tables:::resolve_suffix_cols_for_comparison(p1)
  positions <- hyperion.tables:::compute_model_positions(p1, p2, suffix_cols)
  result <- hyperion.tables:::join_comparison_params(
    p1, p2, suffix_cols, positions
  )

  # Should have suffixed columns for both models
  expect_true("estimate_1" %in% names(result))
  expect_true("estimate_2" %in% names(result))
  expect_true("rse_1" %in% names(result))
  expect_true("rse_2" %in% names(result))
  # Full join: THETA1, THETA2, THETA3 (only in p1), THETA4 (only in p2)
  expect_equal(nrow(result), 4)
  expect_equal(sort(result$name), c("THETA1", "THETA2", "THETA3", "THETA4"))
  # No hyperion_comparison class - just a plain data frame
  expect_false(inherits(result, "hyperion_comparison"))
})

test_that("join_comparison_params produces correct columns for chained join", {
  spec <- TableSpec(n_sigfig = 3)

  p1 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  attr(p1, "table_spec") <- spec
  attr(p1, "model_summary") <- list(run_name = "run001")

  p2 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.5, 2.5),
    stringsAsFactors = FALSE
  )
  attr(p2, "model_summary") <- list(run_name = "run002")

  comp <- compare_with(p1, p2, labels = c("run001", "run002"))

  p3 <- data.frame(
    name = c("THETA1", "THETA2"),
    estimate = c(1.8, 2.8),
    stringsAsFactors = FALSE
  )
  attr(p3, "model_summary") <- list(run_name = "run003")

  suffix_cols <- hyperion.tables:::resolve_suffix_cols_for_comparison(comp)
  positions <- hyperion.tables:::compute_model_positions(comp, p3, suffix_cols)
  result <- hyperion.tables:::join_comparison_params(
    comp, p3, suffix_cols, positions
  )

  # Should have estimate_1, estimate_2 (from existing), estimate_3 (new)
  expect_true("estimate_1" %in% names(result))
  expect_true("estimate_2" %in% names(result))
  expect_true("estimate_3" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("parameter comparison table: errors no spec", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  # Get model summaries and parameter info for all three models
  mod1 <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  mod2 <- hyperion::read_model(file.path(model_dir, "run002.mod"))
  mod3 <- hyperion::read_model(file.path(model_dir, "run003.mod"))

  sum1 <- summary(mod1)
  sum2 <- summary(mod2)
  sum3 <- summary(mod3)

  # Get lineage for LRT calculation
  lineage <- hyperion::get_model_lineage(model_dir)

  # Build comparison: run001 -> run002 -> run003 with pairwise comparisons
  expect_error(
    hyperion::get_parameters(mod1) |>
      add_summary_info(sum1) |>
      compare_with(
        hyperion::get_parameters(mod2) |>
          add_summary_info(sum2),
        labels = c("run001", "run002")
      ) |>
      compare_with(
        hyperion::get_parameters(mod3) |>
          add_summary_info(sum3),
        labels = "run003"
      ) |>
      add_model_lineage(lineage) |>
      make_comparison_table()
  )
})
