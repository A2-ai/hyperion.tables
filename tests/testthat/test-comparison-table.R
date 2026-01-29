test_that("parameter comparison table: run002 vs run003b1", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-grandparent-ft")
})

test_that("parameter comparison table: run003 vs run003b1", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-child-ft")
})

test_that("parameter comparison table: run002 vs run003b1 drop symbol", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-no-symbol-ft")
})

test_that("parameter comparison table: run002 vs run003 drop ci has correct footnotes", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-ci-fn-ft")
})

test_that("parameter comparison table: run002 vs run003b1 drop configurable", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-drop-cols-ft")
})

test_that("parameter comparison table: three models with reference_model", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-ref-mod-ft")
})

test_that("parameter comparison table: three models with lineage shows LRT", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-lineage-lrt-ft")
})

test_that("parameter comparison table: broken lineage suppresses LRT", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-lineage-brk-ft")
})

test_that("parameter comparison table: pvalue_threshold formats small p-values", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
  snapshot_flextable(make_comparison_table(comp, output = "flextable"), "cmp-pval-thresh-ft")
})

test_that("parameter comparison table: errors no spec", {
  model_dir <- system.file("extdata", "models", "onecmt", package = "hyperion.tables")

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
