# ==============================================================================
# Tests for SummarySpec sections
# ==============================================================================

test_that("tag-based sections render correctly (gt + flextable)", {
  testthat::skip_if_not_installed("gt")

  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  tree <- hyperion::get_model_lineage(model_dir)

  spec <- SummarySpec(
    sections = section_rules(
      "base" %in% tags ~ "Base Models",
      "key" %in% tags ~ "Key Models"
    )
  ) |>
    set_spec_section_filter(NA)

  table <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table()

  snapshot_gt(table, "sum-sections-gt")

  table_ft <- tree |>
    apply_summary_spec(spec) |>
    make_summary_table(output = "flextable")

  snapshot_flextable(table_ft, "sum-sections-ft")
})

test_that("section_filter multi-section", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )

  tree <- hyperion::get_model_lineage(model_dir)

  tree$nodes$run001.mod$tags <- c(tree$nodes$run001.mod$tags, "one-comp")
  tree$nodes$run002.mod$tags <- c(tree$nodes$run002.mod$tags, "two-comp")
  tree$nodes$run003.mod$tags <- c(tree$nodes$run003.mod$tags, "two-comp")

  # No catch-all: untagged models get NA section
  # Filter both "Key Models" and NA — only "Base Models" (run001) survives
  spec <- SummarySpec() |>
    set_spec_sections(
      "one-comp" %in% tags ~ "One Compartment",
      "base" %in% tags ~ "Base Model",
      "two-comp" %in% tags ~ "Two Compartment",
      TRUE ~ "Other"
    ) |>
    set_spec_section_filter("Other")

  # Multiple sections for single model warning
  expect_warning(
    table <- tree |>
      apply_summary_spec(spec) |>
      make_summary_table()
  )

  snapshot_gt(table, "multi-section-gt")

  # Multiple sections for single model warning
  expect_warning(
    table_ft <- tree |>
      apply_summary_spec(spec) |>
      make_summary_table(output = "flextable")
  )

  snapshot_flextable(table_ft, "multi-sections-ft")
})

test_that("section_filter drops sections from parameter table", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  params <- hyperion::get_parameters(mod)
  info <- hyperion::get_model_parameter_info(mod)

  spec <- TableSpec(
    sections = section_rules(
      kind == "THETA" ~ "Structural",
      kind == "OMEGA" ~ "IIV",
      kind == "SIGMA" ~ "Residual"
    )
  ) |>
    set_spec_section_filter("Residual")

  df <- apply_table_spec(params, spec, info)

  expect_false("Residual" %in% df$section)
  expect_true(all(df$section %in% c("Structural", "IIV")))
})

test_that("multi-match warning fires for parameter table sections", {
  model_dir <- system.file(
    "extdata",
    "models",
    "onecmt",
    package = "hyperion.tables"
  )
  mod <- hyperion::read_model(file.path(model_dir, "run001.mod"))
  params <- hyperion::get_parameters(mod)
  info <- hyperion::get_model_parameter_info(mod)

  # OMEGA rows are both "OMEGA" kind and diagonal, so they match both rules
  spec <- TableSpec(
    sections = section_rules(
      kind == "OMEGA" ~ "Random Effects",
      diagonal ~ "Diagonal Elements",
      kind == "THETA" ~ "Structural"
    )
  )

  expect_warning(
    apply_table_spec(params, spec, info),
    "multiple section rules"
  )
})

test_that("SummarySpec accumulates section rules across calls", {
  spec <- SummarySpec() |>
    set_spec_sections(
      "base" %in% tags ~ "Base Models",
      "final" %in% tags ~ "Final Models"
    ) |>
    set_spec_sections(
      model == "run001" ~ "Base Models"
    )

  rules <- get_spec_sections(spec)
  labels <- vapply(
    rules,
    function(r) {
      rlang::f_rhs(rlang::eval_tidy(r))
    },
    character(1)
  )
  expect_equal(sum(labels == "Base Models"), 2)
  expect_length(rules, 3)
})

test_that("SummarySpec rejects invalid section rules", {
  expect_error(
    SummarySpec(sections = list("not a formula")),
    "section rules"
  )
})
