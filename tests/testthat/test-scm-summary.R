# The completed SCM process hyperion ships as a fixture; its project root is
# hyperion's installed directory, which holds the pharos.toml.
scm_fixture_config <- function() {
  system.file(
    "extdata", "models", "scm-complete", "scm", "1001scm.toml",
    package = "hyperion"
  )
}

section_labels <- function(spec) {
  unname(vapply(
    spec@sections@rules,
    function(rule) rlang::f_rhs(rlang::eval_tidy(rule)),
    character(1)
  ))
}

test_that("scm_summary_spec() sections the rounds in the order the process ran them", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")

  spec <- scm_summary_spec(scm_fixture_config())
  expect_true(S7::S7_inherits(spec, SummarySpec))
  expect_equal(
    section_labels(spec),
    c(
      "Reference", "Forward round 1", "Forward round 2", "Forward round 3",
      "Forward model", "Backward round 1", "Backward round 2", "Final model",
      "Other"
    )
  )
  expect_equal(get_spec_title(spec), "")
  expect_equal(get_spec_title(scm_summary_spec(scm_fixture_config(), title = "SCM")), "SCM")
  expect_equal(
    section_labels(scm_summary_spec(scm_fixture_config(), other_section_label = "Else"))[9],
    "Else"
  )

  # The lineage itself works as the input, so it need not be built twice.
  tree <- hyperion::get_scm_lineage(scm_fixture_config())
  expect_equal(section_labels(scm_summary_spec(tree)), section_labels(spec))

  expect_error(scm_summary_spec(scm_fixture_config(), title = 1), "`title` must be")
  expect_error(scm_summary_spec(scm_fixture_config(), other_section_label = NULL), "`other_section_label` must be")
  expect_error(scm_summary_spec(hyperion::get_model_lineage()), "no SCM fits")
})

test_that("an SCM lineage summarizes as one row per fit with its round's dOFV and p-value", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  tree <- hyperion::get_scm_lineage(scm_fixture_config())
  data <- apply_summary_spec(tree, scm_summary_spec(tree))

  # Every fit gets a row, although the same stem recurs across rounds.
  expect_equal(nrow(data), 12)
  expect_equal(sum(data$model == "1001_wt_v"), 2)
  expect_equal(
    unique(data$section),
    c(
      "Reference", "Forward round 1", "Forward round 2", "Forward round 3",
      "Forward model", "Backward round 1", "Backward round 2", "Final model"
    )
  )

  row <- function(model, section) data[data$model == model & data$section == section, ]
  # A forward step: the candidate adds a parameter to its round's reference.
  added <- row("1001_wt_cl", "Forward round 1")
  expect_equal(added$based_on, "1001_base")
  expect_equal(added$dofv, -20)
  expect_equal(added$df, 1L)
  expect_equal(added$pvalue, stats::pchisq(20, 1, lower.tail = FALSE))
  # A backward step: the candidate drops one, and the reference is the fuller model.
  dropped <- row("1001_crcl_cl", "Backward round 1")
  expect_equal(dropped$based_on, "1001_crcl_cl")
  expect_equal(dropped$dofv, 6)
  expect_equal(dropped$df, 1L)
  expect_equal(dropped$pvalue, stats::pchisq(6, 1, lower.tail = FALSE))
  expect_true(is.na(row("1001_base", "Reference")$dofv))

  shown <- as.data.frame(data)[, c(
    "section", "model", "based_on", "description", "n_parameters", "ofv", "dofv", "df", "pvalue"
  )]
  expect_snapshot(print(shown, digits = 4, row.names = FALSE))

  table <- make_summary_table(data)
  snapshot_gt(table, "scm-summary-gt")
})
