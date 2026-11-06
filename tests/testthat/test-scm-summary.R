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

# The fixture's sections, in order, with the catch-all last.
scm_fixture_sections <- function(other = "Other") {
  c(
    "Reference",
    "Forward round 1 \u2014 reference: 1001_base",
    "Forward round 2 \u2014 reference: 1001_wt_cl; retained: WT_CL",
    "Forward round 3 \u2014 reference: 1001_crcl_cl; retained: WT_CL, CRCL_CL",
    "Forward model \u2014 reference: 1001_crcl_cl; retained: WT_CL, CRCL_CL",
    "Backward round 1 \u2014 reference: 1001_crcl_cl; retained: WT_CL, CRCL_CL",
    "Backward round 2 \u2014 reference: 1001_crcl_cl; retained: WT_CL",
    "Final model \u2014 reference: 1001_crcl_cl; retained: WT_CL",
    other
  )
}

test_that("scm_summary_spec() sections the rounds in the order the process ran them", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  spec <- scm_summary_spec(scm_fixture_config())
  expect_true(S7::S7_inherits(spec, SummarySpec))
  # Each label carries the round's reference model and what the process had
  # retained going into it, from the record.
  expect_equal(section_labels(spec), scm_fixture_sections())
  expect_equal(get_spec_title(spec), "")
  expect_equal(get_spec_pvalue(spec)$direction, "both")
  expect_equal(get_spec_columns(spec), c("ofv", "dofv", "action"))
  expect_named(get_spec_mutate(spec), c("ofv", "dofv", "action"))
  expect_null(get_spec_footnotes(spec))
  expect_equal(get_spec_title(scm_summary_spec(scm_fixture_config(), title = "SCM")), "SCM")
  expect_equal(
    section_labels(scm_summary_spec(scm_fixture_config(), other_section_label = "Else"))[9],
    "Else"
  )

  # The SCM object works as the input too; the lineage alone does not.
  scm <- hyperion::read_scm(scm_fixture_config())
  expect_equal(section_labels(scm_summary_spec(scm)), section_labels(spec))
  expect_error(scm_summary_spec(hyperion::get_scm_lineage(scm)), "process record")

  expect_error(scm_summary_spec(scm_fixture_config(), title = 1), "`title` must be")
  expect_error(scm_summary_spec(scm_fixture_config(), other_section_label = NULL), "`other_section_label` must be")
  expect_error(scm_summary_spec(hyperion::get_model_lineage()), "no SCM fits")
})

test_that("an SCM lineage summarizes as one row per fit with its round's dOFV", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  scm <- hyperion::read_scm(scm_fixture_config())
  tree <- hyperion::get_scm_lineage(scm)
  data <- apply_summary_spec(tree, scm_summary_spec(scm))

  expect_equal(names(data), c("model", "ofv", "dofv", "action", "section"))
  # Every fit gets a row, although the same stem recurs across rounds.
  expect_equal(nrow(data), 12)
  expect_equal(sum(data$model == "1001_wt_v"), 2)
  sections <- scm_fixture_sections()[1:8]
  expect_equal(unique(data$section), sections)

  row <- function(model, section) data[data$model == model & data$section == section, ]
  # The stage models show their OFV and no dOFV.
  stage_sections <- sections[c(1, 5, 8)]
  stages <- data[data$section %in% stage_sections, ]
  expect_equal(stages$model, c("1001_base", "1001_scm_forward_final", "1001_scm_final"))
  expect_equal(stages$ofv, c(1000, 973.8, 979.5))
  expect_true(all(is.na(stages$dofv)))
  expect_true(all(is.na(stages$action)))
  # The candidates show their round's dOFV and no OFV; the kept one is marked.
  candidates <- data[!data$section %in% stage_sections, ]
  expect_true(all(is.na(candidates$ofv)))
  expect_false(anyNA(candidates$dofv))
  added <- row("1001_wt_cl", sections[2])
  expect_equal(added$dofv, -20)
  expect_equal(added$action, "Selected")
  expect_true(is.na(row("1001_wt_v", sections[2])$action))
  # A backward step: the candidate drops one, and the reference is the fuller model.
  dropped <- row("1001_crcl_cl", sections[6])
  expect_equal(dropped$dofv, 6)

  shown <- as.data.frame(data)[, c("section", "model", "ofv", "dofv", "action")]
  expect_snapshot(print(shown, digits = 4, row.names = FALSE))

  table <- make_summary_table(data)
  snapshot_gt(table, "scm-summary-gt")
})

test_that("the SCM preset tests both directions when the p-value column is selected", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  scm <- hyperion::read_scm(scm_fixture_config())
  tree <- hyperion::get_scm_lineage(scm)
  spec <- scm_summary_spec(scm) |>
    set_spec_columns("based_on", "dofv", "pvalue")
  data <- apply_summary_spec(tree, spec)
  sections <- scm_fixture_sections()
  row <- function(model, section) data[data$model == model & data$section == section, ]

  # A forward step: the candidate adds a parameter to its round's reference.
  added <- row("1001_wt_cl", sections[2])
  expect_equal(added$based_on, "1001_base")
  expect_equal(added$df, 1L)
  expect_equal(added$pvalue, stats::pchisq(20, 1, lower.tail = FALSE))
  # A backward step: the candidate drops one, and the reference is the fuller model.
  dropped <- row("1001_crcl_cl", sections[6])
  expect_equal(dropped$based_on, "1001_crcl_cl")
  expect_equal(dropped$df, 1L)
  expect_equal(dropped$pvalue, stats::pchisq(6, 1, lower.tail = FALSE))

  # The default direction keeps the backward step's dOFV but gives it no test.
  forward_only <- apply_summary_spec(tree, set_spec_pvalue(spec, direction = "forward"))
  dropped <- forward_only[
    forward_only$model == "1001_crcl_cl" & forward_only$section == sections[6],
  ]
  expect_equal(dropped$dofv, 6)
  expect_true(is.na(dropped$df))
  expect_true(is.na(dropped$pvalue))
})
