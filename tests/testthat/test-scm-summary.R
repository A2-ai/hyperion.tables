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

# A lineage as hyperion::get_model_lineage() gives it, from the metadata
# pharos writes beside each SCM fit.
scm_node <- function(path, based_on, description, tags) {
  list(
    name = path,
    model = list(
      based_on = if (is.null(based_on)) list() else list(based_on),
      copied_from = "",
      description = description,
      tags = as.list(tags)
    )
  )
}

pharos_tagged_lineage <- function() {
  nodes <- list(
    scm_node("model/nonmem/run001.mod", NULL, "structural model", "structural"),
    scm_node("scm/base/run001_base.mod", "model/nonmem/run001.mod", "SCM reference: fit base model (attempt 1)", c("scm", "base")),
    scm_node("scm/forward_round1/run001_wt_cl.mod", "scm/base/run001_base.mod", "SCM forward_round1: add WT_CL (attempt 1)", c("scm", "forward_round1", "selected")),
    scm_node("scm/forward_round1/run001_wt_v.mod", "scm/base/run001_base.mod", "SCM forward_round1: add WT_V (attempt 1)", c("scm", "forward_round1")),
    scm_node("scm/forward_round2/run001_wt_v_try2.mod", "scm/forward_round1/run001_wt_cl.mod", "SCM forward_round2: add WT_V (attempt 2)", c("scm", "forward_round2", "retry")),
    scm_node("scm/forward_final/run001_scm_forward_final.mod", "scm/forward_round1/run001_wt_cl.mod", "SCM forward model: retained WT_CL; cov step on", c("scm", "forward_final")),
    scm_node("scm/backward_round1/run001_wt_cl.mod", "scm/forward_round1/run001_wt_cl.mod", "SCM backward_round1: drop WT_CL (attempt 1)", c("scm", "backward_round1")),
    scm_node("scm/final/run001_scm_final.mod", "scm/forward_round1/run001_wt_cl.mod", "SCM final model: retained WT_CL; cov step on", c("scm", "final"))
  )
  structure(list(nodes = nodes), class = "hyperion_nonmem_tree")
}

test_that("scm_summary_spec() sections the rounds pharos tagged, in the order they ran", {
  spec <- scm_summary_spec(pharos_tagged_lineage())
  expect_true(S7::S7_inherits(spec, SummarySpec))
  expect_equal(
    section_labels(spec),
    c(
      "Base model", "Forward round 1", "Forward round 2", "Forward final model",
      "Backward round 1", "Final model", "Other"
    )
  )
  # The preset is the usual spec settings and nothing else.
  expect_equal(get_spec_tag_filter(spec)$include, "scm")
  expect_equal(get_spec_columns(spec), c("based_on", "description", "ofv", "dofv", "action"))
  expect_named(get_spec_mutate(spec), "action")
  expect_equal(get_spec_pvalue(spec)$direction, "both")
  expect_null(get_spec_footnotes(spec))
  expect_equal(get_spec_title(spec), "")
  expect_equal(get_spec_title(scm_summary_spec(pharos_tagged_lineage(), title = "SCM")), "SCM")
  expect_equal(
    section_labels(scm_summary_spec(pharos_tagged_lineage(), other_section_label = "Else"))[7],
    "Else"
  )

  # A backward-first process starts from the full model.
  full_first <- pharos_tagged_lineage()
  full_first$nodes[[2]]$model$tags <- list("scm", "full")
  expect_equal(section_labels(scm_summary_spec(full_first))[1], "Full model")

  expect_error(scm_summary_spec(pharos_tagged_lineage(), title = 1), "`title` must be")
  expect_error(
    scm_summary_spec(pharos_tagged_lineage(), other_section_label = NULL),
    "`other_section_label` must be"
  )
  no_scm <- pharos_tagged_lineage()
  no_scm$nodes <- no_scm$nodes[1]
  expect_error(scm_summary_spec(no_scm), "no SCM fits")
})

test_that("scm_summary_spec() reads hyperion's record lineage too", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  sections <- c(
    "Reference model", "Forward round 1", "Forward round 2", "Forward round 3",
    "Forward final model", "Backward round 1", "Backward round 2", "Final model",
    "Other"
  )
  expect_equal(section_labels(scm_summary_spec(scm_fixture_config())), sections)
  scm <- hyperion::read_scm(scm_fixture_config())
  expect_equal(section_labels(scm_summary_spec(scm)), sections)
  expect_equal(section_labels(scm_summary_spec(hyperion::get_scm_lineage(scm))), sections)
})

test_that("an SCM lineage summarizes as one row per fit with its round's dOFV", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  tree <- hyperion::get_scm_lineage(scm_fixture_config())
  data <- apply_summary_spec(tree, scm_summary_spec(tree))

  expect_equal(
    names(data),
    c("model", "based_on", "description", "ofv", "dofv", "action", "section")
  )
  # Every fit gets a row, although the same stem recurs across rounds.
  expect_equal(nrow(data), 12)
  expect_equal(sum(data$model == "1001_wt_v"), 2)
  expect_equal(
    unique(data$section),
    c(
      "Reference model", "Forward round 1", "Forward round 2", "Forward round 3",
      "Forward final model", "Backward round 1", "Backward round 2", "Final model"
    )
  )

  row <- function(model, section) data[data$model == model & data$section == section, ]
  # A forward step: the candidate adds a parameter to its round's reference.
  added <- row("1001_wt_cl", "Forward round 1")
  expect_equal(added$based_on, "1001_base")
  expect_equal(added$dofv, -20)
  expect_equal(added$action, "Selected")
  expect_true(is.na(row("1001_wt_v", "Forward round 1")$action))
  # A backward step: the candidate drops one, and the reference is the fuller model.
  dropped <- row("1001_crcl_cl", "Backward round 1")
  expect_equal(dropped$based_on, "1001_crcl_cl")
  expect_equal(dropped$dofv, 6)
  # The reference and final fits are not candidates, so they are never "Selected".
  expect_true(is.na(row("1001_base", "Reference model")$dofv))
  expect_true(all(is.na(data$action[data$section %in% c("Reference model", "Forward final model", "Final model")])))

  shown <- as.data.frame(data)[, c(
    "section", "model", "based_on", "description", "ofv", "dofv", "action"
  )]
  expect_snapshot(print(shown, digits = 4, row.names = FALSE))

  table <- make_summary_table(data)
  snapshot_gt(table, "scm-summary-gt")
})

test_that("the SCM preset tests both directions when the p-value column is selected", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  tree <- hyperion::get_scm_lineage(scm_fixture_config())
  spec <- scm_summary_spec(tree) |>
    set_spec_columns("based_on", "dofv", "pvalue")
  data <- apply_summary_spec(tree, spec)
  row <- function(model, section) data[data$model == model & data$section == section, ]

  # A forward step: the candidate adds a parameter to its round's reference.
  added <- row("1001_wt_cl", "Forward round 1")
  expect_equal(added$df, 1L)
  expect_equal(added$pvalue, stats::pchisq(20, 1, lower.tail = FALSE))
  # A backward step: the candidate drops one, and the reference is the fuller model.
  dropped <- row("1001_crcl_cl", "Backward round 1")
  expect_equal(dropped$df, 1L)
  expect_equal(dropped$pvalue, stats::pchisq(6, 1, lower.tail = FALSE))

  # The default direction keeps the backward step's dOFV but gives it no test.
  forward_only <- apply_summary_spec(tree, set_spec_pvalue(spec, direction = "forward"))
  dropped <- forward_only[
    forward_only$model == "1001_crcl_cl" & forward_only$section == "Backward round 1",
  ]
  expect_equal(dropped$dofv, 6)
  expect_true(is.na(dropped$df))
  expect_true(is.na(dropped$pvalue))
})
