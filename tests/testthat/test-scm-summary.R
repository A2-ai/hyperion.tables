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
  plain <- c("based_on", "description", "ofv", "dofv", "pvalue")
  expect_equal(get_spec_columns(spec), plain)
  # `selected` picks one way to show the candidate each round kept. By
  # default every candidate shows and the kept one is not marked.
  expect_length(get_spec_computed_columns(spec), 0)
  expect_length(get_spec_bold_rows(spec), 0)
  expect_length(get_spec_model_filter(spec), 0)
  # "column": its result in a computed Round Result column.
  column <- scm_summary_spec(pharos_tagged_lineage(), selected = "column")
  expect_equal(get_spec_computed_columns(column)$round_result$label, "Round Result")
  expect_equal(get_spec_columns(column), c(plain, "round_result"))
  expect_length(get_spec_bold_rows(column), 0)
  expect_length(get_spec_model_filter(column), 0)
  # "bold": its row in bold, and no Round Result column.
  bold <- scm_summary_spec(pharos_tagged_lineage(), selected = "bold")
  expect_length(get_spec_bold_rows(bold), 1)
  expect_length(get_spec_computed_columns(bold), 0)
  expect_length(get_spec_model_filter(bold), 0)
  expect_equal(get_spec_columns(bold), plain)
  # "none" is the default spelled out.
  none <- scm_summary_spec(pharos_tagged_lineage(), selected = "none")
  expect_length(get_spec_bold_rows(none), 0)
  expect_length(get_spec_computed_columns(none), 0)
  expect_length(get_spec_model_filter(none), 0)
  expect_equal(get_spec_columns(none), plain)
  expect_equal(section_labels(none), section_labels(spec))
  # "only": the kept candidates alone, so nothing is marked, and the rounds
  # are grouped by direction.
  only <- scm_summary_spec(pharos_tagged_lineage(), selected = "only")
  expect_length(get_spec_model_filter(only), 1)
  expect_length(get_spec_bold_rows(only), 0)
  expect_length(get_spec_computed_columns(only), 0)
  expect_equal(get_spec_columns(only), plain)
  expect_equal(
    section_labels(only),
    c("Base model", "Forward", "Forward final model", "Backward", "Final model", "Other")
  )
  expect_error(scm_summary_spec(pharos_tagged_lineage(), selected = "x"), "should be one of")
  # The kept candidates are no longer a separate argument.
  expect_error(scm_summary_spec(pharos_tagged_lineage(), candidates = "selected"), "unused argument")

  # The Description column shows the description pharos writes as it is by
  # default, or its step, or the model's covariates; one without the
  # covariates part is left alone.
  written <- c(
    "SCM forward_round2: add CRCL_CL (attempt 1); covariates: WT_CL, CRCL_CL",
    "SCM reference: fit base model (attempt 1); covariates: none",
    "structural model"
  )
  expect_null(get_spec_description_transform(spec))
  step <- scm_summary_spec(pharos_tagged_lineage(), description = "step")
  expect_equal(
    get_spec_description_transform(step)(written),
    c("SCM forward_round2: add CRCL_CL (attempt 1)", "SCM reference: fit base model (attempt 1)", "structural model")
  )
  covariates <- scm_summary_spec(pharos_tagged_lineage(), description = "covariates")
  expect_equal(
    get_spec_description_transform(covariates)(written),
    c("WT_CL, CRCL_CL", "none", "structural model")
  )
  expect_error(scm_summary_spec(pharos_tagged_lineage(), description = "x"), "should be one of")
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

  # By default the candidate a round kept is not marked.
  expect_equal(
    names(data),
    c("model", "based_on", "description", "ofv", "dofv", "pvalue", "df", "section")
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
  expect_equal(added$description, "add WT_CL; covariates: WT_CL")
  expect_equal(added$dofv, -20)
  # A backward step: the candidate drops one, and the reference is the fuller model.
  dropped <- row("1001_crcl_cl", "Backward round 1")
  expect_equal(dropped$based_on, "1001_crcl_cl")
  expect_equal(dropped$dofv, 6)
  # The forward final and final models are based on the reference fit.
  forward_final <- row("1001_scm_forward_final", "Forward final model")
  expect_equal(forward_final$based_on, "1001_base")
  expect_equal(forward_final$dofv, 973.8 - 1000)
  final <- row("1001_scm_final", "Final model")
  expect_equal(final$based_on, "1001_base")
  expect_equal(final$dofv, 979.5 - 1000)

  # With description = "step", each row shows the fit's action alone; with
  # "covariates", what its model has.
  with_step <- apply_summary_spec(tree, scm_summary_spec(tree, description = "step"))
  expect_equal(with_step$description[with_step$model == "1001_wt_cl"][1], "add WT_CL")
  with_covariates <- apply_summary_spec(tree, scm_summary_spec(tree, description = "covariates"))
  expect_equal(
    with_covariates$description[with_covariates$section == "Forward round 2"],
    c("WT_CL, CRCL_CL", "WT_CL, WT_V")
  )

  stages <- c("Reference model", "Forward final model", "Final model")
  expect_true(is.na(row("1001_base", "Reference model")$dofv))

  # With selected = "column", the same rows gain a Round Result: the round's
  # result on the candidate it kept, and nothing on the others.
  marked <- apply_summary_spec(tree, scm_summary_spec(tree, selected = "column"))
  expect_equal(
    names(marked),
    c("model", "based_on", "description", "ofv", "dofv", "pvalue", "df", "round_result", "section")
  )
  expect_equal(marked$model, data$model)
  result <- function(model, section) {
    marked$round_result[marked$model == model & marked$section == section]
  }
  expect_equal(result("1001_wt_cl", "Forward round 1"), "added")
  expect_true(is.na(result("1001_wt_v", "Forward round 1")))
  expect_equal(result("1001_crcl_cl", "Backward round 1"), "dropped")
  # The reference and final fits are not candidates, so they have no result.
  expect_true(all(is.na(marked$round_result[marked$section %in% stages])))

  # With selected = "bold", the kept candidates are in bold and nothing else.
  bold <- apply_summary_spec(tree, scm_summary_spec(tree, selected = "bold"))
  expect_false("round_result" %in% names(bold))
  expect_equal(bold$.bold, !is.na(marked$round_result))

  # With selected = "only", only the stages and the kept candidates stay,
  # unmarked, with the rounds grouped by direction.
  kept <- apply_summary_spec(tree, scm_summary_spec(tree, selected = "only"))
  expect_equal(kept$model, data$model[data$section %in% stages | !is.na(marked$round_result)])
  expect_false(any(c("round_result", ".bold") %in% names(kept)))
  expect_lt(nrow(kept), nrow(data))
  expect_equal(
    kept$section,
    c("Reference model", "Forward", "Forward", "Forward final model", "Backward", "Final model")
  )
  expect_equal(kept$dofv[kept$model == "1001_wt_cl" & kept$section == "Forward"], -20)

  # Without a backward step that dropped anything, the forward final model
  # is left out: the final model is the result.
  forward_only <- tree
  forward_only$nodes <- Filter(
    function(node) !any(grepl("^backward_round", unlist(node$model$tags))),
    forward_only$nodes
  )
  only <- apply_summary_spec(forward_only, scm_summary_spec(forward_only, selected = "only"))
  expect_equal(only$section, c("Reference model", "Forward", "Forward", "Final model"))
  expect_false("1001_scm_forward_final" %in% only$model)
  expect_true("1001_scm_forward_final" %in% kept$model)
  # With every candidate shown, the rounds keep their own sections.
  expect_true("Forward round 1" %in% apply_summary_spec(forward_only, scm_summary_spec(forward_only))$section)

  shown <- as.data.frame(marked)[, c(
    "section", "model", "based_on", "description", "ofv", "dofv", "pvalue", "df", "round_result"
  )]
  expect_snapshot(print(shown, digits = 4, row.names = FALSE))

  expect_equal(make_summary_table(marked, output = "data")@col_labels$round_result, "Round Result")
  snapshot_gt(make_summary_table(data), "scm-summary-gt")
  snapshot_gt(make_summary_table(marked), "scm-summary-column-gt")
  snapshot_gt(make_summary_table(kept), "scm-summary-only-gt")
})

test_that("the SCM preset's p-value column tests both directions", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  tree <- hyperion::get_scm_lineage(scm_fixture_config())
  spec <- scm_summary_spec(tree)
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
  # The final model is tested against the reference fit it is based on.
  final <- row("1001_scm_final", "Final model")
  expect_equal(final$df, 1L)
  expect_equal(final$pvalue, stats::pchisq(1000 - 979.5, 1, lower.tail = FALSE))

  # The default direction keeps the backward step's dOFV but gives it no test.
  forward_only <- apply_summary_spec(tree, set_spec_pvalue(spec, direction = "forward"))
  dropped <- forward_only[
    forward_only$model == "1001_crcl_cl" & forward_only$section == "Backward round 1",
  ]
  expect_equal(dropped$dofv, 6)
  expect_true(is.na(dropped$df))
  expect_true(is.na(dropped$pvalue))
})
