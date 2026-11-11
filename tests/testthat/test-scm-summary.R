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
    scm_node("scm/working_full/run001_scm_working_full.mod", "scm/forward_round1/run001_wt_cl.mod", "SCM working full model: retained WT_CL; cov step on", c("scm", "working_full")),
    scm_node("scm/backward_round1/run001_wt_cl.mod", "scm/forward_round1/run001_wt_cl.mod", "SCM backward_round1: drop WT_CL (attempt 1)", c("scm", "backward_round1")),
    scm_node("scm/reduced_covariate/run001_scm_reduced_covariate.mod", "scm/forward_round1/run001_wt_cl.mod", "SCM reduced covariate model: retained WT_CL; cov step on", c("scm", "reduced_covariate"))
  )
  structure(list(nodes = nodes), class = "hyperion_nonmem_tree")
}

test_that("scm_summary_spec() sections the rounds pharos tagged, in the order they ran", {
  spec <- scm_summary_spec(pharos_tagged_lineage())
  expect_true(S7::S7_inherits(spec, SummarySpec))
  expect_equal(
    section_labels(spec),
    c(
      "Base model", "Forward round 1", "Forward round 2", "Working full model",
      "Backward round 1", "Reduced covariate model", "Other"
    )
  )
  # The preset is the usual spec settings and nothing else.
  expect_equal(get_spec_tag_filter(spec)$include, "scm")
  plain <- c("based_on", "description", "ofv", "dofv", "pvalue")
  expect_equal(get_spec_columns(spec), plain)
  # `selected` picks how to show the candidate each round kept. By
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
  # "path": the kept candidates alone, so nothing is marked, and the rounds
  # are grouped by direction.
  path <- scm_summary_spec(pharos_tagged_lineage(), selected = "path")
  expect_length(get_spec_model_filter(path), 1)
  expect_length(get_spec_bold_rows(path), 0)
  expect_length(get_spec_computed_columns(path), 0)
  expect_equal(get_spec_columns(path), plain)
  expect_equal(
    section_labels(path),
    c("Base model", "Forward", "Working full model", "Backward", "Reduced covariate model", "Other")
  )
  # "column" and "bold" together: both marks.
  both <- scm_summary_spec(pharos_tagged_lineage(), selected = c("column", "bold"))
  expect_equal(get_spec_columns(both), c(plain, "round_result"))
  expect_named(get_spec_computed_columns(both), "round_result")
  expect_length(get_spec_bold_rows(both), 1)
  expect_length(get_spec_model_filter(both), 0)
  expect_error(scm_summary_spec(pharos_tagged_lineage(), selected = "x"), "must be")
  expect_error(scm_summary_spec(pharos_tagged_lineage(), selected = character()), "must be")
  # The other two are given alone.
  expect_error(
    scm_summary_spec(pharos_tagged_lineage(), selected = c("path", "bold")),
    "given alone"
  )
  expect_error(
    scm_summary_spec(pharos_tagged_lineage(), selected = c("none", "column")),
    "given alone"
  )
  # The kept candidates are no longer a separate argument.
  expect_error(scm_summary_spec(pharos_tagged_lineage(), candidates = "selected"), "unused argument")

  # The Description column shows what each fit added or dropped by default,
  # or that and its model's covariates, without
  # the round and attempt pharos writes around the step. A stage fit has no
  # step, and a description without the covariates part shows its step alone.
  written <- c(
    "SCM forward_round2: add CRCL_CL (attempt 1); covariates: WT_CL, CRCL_CL",
    "SCM reference: fit base model (attempt 1); covariates: none",
    "SCM working full model: cov step on (attempt 2); covariates: WT_CL, CRCL_CL",
    "add WT_V; covariates: WT_CL, WT_V",
    "reduced covariate model; covariates: WT_CL",
    "SCM backward_round1: drop WT_CL (attempt 1)",
    "structural model"
  )
  written_tags <- list(
    c("scm", "forward_round2", "selected"),
    c("scm", "base"),
    c("scm", "working_full"),
    c("scm", "forward_round2"),
    c("scm", "reduced_covariate"),
    c("scm", "backward_round1"),
    "structural"
  )
  expect_equal(
    get_spec_description_transform(spec)(written, tags = written_tags),
    c("add CRCL_CL", "", "", "add WT_V", "", "drop WT_CL", "structural model")
  )
  verbose <- scm_summary_spec(pharos_tagged_lineage(), description = "verbose")
  expect_equal(
    get_spec_description_transform(verbose)(written, tags = written_tags),
    c(
      "add CRCL_CL; covariates: WT_CL, CRCL_CL",
      "covariates: none",
      "covariates: WT_CL, CRCL_CL",
      "add WT_V; covariates: WT_CL, WT_V",
      "covariates: WT_CL",
      "drop WT_CL",
      "structural model"
    )
  )
  # The covariates alone are no longer a choice.
  expect_error(
    scm_summary_spec(pharos_tagged_lineage(), description = "covariates"),
    "should be one of"
  )
  expect_error(scm_summary_spec(pharos_tagged_lineage(), description = "x"), "should be one of")
  # The earlier names for the choices are gone.
  expect_error(scm_summary_spec(pharos_tagged_lineage(), description = "step"), "should be one of")
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
    "Working full model", "Backward round 1", "Backward round 2", "Reduced covariate model",
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
      "Working full model", "Backward round 1", "Backward round 2", "Reduced covariate model"
    )
  )

  row <- function(model, section) data[data$model == model & data$section == section, ]
  # A forward step: the candidate adds a parameter to its round's reference.
  added <- row("1001_wt_cl", "Forward round 1")
  expect_equal(added$based_on, "1001_base")
  expect_equal(added$description, "add WT_CL")
  expect_equal(added$dofv, -20)
  # A backward step: the candidate drops one, and the reference is the fuller model.
  dropped <- row("1001_crcl_cl", "Backward round 1")
  expect_equal(dropped$based_on, "1001_crcl_cl")
  expect_equal(dropped$dofv, 6)
  # The working full and reduced covariate models are based on the reference fit.
  working_full <- row("1001_scm_working_full", "Working full model")
  expect_equal(working_full$based_on, "1001_base")
  expect_equal(working_full$dofv, 973.8 - 1000)
  final <- row("1001_scm_reduced_covariate", "Reduced covariate model")
  expect_equal(final$based_on, "1001_base")
  expect_equal(final$dofv, 979.5 - 1000)

  # By default each row shows what the fit added or dropped alone, so the
  # stage fits, which do not repeat their name, show nothing.
  expect_equal(
    data$description[data$section == "Backward round 1"],
    c("drop CRCL_CL", "drop WT_CL")
  )
  expect_equal(row("1001_base", "Reference model")$description, "")
  expect_equal(working_full$description, "")
  expect_equal(final$description, "")

  # With description = "verbose", each row also shows the covariates its
  # model has, which is all a stage fit shows.
  verbose <- apply_summary_spec(tree, scm_summary_spec(tree, description = "verbose"))
  expect_equal(verbose$description[verbose$model == "1001_wt_cl"][1], "add WT_CL; covariates: WT_CL")
  expect_equal(
    verbose$description[verbose$section %in% c("Reference model", "Working full model", "Reduced covariate model")],
    c("covariates: none", "covariates: WT_CL, CRCL_CL", "covariates: WT_CL")
  )
  expect_equal(
    verbose$description[verbose$section == "Forward round 2"],
    c("add CRCL_CL; covariates: WT_CL, CRCL_CL", "add WT_V; covariates: WT_CL, WT_V")
  )

  stages <- c("Reference model", "Working full model", "Reduced covariate model")
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

  # With both, the same rows have the Round Result and the bold rows.
  both <- apply_summary_spec(tree, scm_summary_spec(tree, selected = c("column", "bold")))
  expect_equal(both$round_result, marked$round_result)
  expect_equal(both$.bold, bold$.bold)

  # With selected = "path", only the stages and the kept candidates stay,
  # unmarked, with the rounds grouped by direction.
  kept <- apply_summary_spec(tree, scm_summary_spec(tree, selected = "path"))
  expect_equal(kept$model, data$model[data$section %in% stages | !is.na(marked$round_result)])
  expect_false(any(c("round_result", ".bold") %in% names(kept)))
  expect_lt(nrow(kept), nrow(data))
  expect_equal(
    kept$section,
    c("Reference model", "Forward", "Forward", "Working full model", "Backward", "Reduced covariate model")
  )
  expect_equal(kept$dofv[kept$model == "1001_wt_cl" & kept$section == "Forward"], -20)

  # Without a backward step that dropped anything, the working full model
  # is left out: the reduced covariate model is the result.
  forward_only <- tree
  forward_only$nodes <- Filter(
    function(node) !any(grepl("^backward_round", unlist(node$model$tags))),
    forward_only$nodes
  )
  path <- apply_summary_spec(forward_only, scm_summary_spec(forward_only, selected = "path"))
  expect_equal(path$section, c("Reference model", "Forward", "Forward", "Reduced covariate model"))
  expect_false("1001_scm_working_full" %in% path$model)
  # The earlier name for the choice is gone.
  expect_error(scm_summary_spec(tree, selected = "only"), "must be")
  expect_true("1001_scm_working_full" %in% kept$model)
  # With every candidate shown, the rounds keep their own sections.
  expect_true("Forward round 1" %in% apply_summary_spec(forward_only, scm_summary_spec(forward_only))$section)

  shown <- as.data.frame(marked)[, c(
    "section", "model", "based_on", "description", "ofv", "dofv", "pvalue", "df", "round_result"
  )]
  expect_snapshot(print(shown, digits = 4, row.names = FALSE))

  expect_equal(make_summary_table(marked, output = "data")@col_labels$round_result, "Round Result")
  snapshot_gt(make_summary_table(data), "scm-summary-gt")
  snapshot_gt(make_summary_table(marked), "scm-summary-column-gt")
  snapshot_gt(make_summary_table(kept), "scm-summary-path-gt")
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
  # The reduced covariate model is tested against the reference fit it is based on.
  final <- row("1001_scm_reduced_covariate", "Reduced covariate model")
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
