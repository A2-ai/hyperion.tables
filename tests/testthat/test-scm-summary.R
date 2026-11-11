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
# pharos writes beside each SCM fit: each fit's description is its step, and
# no node says which covariates its model has.
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
    scm_node("scm/base/run001_base.mod", "model/nonmem/run001.mod", "base model", c("scm", "base")),
    scm_node("scm/forward_round1/run001_wt_cl.mod", "scm/base/run001_base.mod", "add WT_CL", c("scm", "forward_round1", "selected")),
    scm_node("scm/forward_round1/run001_wt_v.mod", "scm/base/run001_base.mod", "add WT_V", c("scm", "forward_round1")),
    scm_node("scm/forward_round2/run001_wt_v_try2.mod", "scm/forward_round1/run001_wt_cl.mod", "add WT_V", c("scm", "forward_round2", "retry")),
    scm_node("scm/working_full/run001_scm_working_full.mod", "scm/forward_round1/run001_wt_cl.mod", "working full model", c("scm", "working_full")),
    scm_node("scm/backward_round1/run001_wt_cl.mod", "scm/forward_round1/run001_wt_cl.mod", "drop WT_CL", c("scm", "backward_round1")),
    scm_node("scm/reduced_covariate/run001_scm_reduced_covariate.mod", "scm/forward_round1/run001_wt_cl.mod", "reduced covariate model", c("scm", "reduced_covariate"))
  )
  structure(list(nodes = nodes), class = "hyperion_nonmem_tree")
}

test_that("scm_summary_spec() is a SummarySpec that needs no lineage", {
  spec <- scm_summary_spec()
  expect_true(S7::S7_inherits(spec, SummarySpec))
  expect_true(is_scm_summary_spec(spec))
  expect_false(is_scm_summary_spec(SummarySpec()))

  # The usual spec settings: the SCM fits, the plain columns, no footnotes.
  expect_equal(get_spec_tag_filter(spec)$include, "scm")
  expect_equal(get_spec_columns(spec), c("based_on", "description", "ofv", "dofv", "pvalue"))
  expect_null(get_spec_footnotes(spec))
  expect_equal(get_spec_title(spec), "")
  expect_equal(get_spec_title(scm_summary_spec(title = "Covariate search")), "Covariate search")
  # Its sections wait for the lineage.
  expect_length(spec@sections@rules, 0)

  # Its SCM settings, with their defaults.
  expect_equal(spec@description, "sparse")
  expect_false(spec@selected@path)
  expect_false(spec@selected@column)
  expect_false(spec@selected@bold)
  expect_equal(scm_summary_spec(description = "verbose")@description, "verbose")
  expect_true(scm_summary_spec(selected = ScmSelectedOptions(path = TRUE))@selected@path)

  # The modifiers take it and it stays an SCM spec.
  modified <- spec |>
    set_spec_pvalue(scientific = TRUE) |>
    set_spec_title("Search")
  expect_true(is_scm_summary_spec(modified))
  expect_equal(get_spec_title(modified), "Search")

  expect_error(scm_summary_spec(title = 1), "single character string")
  expect_error(scm_summary_spec(description = "x"), "should be one of")
  expect_error(scm_summary_spec(description = "covariates"), "should be one of")
  expect_error(scm_summary_spec(selected = "column"), "<ScmSelectedOptions>")
  # The path is one of the `selected` options, not an argument of its own.
  expect_error(scm_summary_spec(path = TRUE), "unused argument")
  expect_error(scm_summary_spec(other_section_label = 1), "single character string")
  # The lineage goes to apply_summary_spec(), not here.
  expect_error(scm_summary_spec(pharos_tagged_lineage()), "does not take the lineage")
})

test_that("ScmSelectedOptions() holds how the selected candidate is marked", {
  options <- ScmSelectedOptions()
  expect_false(options@column)
  expect_false(options@bold)
  expect_false(options@path)

  both <- ScmSelectedOptions(column = TRUE, bold = TRUE)
  expect_true(both@column)
  expect_true(both@bold)
  expect_true(ScmSelectedOptions(path = TRUE)@path)
  expect_error(ScmSelectedOptions(path = NA), "@path must be TRUE or FALSE")

  expect_error(ScmSelectedOptions(column = NA), "@column must be TRUE or FALSE")
  expect_error(ScmSelectedOptions(bold = c(TRUE, FALSE)), "@bold must be TRUE or FALSE")
  # The Round Result column's label is fixed.
  expect_error(ScmSelectedOptions(column_label = "Step"), "unused argument")
})

test_that("an SCM spec sections the rounds of the lineage it is applied to", {
  tree <- pharos_tagged_lineage()
  spec <- scm_spec_sections(scm_summary_spec(), tree)
  expect_equal(
    section_labels(spec),
    c(
      "Base model", "Forward round 1", "Forward round 2", "Working full model",
      "Backward round 1", "Reduced covariate model", "Other"
    )
  )
  # With only the path shown, the rounds of a direction share a section.
  expect_equal(
    section_labels(scm_spec_sections(
      scm_summary_spec(selected = ScmSelectedOptions(path = TRUE)),
      tree
    )),
    c("Base model", "Forward", "Working full model", "Backward", "Reduced covariate model", "Other")
  )
  expect_equal(
    section_labels(scm_spec_sections(scm_summary_spec(other_section_label = "Rest"), tree))[7],
    "Rest"
  )

  # Section rules the user set replace the ones it would work out.
  own <- set_spec_sections(scm_summary_spec(), "base" %in% tags ~ "Start")
  expect_equal(section_labels(scm_spec_sections(own, tree)), "Start")
  # Any other spec passes through untouched.
  plain <- SummarySpec()
  expect_identical(scm_spec_sections(plain, tree), plain)

  no_scm <- structure(
    list(nodes = list(scm_node("model/nonmem/run001.mod", NULL, "structural model", "structural"))),
    class = "hyperion_nonmem_tree"
  )
  expect_error(scm_spec_sections(scm_summary_spec(), no_scm), "has no SCM fits")
})

test_that("an SCM spec shapes the summary rows and leaves other specs alone", {
  tree <- pharos_tagged_lineage()
  scm_nodes <- tree$nodes[-1]
  df <- data.frame(
    model = vapply(scm_nodes, function(n) tools::file_path_sans_ext(basename(n$name)), ""),
    .name = vapply(scm_nodes, function(n) tools::file_path_sans_ext(n$name), ""),
    description = vapply(scm_nodes, function(n) n$model$description, ""),
    stringsAsFactors = FALSE
  )
  df$tags <- I(lapply(scm_nodes, function(n) as.character(unlist(n$model$tags))))

  expect_identical(scm_summary_rows(df, SummarySpec(), tree), df)

  # Sparse: a candidate shows its step, a stage fit nothing.
  sparse <- scm_summary_rows(df, scm_summary_spec(), tree)
  expect_equal(
    sparse$description,
    c("", "add WT_CL", "add WT_V", "add WT_V", "", "drop WT_CL", "")
  )
  expect_false(any(c("round_result", ".bold") %in% names(sparse)))

  # Verbose needs the covariates on the lineage; pharos' metadata has none.
  expect_warning(
    verbose <- scm_summary_rows(df, scm_summary_spec(description = "verbose"), tree),
    "does not say which covariates"
  )
  expect_equal(verbose$description, sparse$description)

  # With them, each row adds its model's covariates; a stage fit shows only those.
  covariates <- list(
    character(), "WT_CL", "WT_V", c("WT_CL", "WT_V"), "WT_CL", character(), "WT_CL"
  )
  with_covariates <- tree
  for (i in seq_along(covariates)) {
    with_covariates$nodes[[i + 1]]$model$covariates <- as.list(covariates[[i]])
  }
  verbose <- scm_summary_rows(df, scm_summary_spec(description = "verbose"), with_covariates)
  expect_equal(
    verbose$description,
    c(
      "covariates: none", "add WT_CL; covariates: WT_CL", "add WT_V; covariates: WT_V",
      "add WT_V; covariates: WT_CL, WT_V", "covariates: WT_CL",
      "drop WT_CL; covariates: none", "covariates: WT_CL"
    )
  )

  # The selected candidate, marked with a column, a bold row, or both.
  marked <- scm_summary_rows(
    df,
    scm_summary_spec(selected = ScmSelectedOptions(column = TRUE, bold = TRUE)),
    tree
  )
  expect_equal(marked$round_result, c(NA, "added", NA, NA, NA, NA, NA))
  expect_equal(marked$.bold, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  column <- scm_summary_rows(df, scm_summary_spec(selected = ScmSelectedOptions(column = TRUE)), tree)
  expect_false(".bold" %in% names(column))
  bold <- scm_summary_rows(df, scm_summary_spec(selected = ScmSelectedOptions(bold = TRUE)), tree)
  expect_false("round_result" %in% names(bold))

  # The path: the stage fits and the selected candidates. No backward round
  # dropped anything here, so the working full model is left out.
  path <- scm_summary_rows(
    df,
    scm_summary_spec(selected = ScmSelectedOptions(path = TRUE)),
    tree
  )
  expect_equal(path$model, c("run001_base", "run001_wt_cl", "run001_scm_reduced_covariate"))

  # The labels an SCM table adds or changes; any other spec's are untouched.
  labels <- list(dofv = "$\\Delta$OFV")
  expect_identical(scm_summary_labels(labels, marked, SummarySpec()), labels)
  scm_labels <- scm_summary_labels(
    labels,
    marked,
    scm_summary_spec(selected = ScmSelectedOptions(column = TRUE))
  )
  expect_equal(scm_labels$round_result, "Round Result")
  expect_equal(scm_labels$dofv, "\u0394OFV")
})

test_that("the general spec functions carry no SCM arguments", {
  expect_false(any(
    c("pvalue_direction", "bold_rows", "description_transform", "computed_columns", "model_filter") %in%
      names(formals(SummarySpec))
  ))
  expect_false("direction" %in% names(formals(set_spec_pvalue)))
})

test_that("an SCM lineage summarizes as one row per fit with its round's dOFV", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  tree <- hyperion::get_scm_lineage(scm_fixture_config())
  data <- apply_summary_spec(tree, scm_summary_spec())

  # By default the candidate a round selected is not marked.
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
  # model has, from the process record, which is all a stage fit shows.
  verbose <- apply_summary_spec(tree, scm_summary_spec(description = "verbose"))
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

  # With a Round Result column, the same rows gain the round's result on
  # the candidate it selected, and nothing on the others.
  marked <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(column = TRUE))
  )
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
  # The reference and stage fits are not candidates, so they have no result.
  expect_true(all(is.na(marked$round_result[marked$section %in% stages])))

  # With bold, the selected candidates are in bold and nothing else.
  bold <- apply_summary_spec(tree, scm_summary_spec(selected = ScmSelectedOptions(bold = TRUE)))
  expect_false("round_result" %in% names(bold))
  expect_equal(bold$.bold, !is.na(marked$round_result))

  # With both, the same rows have the Round Result and the bold rows.
  both <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(column = TRUE, bold = TRUE))
  )
  expect_equal(both$round_result, marked$round_result)
  expect_equal(both$.bold, bold$.bold)

  # With the path, only the stages and the selected candidates stay,
  # with the rounds grouped by direction.
  kept <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(path = TRUE))
  )
  expect_equal(kept$model, data$model[data$section %in% stages | !is.na(marked$round_result)])
  expect_false(any(c("round_result", ".bold") %in% names(kept)))
  expect_lt(nrow(kept), nrow(data))
  expect_equal(
    kept$section,
    c("Reference model", "Forward", "Forward", "Working full model", "Backward", "Reduced covariate model")
  )
  expect_equal(kept$dofv[kept$model == "1001_wt_cl" & kept$section == "Forward"], -20)
  # The path can carry the Round Result too.
  kept_marked <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(path = TRUE, column = TRUE))
  )
  expect_equal(kept_marked$round_result, c(NA, "added", "added", NA, "dropped", NA))

  # Without a backward step that dropped anything, the working full model
  # is left out: the reduced covariate model is the result.
  forward_only <- tree
  forward_only$nodes <- Filter(
    function(node) !any(grepl("^backward_round", unlist(node$model$tags))),
    forward_only$nodes
  )
  path <- apply_summary_spec(
    forward_only,
    scm_summary_spec(selected = ScmSelectedOptions(path = TRUE))
  )
  expect_equal(path$section, c("Reference model", "Forward", "Forward", "Reduced covariate model"))
  expect_false("1001_scm_working_full" %in% path$model)
  expect_true("1001_scm_working_full" %in% kept$model)
  # With every candidate shown, the rounds keep their own sections.
  expect_true("Forward round 1" %in% apply_summary_spec(forward_only, scm_summary_spec())$section)

  shown <- as.data.frame(marked)[, c(
    "section", "model", "based_on", "description", "ofv", "dofv", "pvalue", "df", "round_result"
  )]
  expect_snapshot(print(shown, digits = 4, row.names = FALSE))

  labels <- make_summary_table(marked, output = "data")@col_labels
  expect_equal(labels$round_result, "Round Result")
  expect_equal(labels$dofv, "\u0394OFV")
  snapshot_gt(make_summary_table(data), "scm-summary-gt")
  snapshot_gt(make_summary_table(marked), "scm-summary-column-gt")
  snapshot_gt(make_summary_table(both), "scm-summary-bold-gt")
  snapshot_gt(make_summary_table(kept), "scm-summary-path-gt")
})

test_that("an SCM table's p-value column tests both directions", {
  testthat::skip_if_not(nzchar(scm_fixture_config()), "hyperion's SCM fixture not found")
  withr::local_options(hyperion.config_dir = system.file(package = "hyperion"))

  tree <- hyperion::get_scm_lineage(scm_fixture_config())
  data <- apply_summary_spec(tree, scm_summary_spec())
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

  # Any other summary spec keeps the backward step's dOFV but gives it no test.
  plain <- apply_summary_spec(
    tree,
    SummarySpec() |>
      set_spec_tag_filter(include = "scm") |>
      set_spec_columns("based_on", "ofv", "dofv", "pvalue")
  )
  dropped <- plain[which(plain$dofv == 6), ]
  expect_equal(nrow(dropped), 1)
  expect_true(is.na(dropped$df))
  expect_true(is.na(dropped$pvalue))
})
