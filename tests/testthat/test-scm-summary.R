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

test_that("ScmSummarySpec is a SummarySpec with the SCM settings", {
  spec <- ScmSummarySpec()
  # A child of SummarySpec, as SummarySpec is of BaseSpec.
  expect_true(S7::S7_inherits(spec, ScmSummarySpec))
  expect_true(S7::S7_inherits(spec, SummarySpec))
  expect_true(S7::S7_inherits(spec, BaseSpec))
  expect_false(S7::S7_inherits(SummarySpec(), ScmSummarySpec))
  expect_false(S7::S7_inherits(TableSpec(), ScmSummarySpec))
  # The preset makes the same spec.
  expect_equal(spec, scm_summary_spec())

  # SummarySpec's arguments pass through; those not given are the SCM table's.
  expect_equal(get_spec_title(ScmSummarySpec(title = "Search")), "Search")
  expect_equal(get_spec_tag_filter(ScmSummarySpec(title = "Search"))$include, "scm")
  expect_equal(get_spec_columns(ScmSummarySpec(columns = c("ofv", "dofv"))), c("ofv", "dofv"))
  expect_equal(ScmSummarySpec(n_decimals_ofv = 1)@n_decimals_ofv, 1)

  expect_error(ScmSummarySpec(show_all_covariates = NA), "@show_all_covariates must be TRUE or FALSE")
  expect_error(
    ScmSummarySpec(show_all_covariates = c(TRUE, FALSE)),
    "@show_all_covariates must be TRUE or FALSE"
  )
  # The description setting it replaced is gone.
  expect_error(ScmSummarySpec(description = "verbose"))
  expect_error(ScmSummarySpec(other_section_label = c("a", "b")), "@other_section_label")
  expect_error(ScmSummarySpec(selected = "column"))
  # The parent's validation still applies.
  expect_error(ScmSummarySpec(columns = "not_a_column"))
})

test_that("scm_summary_spec() is a ScmSummarySpec that needs no lineage", {
  spec <- scm_summary_spec()
  expect_true(S7::S7_inherits(spec, ScmSummarySpec))
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
  expect_false(spec@show_all_covariates)
  expect_false(spec@selected@selected_only)
  expect_true(spec@selected@column)
  expect_false(spec@selected@bold)
  expect_true(scm_summary_spec(show_all_covariates = TRUE)@show_all_covariates)
  expect_true(scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE))@selected@selected_only)

  # The modifiers take it and it stays an SCM spec.
  modified <- spec |>
    set_spec_pvalue(scientific = TRUE) |>
    set_spec_title("Search")
  expect_true(is_scm_summary_spec(modified))
  expect_equal(get_spec_title(modified), "Search")

  expect_error(scm_summary_spec(title = 1), "single character string")
  expect_error(scm_summary_spec(show_all_covariates = "yes"), "must be TRUE or FALSE")
  expect_error(scm_summary_spec(show_all_covariates = NA), "must be TRUE or FALSE")
  # Listing the covariates is a flag; the description setting is gone.
  expect_error(scm_summary_spec(description = "verbose"), "unused argument")
  expect_error(scm_summary_spec(selected = "column"), "<ScmSelectedOptions>")
  # Showing only the selected candidates is one of the `selected` options,
  # not an argument of its own, and its earlier name is gone.
  expect_error(scm_summary_spec(selected_only = TRUE), "unused argument")
  expect_error(ScmSelectedOptions(path = TRUE), "unused argument")
  expect_error(scm_summary_spec(other_section_label = 1), "single character string")
  # The lineage goes to apply_summary_spec(), not here.
  expect_error(scm_summary_spec(pharos_tagged_lineage()), "does not take the lineage")
})

test_that("ScmSelectedOptions() holds how the selected candidate is marked", {
  # The Round Result column is on unless turned off.
  options <- ScmSelectedOptions()
  expect_true(options@column)
  expect_false(options@bold)
  expect_false(options@selected_only)
  expect_false(ScmSelectedOptions(column = FALSE)@column)

  both <- ScmSelectedOptions(bold = TRUE)
  expect_true(both@column)
  expect_true(both@bold)
  expect_true(ScmSelectedOptions(selected_only = TRUE)@selected_only)
  expect_error(ScmSelectedOptions(selected_only = NA), "@selected_only must be TRUE or FALSE")

  expect_error(ScmSelectedOptions(column = NA), "@column must be TRUE or FALSE")
  expect_error(ScmSelectedOptions(bold = c(TRUE, FALSE)), "@bold must be TRUE or FALSE")
  # The Round Result column's label is fixed.
  expect_error(ScmSelectedOptions(column_label = "Step"), "unused argument")
})

test_that("set_spec_scm_selected() sets the selected options of an SCM spec", {
  spec <- scm_summary_spec() |>
    set_spec_scm_selected(column = FALSE, bold = TRUE)
  expect_true(is_scm_summary_spec(spec))
  expect_false(spec@selected@column)
  expect_true(spec@selected@bold)
  expect_false(spec@selected@selected_only)

  # A setting not given keeps its value.
  spec <- set_spec_scm_selected(spec, selected_only = TRUE, bold = FALSE)
  expect_false(spec@selected@column)
  expect_false(spec@selected@bold)
  expect_true(spec@selected@selected_only)
  expect_identical(set_spec_scm_selected(spec), spec)
  expect_true(set_spec_scm_selected(spec, column = TRUE)@selected@column)

  # It is the same spec as the one made in one call.
  expect_equal(
    scm_summary_spec() |> set_spec_scm_selected(selected_only = TRUE),
    scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE))
  )

  # The getter returns the options object.
  expect_identical(get_spec_scm_selected(spec), spec@selected)
  expect_true(S7::S7_inherits(get_spec_scm_selected(scm_summary_spec()), ScmSelectedOptions))
  expect_true(get_spec_scm_selected(scm_summary_spec())@column)
  expect_error(get_spec_scm_selected(SummarySpec()), "<ScmSummarySpec>")
  expect_error(get_spec_scm_selected(TableSpec()), "<ScmSummarySpec>")

  expect_error(set_spec_scm_selected(SummarySpec(), column = TRUE), "<ScmSummarySpec>")
  expect_error(set_spec_scm_selected(TableSpec(), column = TRUE), "<ScmSummarySpec>")
  expect_error(set_spec_scm_selected(scm_summary_spec(), column = NA), "@column must be TRUE or FALSE")
  expect_error(set_spec_scm_selected(scm_summary_spec(), TRUE))
})

test_that("set_spec_scm_rounds() picks the rounds an SCM spec shows", {
  # Every round by default.
  expect_null(get_spec_scm_rounds(scm_summary_spec()))

  spec <- scm_summary_spec() |>
    set_spec_scm_rounds("forward_round1", "forward_round2")
  expect_true(is_scm_summary_spec(spec))
  expect_equal(get_spec_scm_rounds(spec), c("forward_round1", "forward_round2"))
  # As a vector, with a direction, and without repeats.
  expect_equal(
    get_spec_scm_rounds(set_spec_scm_rounds(spec, c("backward", "forward_round1", "backward"))),
    c("backward", "forward_round1")
  )
  # With none, every round again.
  expect_null(get_spec_scm_rounds(set_spec_scm_rounds(spec)))
  expect_null(get_spec_scm_rounds(set_spec_scm_rounds(spec, NULL)))
  # The same spec as the one made in one call, or from the class.
  expect_equal(spec, scm_summary_spec(rounds = c("forward_round1", "forward_round2")))
  expect_equal(spec, ScmSummarySpec(rounds = c("forward_round1", "forward_round2")))

  # NA is no round: only the milestones. It is held as a character NA.
  milestones <- set_spec_scm_rounds(spec, NA)
  expect_identical(get_spec_scm_rounds(milestones), NA_character_)
  expect_equal(milestones, scm_summary_spec(rounds = NA))
  expect_equal(milestones, ScmSummarySpec(rounds = NA))
  expect_equal(milestones, scm_summary_spec(rounds = NA_character_))
  expect_null(get_spec_scm_rounds(set_spec_scm_rounds(milestones)))
  # It does not combine with rounds.
  expect_error(scm_summary_spec(rounds = c(NA, "forward")), "@rounds must name rounds")
  expect_error(set_spec_scm_rounds(spec, NA, "forward"), "@rounds must name rounds")

  # Only rounds and directions are names of rounds; a stage fit is not one.
  expect_error(set_spec_scm_rounds(spec, "Forward round 1"), "@rounds must name rounds")
  expect_error(set_spec_scm_rounds(spec, "working_full"), "Got: 'working_full'")
  expect_error(set_spec_scm_rounds(spec, 1), "given as strings")
  expect_error(scm_summary_spec(rounds = 1), "character vector")
  expect_error(scm_summary_spec(rounds = TRUE), "character vector")
  expect_error(scm_summary_spec(rounds = "round1"), "@rounds must name rounds")
  expect_error(ScmSummarySpec(rounds = character()), "@rounds must name rounds")
  expect_error(set_spec_scm_rounds(SummarySpec(), "forward"), "<ScmSummarySpec>")
  expect_error(get_spec_scm_rounds(SummarySpec()), "<ScmSummarySpec>")
  expect_error(get_spec_scm_rounds(TableSpec()), "<ScmSummarySpec>")

  # The rows a round keeps: its candidates, and the stage fits, which are
  # in no round.
  tree <- pharos_tagged_lineage()
  scm_nodes <- tree$nodes[-1]
  df <- data.frame(
    model = vapply(scm_nodes, function(n) tools::file_path_sans_ext(basename(n$name)), ""),
    .name = vapply(scm_nodes, function(n) tools::file_path_sans_ext(n$name), ""),
    stringsAsFactors = FALSE
  )
  df$tags <- I(lapply(scm_nodes, function(n) as.character(unlist(n$model$tags))))
  shown <- function(...) {
    scm_summary_rows(df, set_spec_scm_rounds(scm_summary_spec(), ...), tree)$model
  }
  stage <- c("run001_base", "run001_scm_working_full", "run001_scm_reduced_covariate")
  expect_equal(shown(), df$model)
  expect_equal(
    shown("forward_round1"),
    c("run001_base", "run001_wt_cl", "run001_wt_v", "run001_scm_working_full", "run001_scm_reduced_covariate")
  )
  expect_equal(
    shown("forward"),
    df$model[df$model != "run001_wt_cl" | !grepl("backward", df$.name)]
  )
  expect_equal(
    shown("backward"),
    c("run001_base", "run001_scm_working_full", "run001_wt_cl", "run001_scm_reduced_covariate")
  )
  expect_equal(shown("forward", "backward"), df$model)
  # A round the lineage does not have is a warning; the stage fits stay.
  expect_warning(only_stage <- shown("forward_round9"), "has no fits in: 'forward_round9'")
  expect_equal(only_stage, stage)
  # NA asks for the stage fits alone, so it is no warning.
  expect_no_warning(milestones_only <- shown(NA))
  expect_equal(milestones_only, stage)
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
  # With only the selected candidates shown, the rounds of a direction share a section.
  expect_equal(
    section_labels(scm_spec_sections(
      scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE)),
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

  # By default a candidate shows its step, a stage fit nothing, and the
  # candidate its round selected has the round's result.
  plain <- scm_summary_rows(df, scm_summary_spec(), tree)
  expect_equal(
    plain$description,
    c("", "add WT_CL", "add WT_V", "add WT_V", "", "drop WT_CL", "")
  )
  expect_equal(plain$round_result, c(NA, "added", NA, NA, NA, NA, NA))
  expect_false(".bold" %in% names(plain))

  # Listing the covariates needs them on the lineage; pharos' metadata has none.
  expect_warning(
    listed <- scm_summary_rows(df, scm_summary_spec(show_all_covariates = TRUE), tree),
    "does not say which covariates"
  )
  expect_equal(listed$description, plain$description)

  # With them, each row adds its model's covariates; a stage fit shows only those.
  covariates <- list(
    character(), "WT_CL", "WT_V", c("WT_CL", "WT_V"), "WT_CL", character(), "WT_CL"
  )
  with_covariates <- tree
  for (i in seq_along(covariates)) {
    with_covariates$nodes[[i + 1]]$model$covariates <- as.list(covariates[[i]])
  }
  listed <- scm_summary_rows(df, scm_summary_spec(show_all_covariates = TRUE), with_covariates)
  expect_equal(
    listed$description,
    c(
      "covariates: none", "add WT_CL; covariates: WT_CL", "add WT_V; covariates: WT_V",
      "add WT_V; covariates: WT_CL, WT_V", "covariates: WT_CL",
      "drop WT_CL; covariates: none", "covariates: WT_CL"
    )
  )

  # The selected candidate, marked with a column, a bold row, or both.
  marked <- scm_summary_rows(
    df,
    scm_summary_spec(selected = ScmSelectedOptions(bold = TRUE)),
    tree
  )
  expect_equal(marked$round_result, c(NA, "added", NA, NA, NA, NA, NA))
  expect_equal(marked$.bold, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  bold <- scm_summary_rows(
    df,
    scm_summary_spec(selected = ScmSelectedOptions(column = FALSE, bold = TRUE)),
    tree
  )
  expect_false("round_result" %in% names(bold))
  expect_equal(bold$.bold, marked$.bold)
  unmarked <- scm_summary_rows(df, scm_summary_spec(selected = ScmSelectedOptions(column = FALSE)), tree)
  expect_false(any(c("round_result", ".bold") %in% names(unmarked)))

  # With the milestones alone there is no round to report a result for, so
  # the column is left out.
  milestones <- scm_summary_rows(df, scm_summary_spec(rounds = NA), tree)
  expect_equal(milestones$model, c("run001_base", "run001_scm_working_full", "run001_scm_reduced_covariate"))
  expect_false("round_result" %in% names(milestones))

  # Selected only: the stage fits and the selected candidates. No backward round
  # dropped anything here, so the working full model is left out.
  only <- scm_summary_rows(
    df,
    scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE)),
    tree
  )
  expect_equal(only$model, c("run001_base", "run001_wt_cl", "run001_scm_reduced_covariate"))

  # The labels an SCM table adds or changes; any other spec's are untouched.
  labels <- list(dofv = "$\\Delta$OFV")
  expect_identical(scm_summary_labels(labels, marked, SummarySpec()), labels)
  scm_labels <- scm_summary_labels(labels, marked, scm_summary_spec())
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
  data <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(column = FALSE))
  )

  # Without the Round Result column, the candidate a round selected is not
  # marked.
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

  # With show_all_covariates, each row also shows the covariates its
  # model has, from the process record, which is all a stage fit shows.
  listed <- apply_summary_spec(tree, scm_summary_spec(show_all_covariates = TRUE))
  expect_equal(listed$description[listed$model == "1001_wt_cl"][1], "add WT_CL; covariates: WT_CL")
  expect_equal(
    listed$description[listed$section %in% c("Reference model", "Working full model", "Reduced covariate model")],
    c("covariates: none", "covariates: WT_CL, CRCL_CL", "covariates: WT_CL")
  )
  expect_equal(
    listed$description[listed$section == "Forward round 2"],
    c("add CRCL_CL; covariates: WT_CL, CRCL_CL", "add WT_V; covariates: WT_CL, WT_V")
  )

  stages <- c("Reference model", "Working full model", "Reduced covariate model")
  expect_true(is.na(row("1001_base", "Reference model")$dofv))

  # By default the table has a Round Result column: the same rows gain the
  # round's result on the candidate it selected, and nothing on the others.
  marked <- apply_summary_spec(tree, scm_summary_spec())
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

  # With bold alone, the selected candidates are in bold and nothing else.
  bold <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(column = FALSE, bold = TRUE))
  )
  expect_false("round_result" %in% names(bold))
  expect_equal(bold$.bold, !is.na(marked$round_result))

  # With both, the same rows have the Round Result and the bold rows.
  both <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(bold = TRUE))
  )
  expect_equal(both$round_result, marked$round_result)
  expect_equal(both$.bold, bold$.bold)

  # With selected_only, only the stages and the selected candidates stay,
  # with the rounds grouped by direction.
  kept <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE, column = FALSE))
  )
  expect_equal(kept$model, data$model[data$section %in% stages | !is.na(marked$round_result)])
  expect_false(any(c("round_result", ".bold") %in% names(kept)))
  expect_lt(nrow(kept), nrow(data))
  expect_equal(
    kept$section,
    c("Reference model", "Forward", "Forward", "Working full model", "Backward", "Reduced covariate model")
  )
  expect_equal(kept$dofv[kept$model == "1001_wt_cl" & kept$section == "Forward"], -20)
  # It carries the Round Result unless that is turned off.
  kept_marked <- apply_summary_spec(
    tree,
    scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE))
  )
  expect_equal(kept_marked$round_result, c(NA, "added", "added", NA, "dropped", NA))

  # With rounds picked, the table has those rounds and the stage fits, and
  # every row keeps the dOFV and p-value it has in the full table.
  two <- apply_summary_spec(
    tree,
    scm_summary_spec() |> set_spec_scm_rounds("forward_round2", "backward_round1")
  )
  expect_equal(
    unique(two$section),
    c(
      "Reference model", "Forward round 2", "Working full model",
      "Backward round 1", "Reduced covariate model"
    )
  )
  in_two <- data$section %in% unique(two$section)
  expect_equal(two$model, data$model[in_two])
  expect_equal(two$dofv, data$dofv[in_two])
  expect_equal(two$pvalue, data$pvalue[in_two])
  expect_equal(two$round_result, marked$round_result[in_two])
  # With rounds = NA, the table has the milestones alone, with the dOFV
  # they have in the full table and no Round Result column.
  milestones <- apply_summary_spec(tree, scm_summary_spec(rounds = NA))
  expect_equal(milestones$section, stages)
  expect_equal(milestones$dofv, data$dofv[data$section %in% stages])
  expect_false("round_result" %in% names(milestones))
  # A direction names all of its rounds, and combines with selected_only.
  backward <- apply_summary_spec(tree, scm_summary_spec() |> set_spec_scm_rounds("backward"))
  expect_equal(
    unique(backward$section),
    c(
      "Reference model", "Working full model", "Backward round 1",
      "Backward round 2", "Reduced covariate model"
    )
  )
  backward_only <- apply_summary_spec(
    tree,
    scm_summary_spec() |>
      set_spec_scm_selected(selected_only = TRUE) |>
      set_spec_scm_rounds("backward")
  )
  expect_equal(
    backward_only$section,
    c("Reference model", "Working full model", "Backward", "Reduced covariate model")
  )

  # Without a backward step that dropped anything, the working full model
  # is left out: the reduced covariate model is the result.
  forward_only <- tree
  forward_only$nodes <- Filter(
    function(node) !any(grepl("^backward_round", unlist(node$model$tags))),
    forward_only$nodes
  )
  only <- apply_summary_spec(
    forward_only,
    scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE))
  )
  expect_equal(only$section, c("Reference model", "Forward", "Forward", "Reduced covariate model"))
  expect_false("1001_scm_working_full" %in% only$model)
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
  snapshot_gt(make_summary_table(kept), "scm-summary-selected-only-gt")
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
