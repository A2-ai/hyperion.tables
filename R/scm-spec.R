#' Pre-configured SummarySpec for a stepwise covariate modeling process
#'
#' `scm_summary_spec()` returns a [SummarySpec] for the run summary table of
#' an SCM process: the fits tagged `"scm"`, in one section per round, from
#' the round tag pharos writes to each fit's metadata (`"base"` or `"full"`,
#' `"forward_round1"`, ..., `"forward_final"`, `"final"`), in the order the
#' process ran them. It composes the usual spec settings and nothing else:
#' it is what you would write by hand with [set_spec_sections()], as in
#' `set_spec_sections("base" %in% tags ~ "Base model", "forward_round1" %in%
#' tags ~ "Forward round 1", ...)`, for however many rounds the process ran.
#'
#' Every fit is based on the model its round tested against, so dOFV is the
#' round's own; the working full and reduced covariate models are based on
#' the reference fit, so theirs is against it. The columns are Reference,
#' Description, OFV, dOFV and p-value. `pvalue_direction` is `"both"`, so
#' the backward steps are tested too, with the round's reference as the
#' fuller model. The table has no footnotes. Customize further with the
#' `set_spec_*()` modifiers.
#'
#' `selected` picks the one way the table shows the candidate each round
#' kept, the fit tagged `"selected"`. The choices exclude one another:
#' \itemize{
#'   \item `"none"` (the default) does not mark it: every candidate shows
#'     alike, with no Round Result column and no bold row.
#'   \item `"column"` adds a Round Result column, which reads "added" or
#'     "dropped" on the kept candidate's row and is empty elsewhere.
#'   \item `"bold"` renders the kept candidate's row in bold. There is no
#'     Round Result column.
#'   \item `"only"` shows no other candidate, so there is nothing to mark
#'     and the table has no Round Result column and no bold row. It keeps
#'     the models that mattered: the base (or full) model, the working
#'     full and reduced covariate models, and in each round the candidate
#'     it kept, so a round that kept none shows no row. Since each round
#'     then has at most one row, the rounds are grouped by direction
#'     instead of sectioned one by one: the reference model, then the
#'     forward steps under "Forward", the working full model, the backward
#'     steps under "Backward", and the reduced covariate model. When no
#'     backward round dropped anything, the working full model says
#'     nothing the reduced covariate model does not, so it is left out too.
#' }
#'
#' pharos ends every SCM fit's description with the covariates its model
#' has, e.g. `"SCM forward_round2: add CRCL_CL (attempt 1); covariates:
#' WT_CL, CRCL_CL"`, as does `hyperion::get_scm_lineage()`. `description`
#' picks what the Description column shows: `"full"` (the default), the
#' description as the lineage gives it, as [SummarySpec()] shows it, so
#' each row names both the step and the covariates its model has;
#' `"step"`, the fit's action alone, "add WT_V"; or `"covariates"`, the
#' covariates alone, "WT_CL, CRCL_CL, WT_V". A description without the
#' covariates part shows as it is under either.
#'
#' The section rules end with a catch-all, so rules appended with
#' [set_spec_sections()] never apply; pass `overwrite = TRUE` to replace them.
#'
#' @param lineage The `hyperion_nonmem_tree` holding the SCM fits:
#'   `hyperion::get_model_lineage()` of the project, which reads the metadata
#'   pharos wrote beside each fit, or `hyperion::get_scm_lineage()` of the
#'   process, built from its record. A `hyperion_nonmem_scm` object or SCM
#'   config (`<stem>scm.toml`) is passed to the latter.
#' @param title Table title. `NULL` (the default) gives a table with no
#'   title; pass a string to set one.
#' @param description What the Description column shows for each fit:
#'   `"full"` (the default), the description as the lineage gives it, its
#'   step and the covariates its model has; `"step"`, its action alone; or
#'   `"covariates"`, the covariates its model has alone.
#' @param selected How the table shows the candidate each round kept, one
#'   of: `"none"` (the default), every candidate with the kept one
#'   unmarked; `"column"`, every candidate with a Round Result column
#'   reading "added" or "dropped" on the kept one's row; `"bold"`, every
#'   candidate with the kept one's row in bold and no Round Result column;
#'   or `"only"`, the kept candidates alone, unmarked, with the rounds
#'   grouped by direction.
#' @param other_section_label Section label for a model matching no round.
#'
#' @return A [SummarySpec] object.
#' @seealso [SummarySpec], [set_spec_sections()], [apply_summary_spec()],
#'   [make_summary_table()].
#' @export
#' @examples \dontrun{
#' # From the fits' metadata, as any other lineage
#' lineage <- hyperion::get_model_lineage()
#' lineage |>
#'   apply_summary_spec(scm_summary_spec(lineage)) |>
#'   make_summary_table()
#'
#' # From the process record
#' scm <- hyperion::read_scm("model/nonmem/scm/run001scm.toml")
#' scm |>
#'   hyperion::get_scm_lineage() |>
#'   apply_summary_spec(scm_summary_spec(scm, title = "Covariate search")) |>
#'   make_summary_table()
#'
#' # Show only each fit's step, or only its model's covariates
#' scm_summary_spec(scm, description = "step")
#' scm_summary_spec(scm, description = "covariates")
#'
#' # Mark the candidate each round kept: a Round Result column, or its row
#' # in bold
#' scm_summary_spec(scm, selected = "column")
#' scm_summary_spec(scm, selected = "bold")
#' # Show only the models that mattered: no other candidates, nothing marked
#' scm_summary_spec(scm, selected = "only")
#'
#' # Customize further with modifiers
#' spec <- scm_summary_spec(scm) |>
#'   set_spec_pvalue(scientific = TRUE)
#' # Or without the p-value
#' spec <- scm_summary_spec(scm) |>
#'   set_spec_columns("based_on", "description", "ofv", "dofv")
#' }
scm_summary_spec <- function(
  lineage,
  title = NULL,
  description = c("full", "step", "covariates"),
  selected = c("none", "column", "bold", "only"),
  other_section_label = "Other"
) {
  if (!is.null(title) && !rlang::is_string(title)) {
    rlang::abort("`title` must be a single character string or NULL.")
  }
  description <- match.arg(description)
  selected <- match.arg(selected)
  if (!rlang::is_string(other_section_label)) {
    rlang::abort("`other_section_label` must be a single character string.")
  }
  tree <- if (inherits(lineage, "hyperion_nonmem_tree")) {
    lineage
  } else {
    hyperion::get_scm_lineage(lineage)
  }

  rounds <- scm_lineage_rounds(tree)
  if (length(rounds) == 0) {
    rlang::abort(
      "`lineage` has no SCM fits: no model is tagged \"scm\" with a round."
    )
  }
  # The rules evaluate against each row with `tags` unwrapped; they need
  # base R for `%in%`, nothing else. With only the kept candidates, the
  # rounds of a direction share a section, which comes where its first
  # round ran.
  label <- if (selected == "only") scm_direction_label else scm_round_label
  labels <- vapply(rounds, label, character(1))
  rules <- lapply(unique(labels), function(section) {
    members <- rounds[labels == section]
    rlang::new_formula(
      if (length(members) == 1) {
        rlang::call2("%in%", members, rlang::sym("tags"))
      } else {
        rlang::call2("any", rlang::call2("%in%", members, rlang::sym("tags")))
      },
      section,
      env = baseenv()
    )
  })
  rules <- c(rules, list(rlang::new_formula(TRUE, other_section_label, env = baseenv())))

  spec <- rlang::inject(set_spec_sections(SummarySpec(), !!!rules))
  spec <- set_spec_tag_filter(spec, include = "scm")
  spec <- set_spec_pvalue(spec, direction = "both")
  # The candidate a round kept gets no mark, or a Round Result, or its row
  # in bold, or is the only one shown; the reference and final fits are not
  # candidates.
  columns <- c("based_on", "description", "ofv", "dofv", "pvalue")
  if (selected == "column") {
    spec <- set_spec_computed_column(
      spec,
      "round_result",
      "selected" %in% tags && any(grepl("^forward_round", tags)) ~ "added",
      "selected" %in% tags && any(grepl("^backward_round", tags)) ~ "dropped",
      label = "Round Result"
    )
    columns <- c(columns, "round_result")
  } else if (selected == "bold") {
    spec <- set_spec_bold_rows(
      spec,
      "selected" %in% tags && !any(!!scm_stage_tags %in% tags)
    )
  } else if (selected == "only") {
    # Without a backward step the working full model is the reduced
    # covariate model less its cov step, so only the latter stays.
    stages <- if (scm_lineage_dropped_any(tree)) {
      scm_stage_tags
    } else {
      setdiff(scm_stage_tags, "forward_final")
    }
    spec <- set_spec_model_filter(
      spec,
      "selected" %in% tags || any(!!stages %in% tags)
    )
  }
  if (description != "full") {
    spec <- set_spec_description_transform(
      spec,
      switch(
        description,
        step = scm_description_step,
        covariates = scm_description_covariates
      )
    )
  }
  spec <- rlang::inject(set_spec_columns(spec, !!!columns))
  spec <- set_spec_footnotes(spec, NULL)
  set_spec_title(spec, title %||% "")
}

# pharos and hyperion end an SCM fit's description with the covariates its
# model has: "add CRCL_CL (attempt 1); covariates: WT_CL, CRCL_CL". The
# default shows it whole; the step is what comes before the part, and a
# description without the part is left alone by both transforms.
scm_description_step <- function(x) {
  sub("; covariates: .*$", "", x)
}

scm_description_covariates <- function(x) {
  has_part <- grepl("; covariates: ", x, fixed = TRUE)
  x[has_part] <- sub("^.*; covariates: ", "", x[has_part])
  x
}

# The round tags of the fits that are not round candidates: the reference
# fit (`base` or `full` from pharos, `reference` from hyperion's record
# lineage), the working full model (tagged `forward_final`) and the reduced
# covariate model (tagged `final`).
scm_stage_tags <- c("base", "full", "reference", "forward_final", "final")

# The rounds in the lineage, in the order their fits appear: each SCM fit's
# tag other than "scm", "selected" and "retry".
scm_lineage_rounds <- function(tree) {
  marker_tags <- c("scm", "selected", "retry")
  rounds <- character()
  for (node in tree$nodes) {
    tags <- as.character(unlist(node$model$tags))
    if (!"scm" %in% tags) {
      next
    }
    rounds <- c(rounds, setdiff(tags, marker_tags))
  }
  unique(rounds)
}

# Whether any backward round kept a candidate, i.e. dropped a covariate.
scm_lineage_dropped_any <- function(tree) {
  for (node in tree$nodes) {
    tags <- as.character(unlist(node$model$tags))
    if ("selected" %in% tags && any(grepl("^backward_round", tags))) {
      return(TRUE)
    }
  }
  FALSE
}

# "forward_round2" -> "Forward round 2"; the stages get their own names.
scm_round_label <- function(round) {
  if (round %in% names(scm_stage_labels)) {
    return(scm_stage_labels[[round]])
  }
  m <- scm_round_parts(round)
  if (length(m) == 3) {
    return(sprintf("%s round %s", tools::toTitleCase(m[2]), m[3]))
  }
  round
}

# "forward_round2" -> "Forward": the rounds of a direction share a section.
scm_direction_label <- function(round) {
  if (round %in% names(scm_stage_labels)) {
    return(scm_stage_labels[[round]])
  }
  m <- scm_round_parts(round)
  if (length(m) == 3) {
    return(tools::toTitleCase(m[2]))
  }
  round
}

scm_stage_labels <- c(
  base = "Base model",
  full = "Full model",
  reference = "Reference model",
  forward_final = "Working full model",
  final = "Reduced covariate model"
)

# c(round, direction, number) for a round tag, or character() for a stage.
scm_round_parts <- function(round) {
  regmatches(round, regexec("^(forward|backward)_round([0-9]+)$", round))[[1]]
}
