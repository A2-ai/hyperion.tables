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
#' round's own. The columns are Reference, Description (the fit's action,
#' such as "SCM forward_round1: add WT_CL (attempt 1)"), OFV, dOFV and
#' Action, which shows "Selected" for the candidate its round kept, from the
#' `"selected"` tag. The table has no footnotes, and `pvalue_direction` is
#' `"both"`, so selecting the `pvalue` column with [set_spec_columns()] tests
#' the backward steps too. Customize further with the `set_spec_*()`
#' modifiers.
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
#' # Customize further with modifiers
#' spec <- scm_summary_spec(scm) |>
#'   set_spec_columns("based_on", "ofv", "dofv", "pvalue", "action") |>
#'   set_spec_pvalue(scientific = TRUE)
#' }
scm_summary_spec <- function(lineage, title = NULL, other_section_label = "Other") {
  if (!is.null(title) && !rlang::is_string(title)) {
    rlang::abort("`title` must be a single character string or NULL.")
  }
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
  # base R for `%in%`, nothing else.
  rules <- lapply(rounds, function(round) {
    rlang::new_formula(
      rlang::call2("%in%", round, rlang::sym("tags")),
      scm_round_label(round),
      env = baseenv()
    )
  })
  rules <- c(rules, list(rlang::new_formula(TRUE, other_section_label, env = baseenv())))

  spec <- rlang::inject(set_spec_sections(SummarySpec(), !!!rules))
  spec <- set_spec_tag_filter(spec, include = "scm")
  spec <- set_spec_pvalue(spec, direction = "both")
  # The candidate a round kept; the reference and final fits are not candidates.
  spec <- set_spec_mutate(
    spec,
    action = if ("selected" %in% tags && !any(!!scm_stage_tags %in% tags)) {
      "Selected"
    } else {
      NA_character_
    }
  )
  spec <- set_spec_columns(spec, "based_on", "description", "ofv", "dofv", "action")
  spec <- set_spec_footnotes(spec, NULL)
  set_spec_title(spec, title %||% "")
}

# The round tags of the fits that are not round candidates: the reference
# fit (`base` or `full` from pharos, `reference` from hyperion's record
# lineage), the forward model and the final model.
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

# "forward_round2" -> "Forward round 2"; the stages get their own names.
scm_round_label <- function(round) {
  stages <- c(
    base = "Base model",
    full = "Full model",
    reference = "Reference model",
    forward_final = "Forward final model",
    final = "Final model"
  )
  if (round %in% names(stages)) {
    return(stages[[round]])
  }
  m <- regmatches(round, regexec("^(forward|backward)_round([0-9]+)$", round))[[1]]
  if (length(m) == 3) {
    return(sprintf("%s round %s", tools::toTitleCase(m[2]), m[3]))
  }
  round
}
