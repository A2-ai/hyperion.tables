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
#' round's own; the forward final and final models are based on the
#' reference fit, so theirs is against it. The columns are Reference,
#' Description, OFV and dOFV, and the candidate each round kept, tagged
#' `"selected"`, is in bold. The table has no footnotes, and
#' `pvalue_direction` is `"both"`, so selecting the `pvalue` column with
#' [set_spec_columns()] tests the backward steps too. Customize further with
#' the `set_spec_*()` modifiers.
#'
#' pharos ends every SCM fit's description with the covariates its model
#' has, e.g. `"SCM forward_round2: add CRCL_CL (attempt 1); covariates:
#' WT_CL, CRCL_CL"`, as does `hyperion::get_scm_lineage()`. `description`
#' picks which part the Description column shows: `"step"` (the default),
#' the fit's action, or `"covariates"`, the covariates the model has, so
#' a fourth-round candidate reads "WT_CL, CRCL_CL, WT_V" rather than
#' "add WT_V". A description without that part shows as it is.
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
#'   `"step"` (the default), its action, or `"covariates"`, the covariates
#'   its model has.
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
#' # Show each model's covariates instead of the step
#' scm_summary_spec(scm, description = "covariates")
#'
#' # Customize further with modifiers
#' spec <- scm_summary_spec(scm) |>
#'   set_spec_columns("based_on", "ofv", "dofv", "pvalue") |>
#'   set_spec_pvalue(scientific = TRUE)
#' }
scm_summary_spec <- function(
  lineage,
  title = NULL,
  description = c("step", "covariates"),
  other_section_label = "Other"
) {
  if (!is.null(title) && !rlang::is_string(title)) {
    rlang::abort("`title` must be a single character string or NULL.")
  }
  description <- match.arg(description)
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
  # The candidate a round kept is in bold; the reference and final fits are
  # not candidates.
  spec <- set_spec_bold_rows(
    spec,
    "selected" %in% tags && !any(!!scm_stage_tags %in% tags)
  )
  spec <- set_spec_description_transform(
    spec,
    switch(
      description,
      step = scm_description_step,
      covariates = scm_description_covariates
    )
  )
  spec <- set_spec_columns(spec, "based_on", "description", "ofv", "dofv")
  spec <- set_spec_footnotes(spec, NULL)
  set_spec_title(spec, title %||% "")
}

# pharos and hyperion end an SCM fit's description with the covariates its
# model has: "add CRCL_CL (attempt 1); covariates: WT_CL, CRCL_CL". The step
# is what comes before; a description without the part is left alone.
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
