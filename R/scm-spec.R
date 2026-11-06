#' Pre-configured SummarySpec for a stepwise covariate modeling process
#'
#' `scm_summary_spec()` returns a [SummarySpec] for the run summary table of
#' an SCM process: one section per round, in the order the process ran them,
#' from the round tags `hyperion::get_scm_lineage()` puts on each fit. Every
#' fit in that lineage is based on the model its round tested against, so
#' the table's dOFV and p-value are the round's own. The returned spec can be
#' customized further with the `set_spec_*()` modifiers.
#'
#' The section rules end with a catch-all, so rules appended with
#' [set_spec_sections()] never apply; pass `overwrite = TRUE` to replace them.
#'
#' @param scm The SCM process: a `hyperion_nonmem_scm` object, its config
#'   (`<stem>scm.toml`), or the `hyperion_nonmem_tree` that
#'   `hyperion::get_scm_lineage()` built from it.
#' @param title Table title. `NULL` (the default) gives a table with no
#'   title; pass a string to set one.
#' @param other_section_label Section label for a model matching no round.
#'
#' @return A [SummarySpec] object.
#' @seealso [SummarySpec], [apply_summary_spec()], [make_summary_table()].
#' @export
#' @examples \dontrun{
#' scm <- hyperion::read_scm("model/nonmem/scm/run001scm.toml")
#'
#' scm |>
#'   hyperion::get_scm_lineage() |>
#'   apply_summary_spec(scm_summary_spec(scm)) |>
#'   make_summary_table()
#'
#' # Customize further with modifiers
#' spec <- scm_summary_spec(scm, title = "Covariate search") |>
#'   drop_spec_columns("condition_number") |>
#'   set_spec_pvalue(scientific = TRUE)
#' }
scm_summary_spec <- function(scm, title = NULL, other_section_label = "Other") {
  if (!is.null(title) && !rlang::is_string(title)) {
    rlang::abort("`title` must be a single character string or NULL.")
  }
  if (!rlang::is_string(other_section_label)) {
    rlang::abort("`other_section_label` must be a single character string.")
  }
  tree <- if (inherits(scm, "hyperion_nonmem_tree")) {
    scm
  } else {
    hyperion::get_scm_lineage(scm)
  }

  rounds <- scm_lineage_rounds(tree)
  if (length(rounds) == 0) {
    rlang::abort(
      "`scm` has no SCM fits: no model in its lineage is tagged with a round."
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
  set_spec_title(spec, title %||% "")
}

# The rounds in the lineage, in the order their fits appear: each fit's tag
# other than "scm", "selected" and "retry".
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
    reference = "Reference",
    forward_final = "Forward model",
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
