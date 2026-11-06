#' Pre-configured SummarySpec for a stepwise covariate modeling process
#'
#' `scm_summary_spec()` returns a [SummarySpec] for the run summary table of
#' an SCM process: one section per round, in the order the process ran them,
#' from the round tags `hyperion::get_scm_lineage()` puts on each fit. Every
#' fit in that lineage is based on the model its round tested against, so
#' each candidate's dOFV is the round's own.
#'
#' Each section's label names the round, the model the round tested against
#' and the covariates the process had retained going into it, from the
#' process record, e.g. `"Forward round 2 — reference: run001_wt_cl;
#' retained: WT_CL"`; a part that does not apply is left out, so the
#' reference section is just `"Reference"`. The forward and final models'
#' sections list what they keep.
#'
#' The columns are OFV, dOFV and Action. The reference, forward and final
#' models show their OFV and no dOFV; each candidate shows its dOFV and no
#' OFV, and the candidate its round selected shows "Selected" in Action. The
#' table has no footnotes. The spec sets `pvalue_direction = "both"`, so
#' selecting the `pvalue` column with [set_spec_columns()] tests the
#' backward steps too. The returned spec can be customized further with the
#' `set_spec_*()` modifiers.
#'
#' The section rules end with a catch-all, so rules appended with
#' [set_spec_sections()] never apply; pass `overwrite = TRUE` to replace them.
#'
#' @param scm The SCM process: a `hyperion_nonmem_scm` object or its config
#'   (`<stem>scm.toml`). The lineage alone is not enough, since the retained
#'   covariates come from the process record.
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
#'   set_spec_columns("ofv", "dofv", "pvalue", "action") |>
#'   set_spec_pvalue(scientific = TRUE)
#' }
scm_summary_spec <- function(scm, title = NULL, other_section_label = "Other") {
  if (!is.null(title) && !rlang::is_string(title)) {
    rlang::abort("`title` must be a single character string or NULL.")
  }
  if (!rlang::is_string(other_section_label)) {
    rlang::abort("`other_section_label` must be a single character string.")
  }
  if (inherits(scm, "hyperion_nonmem_tree")) {
    rlang::abort(
      "`scm` must be the SCM process or its config, not its lineage: the retained covariates come from the process record."
    )
  }
  if (rlang::is_string(scm)) {
    scm <- hyperion::read_scm(scm)
  }
  tree <- hyperion::get_scm_lineage(scm)
  retained <- scm_retained_by_round(summary(scm))

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
      scm_section_label(
        round,
        reference = scm_round_reference(tree, round),
        retained = if (round %in% names(retained)) retained[[round]] else NA_character_
      ),
      env = baseenv()
    )
  })
  rules <- c(rules, list(rlang::new_formula(TRUE, other_section_label, env = baseenv())))

  spec <- rlang::inject(set_spec_sections(SummarySpec(), !!!rules))
  spec <- set_spec_pvalue(spec, direction = "both")
  # The stage models show their OFV; the candidates show their round's dOFV
  # and whether the round kept them.
  spec <- set_spec_mutate(
    spec,
    ofv = if (any(!!scm_stage_tags %in% tags)) ofv else NA_real_,
    dofv = if (any(!!scm_stage_tags %in% tags)) NA_real_ else dofv,
    action = if ("selected" %in% tags && !any(!!scm_stage_tags %in% tags)) {
      "Selected"
    } else {
      NA_character_
    }
  )
  spec <- set_spec_columns(spec, "ofv", "dofv", "action")
  spec <- set_spec_footnotes(spec, NULL)
  set_spec_title(spec, title %||% "")
}

# The tags of the fits that are not round candidates.
scm_stage_tags <- c("reference", "forward_final", "final")

# What the process had retained going into each round, keyed by the round's
# tag, from its record: a comma-separated list, or NA when nothing was.
# The forward and final models get what they keep.
scm_retained_by_round <- function(record) {
  text <- function(x) {
    x <- as.character(unlist(x))
    if (length(x) == 0) NA_character_ else paste(x, collapse = ", ")
  }
  out <- character()
  for (r in record$rounds %||% list()) {
    if (!is.null(r$round)) {
      out[[r$round]] <- text(r$retained_before)
    }
  }
  if (!is.null(record$forward_final$model)) {
    out[["forward_final"]] <- text(record$forward_final$retained)
  }
  if (!is.null(record$final_model)) {
    out[["final"]] <- text(record$retained)
  }
  out
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

# The model a round's fits are based on, as shown in the table: its stem, or
# the stems joined when the round's fits disagree. NA for the reference.
scm_round_reference <- function(tree, round) {
  refs <- character()
  for (node in tree$nodes) {
    tags <- as.character(unlist(node$model$tags))
    if (round %in% tags) {
      parents <- as.character(unlist(node$model$based_on))
      refs <- c(refs, tools::file_path_sans_ext(basename(parents)))
    }
  }
  refs <- unique(refs)
  if (length(refs) == 0) NA_character_ else paste(refs, collapse = ", ")
}

# The section label: the round's name, then the parts that apply, e.g.
# "Forward round 2 \u2014 reference: run001_wt_cl; retained: WT_CL".
scm_section_label <- function(round, reference, retained) {
  parts <- c(
    if (!is.na(reference)) paste0("reference: ", reference),
    if (!is.na(retained)) paste0("retained: ", retained)
  )
  label <- scm_round_label(round)
  if (length(parts) == 0) {
    return(label)
  }
  paste0(label, " \u2014 ", paste(parts, collapse = "; "))
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
