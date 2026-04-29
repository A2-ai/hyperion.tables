# ==============================================================================
# Pipeline-state attribute helpers
#
# Internal helpers wrapping raw `attr()` reads/writes for state that flows
# through pipelines as data-frame attributes. Centralizing them keeps the
# carrier pattern grep-able and gives one place to add type checks. The
# underlying storage is still `attr()` — these are not class wrappers.
# ==============================================================================

# ---- Parameter pipeline (apply_table_spec → add_summary_info → render) ----

#' @noRd
attach_table_spec <- function(df, spec) {
  if (!is.null(spec) && !S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a TableSpec or NULL.")
  }
  attr(df, "table_spec") <- spec
  df
}

#' @noRd
attach_model_summary <- function(df, summary) {
  attr(df, "model_summary") <- summary
  df
}

#' @noRd
get_attached_summary <- function(df) {
  attr(df, "model_summary")
}

# ---- Summary pipeline (apply_summary_spec → make_summary_table) ----

#' @noRd
attach_summary_spec <- function(df, spec) {
  if (!is.null(spec) && !S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a SummarySpec or NULL.")
  }
  attr(df, "summary_spec") <- spec
  df
}

#' @noRd
attach_summary_time_unit <- function(df, unit) {
  attr(df, "summary_time_unit") <- unit
  df
}

#' @noRd
get_summary_time_unit <- function(df) {
  attr(df, "summary_time_unit")
}

#' @noRd
attach_needs_dofv <- function(df, value) {
  attr(df, ".needs_dofv") <- value
  df
}

#' @noRd
take_needs_dofv <- function(df) {
  # Read-and-clear: returns a list(value, df) so the caller can use the
  # value without leaving the marker on the df for the next stage.
  value <- isTRUE(attr(df, ".needs_dofv"))
  attr(df, ".needs_dofv") <- NULL
  list(value = value, df = df)
}

# ---- Comparison object metadata ----
# A comparison data frame carries 5 pieces of metadata as attrs. See
# `capture_comparison_attrs()` / `restore_comparison_attrs()` for bulk
# save/restore around dplyr ops that strip attrs.

#' @noRd
attach_comparison_summaries <- function(df, summaries) {
  attr(df, "summaries") <- summaries
  df
}

#' @noRd
get_comparison_summaries <- function(df) {
  attr(df, "summaries")
}

#' @noRd
attach_comparison_labels <- function(df, labels) {
  attr(df, "labels") <- labels
  df
}

#' @noRd
get_comparison_labels <- function(df) {
  attr(df, "labels")
}

#' @noRd
attach_comparison_table_spec <- function(df, spec) {
  attr(df, "table_spec") <- spec
  df
}

#' @noRd
get_comparison_table_spec <- function(df) {
  attr(df, "table_spec")
}

#' @noRd
attach_comparison_pct_change_refs <- function(df, refs) {
  attr(df, "pct_change_refs") <- refs
  df
}

#' @noRd
get_comparison_pct_change_refs <- function(df) {
  attr(df, "pct_change_refs")
}

#' @noRd
attach_comparison_lineage <- function(df, lineage) {
  attr(df, "lineage") <- lineage
  df
}

#' @noRd
get_comparison_lineage <- function(df) {
  attr(df, "lineage")
}
