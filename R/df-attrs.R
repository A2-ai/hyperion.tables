# ==============================================================================
# Pipeline-state attribute helpers
#
# Spec attachers validate the spec's S7 class before storing it. The
# `take_needs_dofv` and `*_comparison_meta*` helpers encapsulate non-trivial
# read/write patterns layered on top of raw `attr()` storage.
# ==============================================================================

# ---- Spec attachment (used by parameter and summary pipelines) ----

#' @noRd
attach_hyperion_spec <- function(df, spec, expected) {
  if (!is.null(spec) && !S7::S7_inherits(spec, expected)) {
    rlang::abort(sprintf(
      "`spec` must be a <%s> or NULL.",
      attr(expected, "name")
    ))
  }
  attr(df, "hyperion_spec") <- spec
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
# A comparison data frame carries five pieces of metadata bundled into a
# single `hyperion_meta` list attr. Keeping them in one attr means dplyr
# verbs only need to round-trip one attr to preserve comparison state.

#' @noRd
get_comparison_meta <- function(df) {
  attr(df, "hyperion_meta") %||% list()
}

#' @noRd
set_comparison_meta_field <- function(df, name, value) {
  meta <- get_comparison_meta(df)
  meta[[name]] <- value
  attr(df, "hyperion_meta") <- meta
  df
}
