# ==============================================================================
# Parameter lookup TOML
# ==============================================================================

#' Read a parameter lookup TOML file
#'
#' Returns a named list keyed by parameter name. Each value is the entry's
#' field list (e.g., `list(section = "...", display = "...", ...)`). Used
#' internally by [apply_table_spec()] when a `lookup_path` is set on the
#' spec via [set_spec_lookup()].
#'
#' @param path Path to a TOML file.
#' @return Named list of per-parameter entries.
#' @noRd
read_lookup_toml <- function(path) {
  check_suggested("tomledit", reason = "to read parameter lookup TOML files.")
  if (!file.exists(path)) {
    rlang::abort(paste0("Lookup TOML not found: ", path))
  }
  tomledit::from_toml(tomledit::read_toml(path))
}

#' Extract a per-parameter section map from a lookup list
#'
#' Returns a named character vector: parameter name → section. Entries
#' without a `section` field (or with non-character values) are dropped.
#'
#' @param lookup Result of [read_lookup_toml()].
#' @return Named character vector (possibly empty).
#' @noRd
lookup_section_map <- function(lookup) {
  has_section <- vapply(
    lookup,
    function(entry) {
      is.list(entry) &&
        "section" %in% names(entry) &&
        is.character(entry$section) &&
        length(entry$section) == 1L &&
        !is.na(entry$section)
    },
    logical(1)
  )
  if (!any(has_section)) {
    return(stats::setNames(character(0), character(0)))
  }
  vapply(lookup[has_section], function(entry) entry$section, character(1))
}
