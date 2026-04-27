# ==============================================================================
# Parameter lookup TOML
# ==============================================================================

#' Read a parameter lookup TOML file
#'
#' Returns a named list keyed by parameter name. Each value is the entry's
#' field list (e.g., `list(section = "...", display = "...", ...)`). Used
#' internally by [apply_table_spec()] when a TOML path was set on the
#' spec via `set_spec_sections(file = ...)`.
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
#' @param lookup Result of `read_lookup_toml()`.
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

#' Convert a lookup TOML to the assignments shape (named list by section)
#' @noRd
toml_lookup_to_assignments <- function(lookup) {
  flat <- lookup_section_map(lookup)
  if (length(flat) == 0L) {
    return(list())
  }
  split(unname(names(flat)), unname(flat))
}

#' Merge a `new` assignments list into a `current` one.
#'
#' Both inputs are named lists keyed by section label, where each value is
#' a character vector of items. When the same item appears in both with
#' different section labels, `new` wins; if `warn_on_conflict` is TRUE a
#' warning is emitted. Items with the same label in both are deduplicated.
#'
#' @noRd
merge_assignments <- function(
  current,
  new,
  source = c("inline", "file"),
  warn_on_conflict = FALSE
) {
  source <- match.arg(source)
  if (!is.list(new)) {
    rlang::abort(c(
      "`parameters` must be a named list keyed by section label.",
      "x" = sprintf("You passed a %s vector.", typeof(new)),
      "i" = "Example: `list(\"Section A\" = c(\"p1\", \"p2\"))`."
    ))
  }
  if (length(new) == 0L) {
    return(current)
  }
  if (is.null(names(new)) || any(!nzchar(names(new)))) {
    rlang::abort(
      "`parameters` must be a *named* list (each name is a section label)."
    )
  }
  bad_values <- !vapply(
    new,
    function(v) {
      is.character(v) &&
        length(v) > 0L &&
        !any(is.na(v)) &&
        all(nzchar(v))
    },
    logical(1)
  )
  if (any(bad_values)) {
    rlang::abort(
      "Each value in `parameters` must be a non-empty character vector of names (no NAs or empty strings)."
    )
  }

  flatten <- function(x) {
    if (length(x) == 0L) {
      return(stats::setNames(character(0), character(0)))
    }
    items <- unlist(x, use.names = FALSE)
    labels <- rep(names(x), lengths(x))
    stats::setNames(labels, items)
  }
  cur_flat <- flatten(current)
  new_flat <- flatten(new)
  if (anyDuplicated(names(new_flat)) > 0L) {
    dups <- unique(names(new_flat)[duplicated(names(new_flat))])
    rlang::abort(paste0(
      "`parameters` lists the same item under multiple sections: ",
      paste(shQuote(dups), collapse = ", "),
      "."
    ))
  }

  conflicts <- intersect(names(cur_flat), names(new_flat))
  if (length(conflicts) > 0L) {
    differing <- conflicts[cur_flat[conflicts] != new_flat[conflicts]]
    if (warn_on_conflict && length(differing) > 0L) {
      rlang::warn(paste0(
        "Per-parameter section conflict between `file` and `parameters` for: ",
        paste(shQuote(differing), collapse = ", "),
        ". Inline `parameters` value(s) win."
      ))
    }
  }

  merged <- cur_flat
  merged[names(new_flat)] <- new_flat

  if (length(merged) == 0L) {
    return(list())
  }
  split(unname(names(merged)), unname(merged))
}
