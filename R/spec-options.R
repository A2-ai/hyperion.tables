# ==============================================================================
# CI options
# ==============================================================================

#' Confidence interval render options
#'
#' Controls CI merge behavior and missing-value display.
#'
#' @param level Confidence interval level, between 0 and 1.
#' @param merge Logical. If TRUE, merge CI low/high into a single column when present.
#' @param pattern sprintf pattern used when merging CI values. Must include exactly two `%s`.
#' @param missing_text Text to display when CI values are missing in rows where CI is expected.
#'
#' @section Properties:
#' The following properties are available on a `CIOptions` object:
#' \itemize{
#'   \item `level` - Confidence interval level (0-1).
#'   \item `merge` - Whether to merge CI bounds into a single column.
#'   \item `pattern` - sprintf pattern for merged CI display (two `%s`).
#'   \item `missing_text` - Text to show for missing CI values.
#' }
#'
#' @export
CIOptions <- S7::new_class(
  "CIOptions",
  properties = list(
    level = S7::new_property(
      class = S7::class_numeric,
      default = 0.95
    ),
    merge = S7::new_property(
      class = S7::class_logical,
      default = TRUE
    ),
    pattern = S7::new_property(
      class = S7::class_character | NULL,
      default = "[%s, %s]"
    ),
    missing_text = S7::new_property(
      class = S7::class_character,
      default = "-"
    )
  ),
  validator = function(self) {
    if (self@level <= 0 || self@level >= 1) {
      return(sprintf(
        "@level must be between 0 and 1 (exclusive). Got: %s",
        self@level
      ))
    }
    if (length(self@merge) != 1 || is.na(self@merge)) {
      return(sprintf(
        "@merge must be TRUE or FALSE. Got: %s",
        self@merge
      ))
    }

    if (length(self@missing_text) != 1 || is.na(self@missing_text)) {
      return(sprintf(
        "@missing_text must be a single character string. Got: %s",
        self@missing_text
      ))
    }

    if (isTRUE(self@merge)) {
      if (length(self@pattern) != 1 || is.na(self@pattern)) {
        return(sprintf(
          "@pattern must be a single character string. Got: %s",
          self@pattern
        ))
      }
      if (
        length(regmatches(self@pattern, gregexpr("%s", self@pattern))[[1]]) != 2
      ) {
        return("@pattern must contain exactly two \"%s\" placeholders.")
      }
    } else {
      if (
        !is.null(self@pattern) && !is.na(self@pattern) && nzchar(self@pattern)
      ) {
        return("@pattern must be NULL or empty when @merge is FALSE.")
      }
    }
  },
  constructor = function(
    level = 0.95,
    merge = TRUE,
    pattern = "[%s, %s]",
    missing_text = "-"
  ) {
    if (!isTRUE(merge)) {
      pattern <- NULL
    }
    S7::new_object(
      S7::S7_object(),
      level = level,
      merge = merge,
      pattern = pattern,
      missing_text = missing_text
    )
  }
)

# ==============================================================================
# Parameter Name Options
# ==============================================================================

#' Parameter name display options
#'
#' Controls how parameter names are displayed in tables.
#'
#' @param source Which name field to use: "name" (default), "display", or "nonmem"
#'
#' @section Properties:
#' The following properties are available on a `ParameterNameOptions` object:
#' \itemize{
#'   \item `source` - Which name field to use ("name", "display", or "nonmem").
#' }
#'
#' @export
ParameterNameOptions <- S7::new_class(
  "ParameterNameOptions",
  properties = list(
    source = S7::new_property(
      class = S7::class_character,
      default = "name"
    )
  ),
  validator = function(self) {
    valid_sources <- c("name", "display", "nonmem")
    if (!self@source %in% valid_sources) {
      return(sprintf(
        "@source must be 'name', 'display', or 'nonmem'. Got: '%s'",
        self@source
      ))
    }
  }
)

# ==============================================================================
# Section Options
# ==============================================================================

#' Section options for a spec
#'
#' Holds rule formulas, per-item assignments, display order, and the
#' filter (keep / exclude) for a `TableSpec` or `SummarySpec`.
#'
#' @param rules List of formulas created with [section_rules()].
#' @param assignments Named list keyed by section label; each value is a
#'   character vector of items belonging to that section.
#' @param order Character vector of section labels in display order, or NULL.
#' @param filter A list specifying section filtering: `list()` (no filter),
#'   `list(keep = c(...))`, or `list(exclude = c(...))`.
#' @param inline_items Internal: items that originated from an inline
#'   `parameters =` override (used to scope unmatched-item warnings).
#'   Not intended for direct use.
#'
#' @section Properties:
#' \itemize{
#'   \item `rules` - List of formulas created with [section_rules()].
#'   \item `assignments` - Named list keyed by section label; each value is
#'     a character vector of items belonging to that section.
#'   \item `order` - Character vector of section labels in display order, or NULL.
#'   \item `filter` - List with one named entry (`keep` or `exclude`) holding
#'     section labels, or empty list for no filter.
#' }
#'
#' @export
SectionOptions <- S7::new_class(
  "SectionOptions",
  package = "hyperion.tables",
  properties = list(
    rules = S7::new_property(
      class = S7::class_list,
      default = list()
    ),
    assignments = S7::new_property(
      class = S7::class_list | S7::class_character,
      default = list()
    ),
    inline_items = S7::new_property(
      class = S7::class_character,
      default = character(0)
    ),
    order = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL,
      setter = function(self, value) {
        if (!is.null(value) && length(value) == 0L) {
          value <- NULL
        }
        S7::prop(self, "order") <- value
        self
      }
    ),
    filter = S7::new_property(
      class = S7::class_list,
      default = list()
    )
  ),
  validator = function(self) {
    if (
      length(self@rules) > 0 &&
        !all(vapply(self@rules, rlang::is_formula, logical(1)))
    ) {
      return(
        "@rules must be formulas; pass formulas via `set_spec_sections(...)` or build with `section_rules()`."
      )
    }

    if (!is.list(self@assignments)) {
      return("@assignments must be a named list keyed by section label.")
    }

    if (length(self@assignments) > 0) {
      nm <- names(self@assignments)
      if (is.null(nm) || any(!nzchar(nm)) || any(is.na(nm))) {
        return("@assignments must be a named list keyed by section label.")
      }
      if (anyDuplicated(nm) > 0L) {
        return("@assignments has duplicate section labels.")
      }
      ok_vals <- vapply(
        self@assignments,
        function(v) {
          is.character(v) &&
            length(v) > 0L &&
            !any(is.na(v)) &&
            all(nzchar(v))
        },
        logical(1)
      )
      if (!all(ok_vals)) {
        return(
          "@assignments values must be non-empty, non-NA character vectors."
        )
      }
      flat <- unlist(self@assignments, use.names = FALSE)
      if (anyDuplicated(flat) > 0L) {
        dups <- unique(flat[duplicated(flat)])
        return(paste0(
          "@assignments lists the same item under multiple sections: ",
          paste(shQuote(dups), collapse = ", "),
          "."
        ))
      }
    }

    if (
      length(self@order) > 0L &&
        (any(!nzchar(self@order)) || any(is.na(self@order)))
    ) {
      return("@order labels must be non-empty and non-NA.")
    }

    if (length(self@filter) > 0L) {
      if (length(self@filter) > 1L) {
        return(
          "@filter must have exactly one entry: `keep` or `exclude`."
        )
      }
      filter_mode <- names(self@filter)
      if (is.null(filter_mode) || !filter_mode %in% c("keep", "exclude")) {
        return(
          "@filter must be a list named `keep` or `exclude`."
        )
      }
      filter_labels <- self@filter[[1]]
      if (
        !is.character(filter_labels) ||
          length(filter_labels) == 0L ||
          any(!is.na(filter_labels) & !nzchar(filter_labels))
      ) {
        return(sprintf(
          "@filter$%s labels must be non-empty (NA allowed).",
          filter_mode
        ))
      }
    }

    if (length(self@order) > 0L || length(self@filter) > 0L) {
      known_labels <- character()
      any_dynamic <- FALSE
      for (rule in self@rules) {
        expr <- if (rlang::is_quosure(rule)) {
          rlang::quo_get_expr(rule)
        } else {
          rule
        }
        if (!rlang::is_formula(expr)) {
          any_dynamic <- TRUE
          next
        }
        rhs <- rlang::f_rhs(expr)
        if (is.character(rhs) && length(rhs) == 1L) {
          known_labels <- c(known_labels, rhs)
          next
        }
        rule_env <- if (rlang::is_quosure(rule)) {
          rlang::quo_get_env(rule)
        } else {
          rlang::caller_env()
        }
        resolved <- tryCatch(
          rlang::eval_tidy(rhs, env = rule_env),
          error = function(e) NULL,
          warning = function(w) NULL
        )
        if (is.character(resolved) && length(resolved) == 1L) {
          known_labels <- c(known_labels, resolved)
        } else {
          any_dynamic <- TRUE
        }
      }
      known <- unique(c(known_labels, names(self@assignments)))
      unknown_for <- function(labels) {
        labels <- labels[!is.na(labels)]
        setdiff(labels, known)
      }
      unknown_msg <- function(unknown, slot_name) {
        paste0(
          "@",
          slot_name,
          " references unknown section ",
          if (length(unknown) == 1L) "label" else "labels",
          ": ",
          paste(shQuote(unknown), collapse = ", "),
          ". Known sections: ",
          paste(shQuote(known), collapse = ", "),
          "."
        )
      }
      bad_order <- unknown_for(self@order)
      bad_filter <- if (length(self@filter) > 0L) {
        unknown_for(self@filter[[1]])
      } else {
        character()
      }
      if (length(bad_order) > 0L || length(bad_filter) > 0L) {
        msgs <- c(
          if (length(bad_order) > 0L) unknown_msg(bad_order, "order"),
          if (length(bad_filter) > 0L) {
            unknown_msg(bad_filter, paste0("filter$", names(self@filter)))
          }
        )
        if (any_dynamic) {
          rlang::warn(paste(msgs, collapse = "\n"))
        } else {
          return(msgs[[1]])
        }
      }
    }

    NULL
  }
)

#' Validate the shape of a section-assignments list
#'
#' Mirrors the SectionOptions validator's checks. Used by
#' `merge_section_assignments()` to decide whether to merge or pass the
#' value through unchanged so the validator produces the friendly error.
#'
#' @noRd
is_valid_assignments_input <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (length(x) == 0L) {
    return(TRUE)
  }
  nm <- names(x)
  if (is.null(nm) || any(!nzchar(nm)) || any(is.na(nm))) {
    return(FALSE)
  }
  ok_vals <- vapply(
    x,
    function(v) {
      is.character(v) &&
        length(v) > 0L &&
        !any(is.na(v)) &&
        all(nzchar(v))
    },
    logical(1)
  )
  if (!all(ok_vals)) {
    return(FALSE)
  }
  anyDuplicated(unlist(x, use.names = FALSE)) == 0L
}

#' Merge a new set of section assignments into the current set
#'
#' `new` takes precedence on conflict. When `warn_on_conflict` is TRUE,
#' warns about items whose section label differs between current and new.
#' When `new` is malformed, returns it as-is so prop assignment triggers
#' the SectionOptions validator's friendly error.
#'
#' @noRd
merge_section_assignments <- function(current, new, warn_on_conflict = FALSE) {
  if (!is_valid_assignments_input(new)) {
    return(new)
  }
  if (length(new) == 0L) {
    return(list())
  }
  if (!is.list(current)) {
    current <- list()
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
  if (warn_on_conflict) {
    conflicts <- intersect(names(cur_flat), names(new_flat))
    differing <- conflicts[cur_flat[conflicts] != new_flat[conflicts]]
    if (length(differing) > 0L) {
      rlang::warn(paste0(
        "Per-parameter section conflict between `file` and `parameters` for: ",
        paste(shQuote(differing), collapse = ", "),
        ". Inline `parameters` value(s) win."
      ))
    }
  }
  merged <- cur_flat
  merged[names(new_flat)] <- new_flat
  split(unname(names(merged)), unname(merged))
}
