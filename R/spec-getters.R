# ==============================================================================
# Spec Getter Functions
# ==============================================================================
# S7 generics and methods for reading properties from TableSpec and SummarySpec.

# ==============================================================================
# Common Getters (Both Specs)
# ==============================================================================

#' Get columns from a spec
#'
#' `get_spec_columns()` returns the current columns list from the spec. The
#' result is resolved as
#' `(columns %||% default_columns) ∪ add_columns − drop_columns`.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Character vector of column names.
#' @seealso [set_spec_columns()], [add_spec_columns()], [drop_spec_columns()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_columns(spec)
get_spec_columns <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  cols <- spec@columns %||% spec@default_columns
  if (!is.null(spec@add_columns)) {
    cols <- unique(c(cols, spec@add_columns))
  }
  cols
}

#' Get title from a spec
#'
#' `get_spec_title()` returns the table header title.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Character string.
#' @seealso [set_spec_title()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_title(spec)
get_spec_title <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@title
}

#' Get significant figures from a spec
#'
#' `get_spec_sigfig()` returns the number of significant figures used for
#' numeric formatting.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Numeric value.
#' @seealso [set_spec_sigfig()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_sigfig(spec)
get_spec_sigfig <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@n_sigfig
}

#' Get OFV decimal places from a spec
#'
#' `get_spec_ofv_decimals()` returns the number of decimal places for OFV and
#' dOFV values.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Numeric value (or NA).
#' @seealso [set_spec_ofv_decimals()].
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_ofv_decimals(spec)
get_spec_ofv_decimals <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@n_decimals_ofv
}

#' Get hide_empty_columns from a spec
#'
#' `get_spec_hide_empty()` returns whether empty columns are automatically
#' hidden.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Logical scalar.
#' @seealso [set_spec_hide_empty()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_hide_empty(spec)
get_spec_hide_empty <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@hide_empty_columns
}

#' Get p-value formatting options from a spec
#'
#' `get_spec_pvalue()` returns the p-value formatting options as a named list.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return A named list with elements `threshold` and `scientific`.
#' @seealso [set_spec_pvalue()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_pvalue(spec)
get_spec_pvalue <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  list(
    threshold = spec@pvalue_threshold,
    scientific = spec@pvalue_scientific
  )
}

#' Get footnote order from a spec
#'
#' `get_spec_footnotes()` returns the footnote order configuration.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Character vector of footnote sections in order, or NULL when
#'   footnotes are disabled.
#' @seealso [set_spec_footnotes()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_footnotes(spec)
get_spec_footnotes <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@footnote_order
}

# ==============================================================================
# TableSpec-Only Getters
# ==============================================================================

#' Get parameter name options from a TableSpec
#'
#' `get_spec_parameter_names()` returns the ParameterNameOptions object
#' controlling how parameter names are displayed. Operates on `TableSpec` only.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return ParameterNameOptions object.
#' @seealso [set_spec_parameter_names()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_parameter_names(spec)
get_spec_parameter_names <- function(spec, ...) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
  spec@parameter_names
}

#' Get CI options from a TableSpec
#'
#' `get_spec_ci()` returns the CIOptions object. Operates on `TableSpec` only.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return CIOptions object.
#' @seealso [set_spec_ci()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_ci(spec)
get_spec_ci <- function(spec, ...) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
  spec@ci
}

#' Get section configuration from a spec
#'
#' `get_spec_sections()` returns the spec's [SectionOptions] object (rules,
#' assignments, order, filter).
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return A [SectionOptions] object.
#' @seealso [set_spec_sections()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_sections(spec)
get_spec_sections <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@sections
}

#' Get section filter from a spec
#'
#' Returns the current section filter as a list with one of two shapes:
#' `list(exclude = c(...))`, `list(keep = c(...))`, or empty `list()` when no
#' filter is set.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @return A named list (possibly empty).
#' @seealso [set_spec_sections()], [get_spec_sections()].
#' @export
get_spec_section_filter <- function(spec) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@sections@filter
}

#' Get per-parameter section assignments from a TableSpec
#'
#' Returns the assignments as a named list keyed by section label, where
#' each value is a character vector of parameter names. Mirrors the shape
#' passed to `set_spec_sections(parameters = ...)`. Operates on `TableSpec`
#' only.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return A named list of parameter-name character vectors (possibly empty).
#' @seealso [set_spec_sections()].
#' @export
get_spec_parameter_sections <- function(spec, ...) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
  spec@sections@assignments
}

#' Get row filter rules from a TableSpec
#'
#' `get_spec_filter()` returns the list of row filter rules. Operates on
#' `TableSpec` only.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return List of quosures.
#' @seealso [set_spec_filter()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_filter(spec)
get_spec_filter <- function(spec, ...) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
  spec@row_filter
}

#' Get display transforms from a TableSpec
#'
#' `get_spec_transforms()` returns the display transforms configuration.
#' Operates on `TableSpec` only.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return Named list with theta, omega, sigma entries.
#' @seealso [set_spec_transforms()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_transforms(spec)
get_spec_transforms <- function(spec, ...) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
  spec@display_transforms
}

#' Get missing value handling from a TableSpec
#'
#' `get_spec_missing()` returns the missing-value handling configuration as a
#' named list. Operates on `TableSpec` only.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return A named list with elements `text` and `apply_to`.
#' @seealso [set_spec_missing()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_missing(spec)
get_spec_missing <- function(spec, ...) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
  list(
    text = spec@missing_text,
    apply_to = spec@missing_apply_to
  )
}

#' Get variability rules from a TableSpec
#'
#' `get_spec_variability()` returns the list of variability display rules.
#' Operates on `TableSpec` only.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return List of quosures.
#' @seealso [set_spec_variability()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_variability(spec)
get_spec_variability <- function(spec, ...) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
  spec@variability_rules
}

# ==============================================================================
# SummarySpec-Only Getters
# ==============================================================================

#' Get models to include from a SummarySpec
#'
#' `get_spec_models()` returns the character vector of model names to include,
#' or NULL when no model filter is set. Operates on `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param ... Not used.
#' @return Character vector of model names, or NULL.
#' @seealso [set_spec_models()].
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_models(spec)
get_spec_models <- function(spec, ...) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  spec@models_to_include
}

#' Get tag filter from a SummarySpec
#'
#' `get_spec_tag_filter()` returns the tag filtering configuration as a named
#' list. Operates on `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param ... Not used.
#' @return A named list with elements `include` and `exclude`.
#' @seealso [set_spec_tag_filter()].
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_tag_filter(spec)
get_spec_tag_filter <- function(spec, ...) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  list(
    include = spec@tag_filter,
    exclude = spec@tag_exclude
  )
}

#' Get remove_unrun_models from a SummarySpec
#'
#' `get_spec_remove_unrun()` returns whether models without completed runs are
#' excluded. Operates on `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param ... Not used.
#' @return Logical scalar.
#' @seealso [set_spec_remove_unrun()].
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_remove_unrun(spec)
get_spec_remove_unrun <- function(spec, ...) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  spec@remove_unrun_models
}

#' Get summary filter rules from a SummarySpec
#'
#' `get_spec_summary_filter()` returns the list of summary filter rules.
#' Operates on `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param ... Not used.
#' @return List of quosures.
#' @seealso [set_spec_summary_filter()].
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_summary_filter(spec)
get_spec_summary_filter <- function(spec, ...) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  spec@summary_filter
}

#' Get time format from a SummarySpec
#'
#' `get_spec_time_format()` returns the time format setting. Operates on
#' `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param ... Not used.
#' @return Character string: "seconds", "minutes", "hours", or "auto".
#' @seealso [set_spec_time_format()].
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_time_format(spec)
get_spec_time_format <- function(spec, ...) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  spec@time_format
}
