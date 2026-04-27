# ==============================================================================
# Spec Getter Functions
# ==============================================================================
# S7 generics and methods for reading properties from TableSpec and SummarySpec.

# ==============================================================================
# Common Getters (Both Specs)
# ==============================================================================

#' Get columns from a spec
#'
#' @description
#' `get_spec_columns()` is an S7 generic that returns the current columns list
#' from the spec. For TableSpec, this includes the base columns plus any
#' add_columns. For SummarySpec, columns are already merged with add_columns in
#' construction.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("get_spec_columns")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Character vector of column names.
#' @seealso [set_spec_columns()], [add_spec_columns()], [drop_spec_columns()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_columns(spec)
get_spec_columns <- S7::new_generic("get_spec_columns", "spec")

S7::method(get_spec_columns, TableSpec) <- function(spec) {
  cols <- spec@columns
  if (!is.null(spec@add_columns)) {
    cols <- unique(c(cols, spec@add_columns))
  }
  cols
}

S7::method(get_spec_columns, SummarySpec) <- function(spec) {
  spec@columns
}

#' Get title from a spec
#'
#' @description
#' `get_spec_title()` is an S7 generic that returns the table header title.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("get_spec_title")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Character string.
#' @seealso [set_spec_title()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_title(spec)
get_spec_title <- S7::new_generic("get_spec_title", "spec")

S7::method(get_spec_title, AnySpec) <- function(spec) {
  spec@title
}

#' Get significant figures from a spec
#'
#' @description
#' `get_spec_sigfig()` is an S7 generic that returns the number of significant
#' figures used for numeric formatting.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("get_spec_sigfig")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return Numeric value.
#' @seealso [set_spec_sigfig()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_sigfig(spec)
get_spec_sigfig <- S7::new_generic("get_spec_sigfig", "spec")

S7::method(get_spec_sigfig, AnySpec) <- function(spec) {
  spec@n_sigfig
}

# ==============================================================================
# TableSpec-Only Getters
# ==============================================================================

#' Get parameter name options from a TableSpec
#'
#' `get_spec_parameter_names()` is an S7 generic that returns the
#' ParameterNameOptions object controlling how parameter names are displayed.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return ParameterNameOptions object.
#' @seealso [set_spec_parameter_names()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_parameter_names(spec)
get_spec_parameter_names <- S7::new_generic("get_spec_parameter_names", "spec")

S7::method(get_spec_parameter_names, TableSpec) <- function(spec) {
  spec@parameter_names
}

#' Get CI options from a TableSpec
#'
#' `get_spec_ci()` is an S7 generic that returns the CIOptions object.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return CIOptions object.
#' @seealso [set_spec_ci()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_ci(spec)
get_spec_ci <- S7::new_generic("get_spec_ci", "spec")

S7::method(get_spec_ci, TableSpec) <- function(spec) {
  spec@ci
}

#' Get section configuration from a spec
#'
#' @description
#' `get_spec_sections()` is an S7 generic that returns the spec's
#' [Sections] object (rules, assignments, order, filter).
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("get_spec_sections")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @return A [Sections] object.
#' @seealso [set_spec_sections()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_sections(spec)
get_spec_sections <- S7::new_generic("get_spec_sections", "spec")

S7::method(get_spec_sections, AnySpec) <- function(spec) {
  spec@sections
}

#' Get section filter from a spec
#'
#' @description
#' `get_spec_section_filter()` returns the current filter as a list with one
#' of two shapes: `list(exclude = c(...))`, `list(keep = c(...))`, or empty
#' `list()` when no filter is set.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("get_spec_section_filter")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @return A named list (possibly empty).
#' @seealso [set_spec_section_filter()].
#' @export
get_spec_section_filter <- S7::new_generic("get_spec_section_filter", "spec")

S7::method(get_spec_section_filter, AnySpec) <- function(spec) {
  s <- spec@sections
  if (length(s@filter_exclude) > 0L) {
    list(exclude = s@filter_exclude)
  } else if (length(s@filter_keep) > 0L) {
    list(keep = s@filter_keep)
  } else {
    list()
  }
}

#' Get per-parameter section assignments from a TableSpec
#'
#' Returns the assignments as a named list keyed by section label, where
#' each value is a character vector of parameter names. Mirrors the shape
#' passed to `set_spec_sections(parameters = ...)`.
#'
#' @param spec A TableSpec object.
#' @return A named list of parameter-name character vectors (possibly empty).
#' @seealso [set_spec_sections()].
#' @export
get_spec_parameter_sections <- S7::new_generic(
  "get_spec_parameter_sections",
  "spec"
)

S7::method(get_spec_parameter_sections, TableSpec) <- function(spec) {
  spec@sections@assignments
}

#' Get row filter rules from a TableSpec
#'
#' `get_spec_filter()` is an S7 generic that returns the list of row filter
#' rules.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return List of quosures.
#' @seealso [set_spec_filter()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_filter(spec)
get_spec_filter <- S7::new_generic("get_spec_filter", "spec")

S7::method(get_spec_filter, TableSpec) <- function(spec) {
  spec@row_filter
}

#' Get display transforms from a TableSpec
#'
#' `get_spec_transforms()` is an S7 generic that returns the display transforms
#' configuration.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return Named list with theta, omega, sigma entries.
#' @seealso [set_spec_transforms()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_transforms(spec)
get_spec_transforms <- S7::new_generic("get_spec_transforms", "spec")

S7::method(get_spec_transforms, TableSpec) <- function(spec) {
  spec@display_transforms
}

#' Get variability rules from a TableSpec
#'
#' `get_spec_variability()` is an S7 generic that returns the list of
#' variability display rules.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @return List of quosures.
#' @seealso [set_spec_variability()].
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_variability(spec)
get_spec_variability <- S7::new_generic("get_spec_variability", "spec")

S7::method(get_spec_variability, TableSpec) <- function(spec) {
  spec@variability_rules
}

# ==============================================================================
# SummarySpec-Only Getters
# ==============================================================================

#' Get time format from a SummarySpec
#'
#' `get_spec_time_format()` is an S7 generic that returns the time format
#' setting.
#'
#' @param spec A SummarySpec object.
#' @param ... Not used.
#' @return Character string: "seconds", "minutes", "hours", or "auto".
#' @seealso [set_spec_time_format()].
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_time_format(spec)
get_spec_time_format <- S7::new_generic("get_spec_time_format", "spec")

S7::method(get_spec_time_format, SummarySpec) <- function(spec) {
  spec@time_format
}
