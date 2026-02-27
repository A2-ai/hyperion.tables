# ==============================================================================
# Spec Getter Functions
# ==============================================================================
# S7 generics and methods for reading properties from TableSpec and SummarySpec.

# ==============================================================================
# Common Getters (Both Specs)
# ==============================================================================

#' Get columns from a spec
#'
#' Returns the current columns list from the spec. For TableSpec, this includes
#' the base columns plus any add_columns. For SummarySpec, columns are already
#' merged with add_columns in construction.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Additional arguments passed to methods.
#' @return Character vector of column names
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
#' Returns the table header title.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Additional arguments passed to methods.
#' @return Character string
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
#' Returns the number of significant figures for numeric formatting.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Additional arguments passed to methods.
#' @return Numeric value
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
#' Returns the ParameterNameOptions object controlling how parameter names
#' are displayed.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @return ParameterNameOptions object
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
#' Returns the CIOptions object.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @return CIOptions object
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_ci(spec)
get_spec_ci <- S7::new_generic("get_spec_ci", "spec")

S7::method(get_spec_ci, TableSpec) <- function(spec) {
  spec@ci
}

#' Get section rules from a spec
#'
#' Returns the list of section assignment rules.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Additional arguments passed to methods.
#' @return List of quosures
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
#' Returns the section labels being filtered out, or NULL if no filter is set.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Additional arguments passed to methods.
#' @return Character vector or NULL
#' @export
get_spec_section_filter <- S7::new_generic("get_spec_section_filter", "spec")

S7::method(get_spec_section_filter, AnySpec) <- function(spec) {
  spec@section_filter
}

#' Get row filter rules from a TableSpec
#'
#' Returns the list of row filter rules.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @return List of quosures
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
#' Returns the display transforms configuration.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @return Named list with theta, omega, sigma entries
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
#' Returns the list of variability display rules.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @return List of quosures
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
#' Returns the time format setting.
#'
#' @param spec A SummarySpec object
#' @param ... Additional arguments passed to methods.
#' @return Character string: "seconds", "minutes", "hours", or "auto"
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_time_format(spec)
get_spec_time_format <- S7::new_generic("get_spec_time_format", "spec")

S7::method(get_spec_time_format, SummarySpec) <- function(spec) {
  spec@time_format
}
