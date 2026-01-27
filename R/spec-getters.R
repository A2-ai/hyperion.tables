# ==============================================================================
# Spec Getter Functions
# ==============================================================================
# Functions for reading properties from TableSpec and SummarySpec objects.

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
#' @return Character vector of column names
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_columns(spec)
get_spec_columns <- function(spec) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }

  if (S7::S7_inherits(spec, TableSpec)) {
    # For TableSpec, merge columns and add_columns
    cols <- spec@columns
    if (!is.null(spec@add_columns)) {
      cols <- unique(c(cols, spec@add_columns))
    }
    return(cols)
  }

  # SummarySpec already has merged columns

  spec@columns
}

#' Get title from a spec
#'
#' Returns the table header title.
#'
#' @param spec A TableSpec or SummarySpec object
#' @return Character string
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_title(spec)
get_spec_title <- function(spec) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }
  spec@title
}

#' Get significant figures from a spec
#'
#' Returns the number of significant figures for numeric formatting.
#'
#' @param spec A TableSpec or SummarySpec object
#' @return Numeric value
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_sigfig(spec)
get_spec_sigfig <- function(spec) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }
  spec@n_sigfig
}

# ==============================================================================
# TableSpec-Only Getters
# ==============================================================================

#' Get name source from a TableSpec
#'
#' Returns the name field setting.
#'
#' @param spec A TableSpec object
#' @return Character string: "name", "display", or "nonmem_name"
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_name_source(spec)
get_spec_name_source <- function(spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }
  spec@name_source
}

#' Get CI options from a TableSpec
#'
#' Returns the CIOptions object.
#'
#' @param spec A TableSpec object
#' @return CIOptions object
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_ci(spec)
get_spec_ci <- function(spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }
  spec@ci
}

#' Get section rules from a TableSpec
#'
#' Returns the list of section assignment rules.
#'
#' @param spec A TableSpec object
#' @return List of quosures
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_sections(spec)
get_spec_sections <- function(spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }
  spec@sections
}

#' Get row filter rules from a TableSpec
#'
#' Returns the list of row filter rules.
#'
#' @param spec A TableSpec object
#' @return List of quosures
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_filter(spec)
get_spec_filter <- function(spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }
  spec@row_filter
}

#' Get display transforms from a TableSpec
#'
#' Returns the display transforms configuration.
#'
#' @param spec A TableSpec object
#' @return Named list with theta, omega, sigma entries
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_transforms(spec)
get_spec_transforms <- function(spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }
  spec@display_transforms
}

#' Get variability rules from a TableSpec
#'
#' Returns the list of variability display rules.
#'
#' @param spec A TableSpec object
#' @return List of quosures
#' @export
#' @examples
#' spec <- TableSpec()
#' get_spec_variability(spec)
get_spec_variability <- function(spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }
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
#' @return Character string: "seconds", "minutes", "hours", or "auto"
#' @export
#' @examples
#' spec <- SummarySpec()
#' get_spec_time_format(spec)
get_spec_time_format <- function(spec) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    stop("spec must be a SummarySpec object")
  }
  spec@time_format
}
