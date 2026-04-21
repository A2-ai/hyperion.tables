# ==============================================================================
# Spec Modifier Functions
# ==============================================================================
# S7 generics and methods for modifying TableSpec and SummarySpec objects.

# ==============================================================================
# Column Operations (Both Specs)
# ==============================================================================

#' Add columns to a spec
#'
#' @description
#' `add_spec_columns()` is an S7 generic that appends columns to the spec's
#' `add_columns` list. These columns will be added to the default column set.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("add_spec_columns")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... See methods.
#' @return Modified spec.
#' @seealso [get_spec_columns()], [set_spec_columns()], [drop_spec_columns()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   add_spec_columns("shrinkage", "cv")
#'
#' sum_spec <- SummarySpec() |>
#'   add_spec_columns("estimation_time")
add_spec_columns <- S7::new_generic("add_spec_columns", "spec")

#' Add columns to a TableSpec
#'
#' Method for [add_spec_columns()] on `TableSpec`. Confidence-interval column
#' aliases (e.g., `"ci"`) are expanded to their underlying column names.
#'
#' @param spec A TableSpec object.
#' @param ... Column names as unnamed character strings. Named arguments are
#'   ignored with a warning.
#' @return Modified TableSpec.
S7::method(add_spec_columns, TableSpec) <- function(spec, ...) {
  dots <- capture_unnamed_dots(..., .enquo = FALSE)
  cols <- expand_ci_alias(unlist(dots))
  spec@add_columns <- unique(c(spec@add_columns, cols))
  spec
}

#' Add columns to a SummarySpec
#'
#' Method for [add_spec_columns()] on `SummarySpec`. Updates `@columns` to
#' reflect the merged default + added columns.
#'
#' @aliases add_spec_columns-hyperion.tables-SummarySpec-method
#' @param spec A SummarySpec object.
#' @param ... Column names as unnamed character strings. Named arguments are
#'   ignored with a warning.
#' @return Modified SummarySpec.
S7::method(add_spec_columns, SummarySpec) <- function(spec, ...) {
  dots <- capture_unnamed_dots(..., .enquo = FALSE)
  cols <- unlist(dots)
  spec@add_columns <- unique(c(spec@add_columns, cols))
  spec@columns <- merge_summary_columns(spec@columns, spec@add_columns)
  spec
}

#' Drop columns from a spec
#'
#' @description
#' `drop_spec_columns()` is an S7 generic that adds columns to the spec's
#' `drop_columns` list. These columns will be excluded from the output table.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("drop_spec_columns")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... See methods.
#' @return Modified spec.
#' @seealso [get_spec_columns()], [add_spec_columns()], [set_spec_columns()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   drop_spec_columns("unit", "symbol")
#'
#' sum_spec <- SummarySpec() |>
#'   drop_spec_columns("description")
drop_spec_columns <- S7::new_generic("drop_spec_columns", "spec")

#' Drop columns from a spec
#'
#' Method for [drop_spec_columns()] on both `TableSpec` and `SummarySpec`.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Column names as unnamed character strings. Named arguments are
#'   ignored with a warning.
#' @return Modified spec.
S7::method(drop_spec_columns, AnySpec) <- function(spec, ...) {
  dots <- capture_unnamed_dots(..., .enquo = FALSE)
  cols <- unlist(dots)
  spec@drop_columns <- unique(c(spec@drop_columns, cols))
  spec
}

#' Set columns for a spec
#'
#' @description
#' `set_spec_columns()` is an S7 generic that replaces the spec's columns list
#' entirely. This overrides the default column set.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_columns")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... See methods.
#' @return Modified spec.
#' @seealso [get_spec_columns()], [add_spec_columns()], [drop_spec_columns()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_columns("name", "estimate", "rse")
set_spec_columns <- S7::new_generic("set_spec_columns", "spec")

#' Set columns for a TableSpec
#'
#' Method for [set_spec_columns()] on `TableSpec`. Confidence-interval column
#' aliases (e.g., `"ci"`) are expanded to their underlying column names.
#'
#' @param spec A TableSpec object.
#' @param ... Column names as unnamed character strings. Named arguments are
#'   ignored with a warning.
#' @return Modified TableSpec.
S7::method(set_spec_columns, TableSpec) <- function(spec, ...) {
  dots <- capture_unnamed_dots(..., .enquo = FALSE)
  cols <- expand_ci_alias(unlist(dots))
  spec@columns <- cols
  spec
}

#' Set columns for a SummarySpec
#'
#' Method for [set_spec_columns()] on `SummarySpec`.
#'
#' @param spec A SummarySpec object.
#' @param ... Column names as unnamed character strings. Named arguments are
#'   ignored with a warning.
#' @return Modified SummarySpec.
S7::method(set_spec_columns, SummarySpec) <- function(spec, ...) {
  dots <- capture_unnamed_dots(..., .enquo = FALSE)
  spec@columns <- unlist(dots)
  spec
}

# ==============================================================================
# Common Setters (Both Specs)
# ==============================================================================

#' Set the title for a spec
#'
#' @description
#' `set_spec_title()` is an S7 generic that sets the table header title.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_title")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param title Character string for the table title.
#' @param ... Not used.
#' @return Modified spec.
#' @seealso [get_spec_title()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_title("Parameter Estimates")
set_spec_title <- S7::new_generic(
  "set_spec_title",
  "spec",
  function(spec, title, ...) S7::S7_dispatch()
)

S7::method(set_spec_title, AnySpec) <- function(spec, title) {
  spec@title <- title
  spec
}

#' Set significant figures for a spec
#'
#' @description
#' `set_spec_sigfig()` is an S7 generic that sets the number of significant
#' figures for numeric formatting.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_sigfig")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param n Positive integer for significant figures.
#' @param ... Not used.
#' @return Modified spec.
#' @seealso [get_spec_sigfig()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_sigfig(4)
set_spec_sigfig <- S7::new_generic(
  "set_spec_sigfig",
  "spec",
  function(spec, n, ...) S7::S7_dispatch()
)

S7::method(set_spec_sigfig, AnySpec) <- function(spec, n) {
  spec@n_sigfig <- n
  spec
}

#' Set decimal places for OFV values
#'
#' @description
#' `set_spec_ofv_decimals()` is an S7 generic that controls the number of
#' decimal places for OFV and dOFV values. Use `NA` to keep significant-figure
#' formatting.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_ofv_decimals")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param n Non-negative integer or NA.
#' @param ... Not used.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_ofv_decimals(2)
set_spec_ofv_decimals <- S7::new_generic(
  "set_spec_ofv_decimals",
  "spec",
  function(spec, n, ...) S7::S7_dispatch()
)

S7::method(set_spec_ofv_decimals, AnySpec) <- function(spec, n) {
  spec@n_decimals_ofv <- n
  spec
}

#' Set hide_empty_columns for a spec
#'
#' @description
#' `set_spec_hide_empty()` is an S7 generic that controls whether empty
#' columns are automatically hidden.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_hide_empty")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param hide Logical value.
#' @param ... Not used.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_hide_empty(FALSE)
set_spec_hide_empty <- S7::new_generic(
  "set_spec_hide_empty",
  "spec",
  function(spec, hide, ...) S7::S7_dispatch()
)

S7::method(set_spec_hide_empty, AnySpec) <- function(spec, hide) {
  spec@hide_empty_columns <- hide
  spec
}

#' Set p-value formatting for a spec
#'
#' @description
#' `set_spec_pvalue()` is an S7 generic that controls how p-values are
#' displayed in the table.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_pvalue")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @param threshold Numeric threshold below which p-values display as "< threshold",
#'   or NULL to disable threshold display.
#' @param scientific Logical. If TRUE, use scientific notation for p-values.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_pvalue(threshold = 0.001, scientific = TRUE)
set_spec_pvalue <- S7::new_generic(
  "set_spec_pvalue",
  "spec",
  function(spec, ..., threshold, scientific) {
    S7::S7_dispatch()
  }
)

S7::method(set_spec_pvalue, AnySpec) <- function(spec, threshold, scientific) {
  if (!missing(threshold)) {
    spec@pvalue_threshold <- threshold
  }
  if (!missing(scientific)) {
    spec@pvalue_scientific <- scientific
  }
  spec
}

#' Set footnote order for a spec
#'
#' @description
#' `set_spec_footnotes()` is an S7 generic that controls the order of footnote
#' sections, or disables footnotes entirely.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_footnotes")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param order Character vector of footnote sections in desired order, or NULL
#'   to disable footnotes. For TableSpec: "summary_info", "equations", "abbreviations".
#'   For SummarySpec: only "abbreviations" is valid.
#' @param ... Not used.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_footnotes(c("abbreviations", "equations"))
#'
#' # Disable footnotes
#' spec <- TableSpec() |>
#'   set_spec_footnotes(NULL)
set_spec_footnotes <- S7::new_generic(
  "set_spec_footnotes",
  "spec",
  function(spec, order, ...) S7::S7_dispatch()
)

S7::method(set_spec_footnotes, AnySpec) <- function(spec, order) {
  spec@footnote_order <- order
  spec
}

#' Set section filter for a spec
#'
#' @description
#' `set_spec_section_filter()` is an S7 generic that filters out rows belonging
#' to specified sections. Use `NA` to also filter unmatched rows (those that
#' didn't match any section rule). Call with no arguments to clear the filter.
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_section_filter")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... See methods.
#' @return Modified spec.
#' @seealso [get_spec_section_filter()].
#' @export
#' @examples
#' spec <- SummarySpec(
#'   sections = section_rules(
#'     "base" %in% tags ~ "Base Models",
#'     TRUE ~ "Other"
#'   )
#' ) |>
#'   set_spec_section_filter("Other")
set_spec_section_filter <- S7::new_generic("set_spec_section_filter", "spec")

#' Set section filter for a spec
#'
#' Method for [set_spec_section_filter()] on both `TableSpec` and `SummarySpec`.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Section labels to exclude, or `NA` to also exclude unmatched
#'   rows. Call with no arguments to clear the filter. Named arguments are
#'   ignored with a warning.
#' @return Modified spec.
S7::method(set_spec_section_filter, AnySpec) <- function(spec, ...) {
  dots <- capture_unnamed_dots(...)
  values <- unlist(
    lapply(dots, function(dot) rlang::eval_tidy(dot)),
    recursive = FALSE,
    use.names = FALSE
  )
  if (length(values) == 0) {
    spec@section_filter <- NULL
  } else {
    spec@section_filter <- as.character(values)
  }
  spec
}

# ==============================================================================
# Section Rules (Both Specs)
# ==============================================================================

#' @noRd
capture_unnamed_dots <- function(..., .enquo = TRUE) {
  dots <- if (.enquo) rlang::enquos(...) else rlang::list2(...)
  dot_names <- rlang::names2(dots)
  unnamed <- dot_names == ""

  if (any(!unnamed)) {
    ignored <- unique(dot_names[!unnamed])
    rlang::warn(
      paste0(
        "Ignoring named argument",
        if (length(ignored) == 1) "" else "s",
        " in `...`: ",
        paste(ignored, collapse = ", ")
      )
    )
  }

  unname(dots[unnamed])
}

#' Set section rules for a spec
#'
#' @description
#' `set_spec_sections()` is an S7 generic that controls how rows are grouped
#' into sections. Pass formula expressions where the LHS is a condition and
#' the RHS is the section label.
#'
#' For TableSpec, rules are evaluated against parameter columns
#' (e.g., `kind == "THETA" ~ "Structural Parameters"`).
#'
#' For SummarySpec, rules are evaluated row-by-row against summary columns
#' including `tags` (e.g., `"base" %in% tags ~ "Base Models"`).
#'
#' Methods are available for the following classes:
#'
#' `r doclisting::methods_list("set_spec_sections")`
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... See methods.
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec.
#' @seealso [get_spec_sections()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_sections(
#'     kind == "THETA" ~ "Structural",
#'     kind == "OMEGA" ~ "IIV"
#'   )
set_spec_sections <- S7::new_generic(
  "set_spec_sections",
  "spec",
  function(spec, ..., overwrite = FALSE) S7::S7_dispatch()
)

#' Set section rules for a spec
#'
#' Method for [set_spec_sections()] on both `TableSpec` and `SummarySpec`.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Section rule formulas. Named arguments are ignored with a
#'   warning.
#' @param overwrite If FALSE (default), append to existing rules. If TRUE,
#'   replace all existing rules.
#' @return Modified spec.
S7::method(set_spec_sections, AnySpec) <- function(
  spec,
  ...,
  overwrite = FALSE
) {
  rule_dots <- capture_unnamed_dots(...)
  new_rules <- section_rules(!!!rule_dots)

  if (overwrite) {
    spec@sections <- new_rules
  } else {
    spec@sections <- c(spec@sections, new_rules)
  }
  spec
}

# ==============================================================================
# TableSpec-Only Setters
# ==============================================================================

#' Set parameter name options for a TableSpec
#'
#' `set_spec_parameter_names()` is an S7 generic that controls how parameter
#' names are displayed in the table.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @param source One of "name", "display", or "nonmem". If NULL, keeps current value.
#' @return Modified spec.
#' @seealso [get_spec_parameter_names()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_parameter_names(source = "nonmem")
set_spec_parameter_names <- S7::new_generic(
  "set_spec_parameter_names",
  "spec",
  function(spec, ..., source = NULL) {
    S7::S7_dispatch()
  }
)

S7::method(set_spec_parameter_names, TableSpec) <- function(
  spec,
  source = NULL
) {
  opts <- spec@parameter_names
  if (!is.null(source)) {
    opts@source <- source
  }
  spec@parameter_names <- opts
  spec
}

#' Set CI options for a TableSpec
#'
#' `set_spec_ci()` is an S7 generic that controls confidence interval rendering
#' options.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @param level Confidence interval level (0-1, exclusive).
#' @param merge Logical. If TRUE, merge CI bounds into a single column.
#' @param pattern sprintf pattern for merged CI display (must contain two %%s).
#' @param missing_text Text to show for missing CI values.
#' @return Modified spec.
#' @seealso [get_spec_ci()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_ci(level = 0.90, merge = TRUE, pattern = "(%s, %s)")
set_spec_ci <- S7::new_generic(
  "set_spec_ci",
  "spec",
  function(
    spec,
    ...,
    level = NULL,
    merge = NULL,
    pattern = NULL,
    missing_text = NULL
  ) {
    S7::S7_dispatch()
  }
)

S7::method(set_spec_ci, TableSpec) <- function(
  spec,
  level = NULL,
  merge = NULL,
  pattern = NULL,
  missing_text = NULL
) {
  ci_args <- list(
    level = spec@ci@level,
    merge = spec@ci@merge,
    pattern = spec@ci@pattern,
    missing_text = spec@ci@missing_text
  )

  if (!is.null(level)) {
    ci_args$level <- level
  }
  if (!is.null(merge)) {
    ci_args$merge <- merge
  }
  if (!is.null(pattern)) {
    ci_args$pattern <- pattern
  }
  if (!is.null(missing_text)) {
    ci_args$missing_text <- missing_text
  }

  spec@ci <- do.call(CIOptions, ci_args)
  spec
}

#' Set missing value handling for a TableSpec
#'
#' `set_spec_missing()` is an S7 generic that controls how NA values are
#' displayed in the table.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @param text Text to substitute for NA values.
#' @param apply_to Which columns to apply missing text to: "all", "numeric",
#'   or "character".
#' @return Modified spec.
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_missing("-", apply_to = "numeric")
set_spec_missing <- S7::new_generic(
  "set_spec_missing",
  "spec",
  function(spec, text = NULL, ..., apply_to = NULL) S7::S7_dispatch()
)

S7::method(set_spec_missing, TableSpec) <- function(
  spec,
  text = NULL,
  apply_to = NULL
) {
  if (!is.null(text)) {
    spec@missing_text <- text
  }
  if (!is.null(apply_to)) {
    spec@missing_apply_to <- apply_to
  }
  spec
}

#' Set display transforms for a TableSpec
#'
#' `set_spec_transforms()` is an S7 generic that controls which transforms are
#' applied for display by parameter kind.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @param theta Columns to transform for theta parameters.
#' @param omega Columns to transform for omega parameters.
#' @param sigma Columns to transform for sigma parameters.
#' @return Modified spec.
#' @seealso [get_spec_transforms()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_transforms(theta = "all", omega = c("estimate", "cv"))
set_spec_transforms <- S7::new_generic(
  "set_spec_transforms",
  "spec",
  function(spec, ..., theta = NULL, omega = NULL, sigma = NULL) {
    S7::S7_dispatch()
  }
)

S7::method(set_spec_transforms, TableSpec) <- function(
  spec,
  theta = NULL,
  omega = NULL,
  sigma = NULL
) {
  transforms <- spec@display_transforms
  if (!is.null(theta)) {
    transforms$theta <- theta
  }
  if (!is.null(omega)) {
    transforms$omega <- omega
  }
  if (!is.null(sigma)) {
    transforms$sigma <- sigma
  }
  spec@display_transforms <- transforms
  spec
}

# ==============================================================================
# TableSpec Rule Modifiers
# ==============================================================================

#' Set row filter rules for a TableSpec
#'
#' `set_spec_filter()` is an S7 generic that controls which parameters appear
#' in the output table. Pass filter expressions like `!fixed`, `diagonal`.
#'
#' @param spec A TableSpec object.
#' @param ... See methods.
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec.
#' @seealso [get_spec_filter()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_filter(!fixed, diagonal)
set_spec_filter <- S7::new_generic(
  "set_spec_filter",
  "spec",
  function(spec, ..., overwrite = FALSE) S7::S7_dispatch()
)

#' Set row filter rules for a TableSpec
#'
#' Method for [set_spec_filter()] on `TableSpec`.
#'
#' @param spec A TableSpec object.
#' @param ... Filter rule expressions. Named arguments are ignored with a
#'   warning.
#' @param overwrite If FALSE (default), append to existing rules. If TRUE,
#'   replace all existing rules.
#' @return Modified TableSpec.
S7::method(set_spec_filter, TableSpec) <- function(
  spec,
  ...,
  overwrite = FALSE
) {
  rule_dots <- capture_unnamed_dots(...)
  new_rules <- filter_rules(!!!rule_dots)
  if (overwrite) {
    spec@row_filter <- new_rules
  } else {
    spec@row_filter <- c(spec@row_filter, new_rules)
  }
  spec
}

#' Set variability rules for a TableSpec
#'
#' `set_spec_variability()` is an S7 generic that controls how the variability
#' display column is constructed.
#'
#' @param spec A TableSpec object.
#' @param ... See methods.
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec.
#' @seealso [get_spec_variability()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_variability(
#'     fixed ~ "(Fixed)",
#'     !is.na(cv) ~ sprintf("CV = %s%%", cv),
#'     TRUE ~ NA_character_
#'   )
set_spec_variability <- S7::new_generic(
  "set_spec_variability",
  "spec",
  function(spec, ..., overwrite = FALSE) S7::S7_dispatch()
)

#' Set variability rules for a TableSpec
#'
#' Method for [set_spec_variability()] on `TableSpec`.
#'
#' @param spec A TableSpec object.
#' @param ... Variability rule formulas. Named arguments are ignored with a
#'   warning.
#' @param overwrite If FALSE (default), append to existing rules. If TRUE,
#'   replace all existing rules.
#' @return Modified TableSpec.
S7::method(set_spec_variability, TableSpec) <- function(
  spec,
  ...,
  overwrite = FALSE
) {
  rule_dots <- capture_unnamed_dots(...)
  new_rules <- variability_rules(!!!rule_dots)
  if (overwrite) {
    spec@variability_rules <- new_rules
  } else {
    spec@variability_rules <- c(spec@variability_rules, new_rules)
  }
  spec
}

# ==============================================================================
# SummarySpec-Only Setters
# ==============================================================================

#' Set time format for a SummarySpec
#'
#' `set_spec_time_format()` is an S7 generic that controls how time columns
#' are formatted.
#'
#' @param spec A SummarySpec object.
#' @param format One of "seconds", "minutes", "hours", or "auto".
#' @param ... Not used.
#' @return Modified spec.
#' @seealso [get_spec_time_format()].
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_time_format("minutes")
set_spec_time_format <- S7::new_generic(
  "set_spec_time_format",
  "spec",
  function(spec, format, ...) S7::S7_dispatch()
)

S7::method(set_spec_time_format, SummarySpec) <- function(spec, format) {
  spec@time_format <- format
  spec
}

#' Set models to include for a SummarySpec
#'
#' `set_spec_models()` is an S7 generic that filters which models appear in
#' the summary table by name.
#'
#' @param spec A SummarySpec object.
#' @param models Character vector of model names, or NULL for all models.
#' @param ... Not used.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_models(c("run001", "run002", "run003"))
set_spec_models <- S7::new_generic(
  "set_spec_models",
  "spec",
  function(spec, models, ...) S7::S7_dispatch()
)

S7::method(set_spec_models, SummarySpec) <- function(spec, models) {
  spec@models_to_include <- models
  spec
}

#' Set tag filter for a SummarySpec
#'
#' `set_spec_tag_filter()` is an S7 generic that filters which models appear
#' in the summary table by tags.
#'
#' @param spec A SummarySpec object.
#' @param ... Not used.
#' @param tags Character vector of tags to include, or NULL for no inclusion
#'   filter. Only models with at least one matching tag are kept.
#' @param exclude Character vector of tags to exclude, or NULL for no exclusion
#'   filter. Models with any matching tag are removed. Applied after `tags`.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_tag_filter(c("final", "approved"))
#'
#' # Exclude models tagged "failed"
#' spec <- SummarySpec() |>
#'   set_spec_tag_filter(exclude = "failed")
set_spec_tag_filter <- S7::new_generic(
  "set_spec_tag_filter",
  "spec",
  function(spec, ..., tags, exclude) S7::S7_dispatch()
)

S7::method(set_spec_tag_filter, SummarySpec) <- function(spec, tags, exclude) {
  if (!missing(tags)) {
    spec@tag_filter <- tags
  }
  if (!missing(exclude)) {
    spec@tag_exclude <- exclude
  }
  spec
}

#' Set remove_unrun_models for a SummarySpec
#'
#' `set_spec_remove_unrun()` is an S7 generic that controls whether models
#' without completed runs are excluded.
#'
#' @param spec A SummarySpec object.
#' @param remove Logical value.
#' @param ... Not used.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_remove_unrun(FALSE)
set_spec_remove_unrun <- S7::new_generic(
  "set_spec_remove_unrun",
  "spec",
  function(spec, remove, ...) S7::S7_dispatch()
)

S7::method(set_spec_remove_unrun, SummarySpec) <- function(spec, remove) {
  spec@remove_unrun_models <- remove
  spec
}

#' Set summary filter rules for a SummarySpec
#'
#' `set_spec_summary_filter()` is an S7 generic that controls which models
#' appear in the summary table based on filter expressions evaluated against
#' summary columns.
#'
#' @param spec A SummarySpec object.
#' @param ... See methods.
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_summary_filter(ofv < 1000)
set_spec_summary_filter <- S7::new_generic(
  "set_spec_summary_filter",
  "spec",
  function(spec, ..., overwrite = FALSE) S7::S7_dispatch()
)

#' Set summary filter rules for a SummarySpec
#'
#' Method for [set_spec_summary_filter()] on `SummarySpec`.
#'
#' @param spec A SummarySpec object.
#' @param ... Summary filter rule expressions evaluated against summary
#'   columns. Named arguments are ignored with a warning.
#' @param overwrite If FALSE (default), append to existing rules. If TRUE,
#'   replace all existing rules.
#' @return Modified SummarySpec.
S7::method(set_spec_summary_filter, SummarySpec) <- function(
  spec,
  ...,
  overwrite = FALSE
) {
  rule_dots <- capture_unnamed_dots(...)
  new_rules <- summary_filter_rules(!!!rule_dots)
  if (overwrite) {
    spec@summary_filter <- new_rules
  } else {
    spec@summary_filter <- c(spec@summary_filter, new_rules)
  }
  spec
}
