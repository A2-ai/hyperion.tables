# ==============================================================================
# Spec Modifier Functions
# ==============================================================================
# S7 generics and methods for modifying TableSpec and SummarySpec objects.

# ==============================================================================
# Column Operations (Both Specs)
# ==============================================================================

#' Add columns to a spec
#'
#' Appends columns to the spec's add_columns list. These columns will be
#' added to the default column set.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Column names to add (character strings)
#' @return Modified spec
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   add_spec_columns("shrinkage", "cv")
#'
#' sum_spec <- SummarySpec() |>
#'   add_spec_columns("estimation_time")
add_spec_columns <- S7::new_generic("add_spec_columns", "spec")

S7::method(add_spec_columns, TableSpec) <- function(spec, ...) {
  cols <- expand_ci_alias(c(...))
  spec@add_columns <- unique(c(spec@add_columns, cols))
  spec
}

S7::method(add_spec_columns, SummarySpec) <- function(spec, ...) {
  cols <- c(...)
  spec@add_columns <- unique(c(spec@add_columns, cols))
  spec@columns <- merge_summary_columns(spec@columns, spec@add_columns)
  spec
}

#' Drop columns from a spec
#'
#' Adds columns to the spec's drop_columns list. These columns will be
#' excluded from the output table.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Column names to drop (character strings)
#' @return Modified spec
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   drop_spec_columns("unit", "symbol")
#'
#' sum_spec <- SummarySpec() |>
#'   drop_spec_columns("description")
drop_spec_columns <- S7::new_generic("drop_spec_columns", "spec")

S7::method(drop_spec_columns, AnySpec) <- function(spec, ...) {
  spec@drop_columns <- unique(c(spec@drop_columns, c(...)))
  spec
}

#' Set columns for a spec
#'
#' Replaces the spec's columns list entirely. This overrides the default
#' column set.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Column names to include (character strings)
#' @return Modified spec
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_columns("name", "estimate", "rse")
set_spec_columns <- S7::new_generic("set_spec_columns", "spec")

S7::method(set_spec_columns, TableSpec) <- function(spec, ...) {
  spec@columns <- expand_ci_alias(c(...))
  spec
}

S7::method(set_spec_columns, SummarySpec) <- function(spec, ...) {
  spec@columns <- c(...)
  spec
}

# ==============================================================================
# Common Setters (Both Specs)
# ==============================================================================

#' Set the title for a spec
#'
#' Sets the table header title.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param title Character string for the table title
#' @param ... Additional arguments passed to methods.
#' @return Modified spec
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
#' Sets the number of significant figures for numeric formatting.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param n Positive integer for significant figures
#' @param ... Additional arguments passed to methods.
#' @return Modified spec
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
#' Controls the number of decimal places for OFV and dOFV values. Use NA to
#' keep significant-figure formatting.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param n Non-negative integer or NA
#' @param ... Additional arguments passed to methods.
#' @return Modified spec
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
#' Controls whether empty columns are automatically hidden.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param hide Logical value
#' @param ... Additional arguments passed to methods.
#' @return Modified spec
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
#' Controls how p-values are displayed in the table.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Additional arguments passed to the methods.
#' @param threshold Numeric threshold below which p-values display as "< threshold",
#'   or NULL to disable threshold display
#' @param scientific Logical. If TRUE, use scientific notation for p-values
#' @return Modified spec
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
#' Controls the order of footnote sections, or disables footnotes entirely.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param order Character vector of footnote sections in desired order, or NULL
#'   to disable footnotes. For TableSpec: "summary_info", "equations", "abbreviations".
#'   For SummarySpec: only "abbreviations" is valid.
#' @param ... Additional arguments passed to methods.
#' @return Modified spec
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
#' Filters out rows belonging to specified sections. Use `NA` to also
#' filter unmatched rows (those that didn't match any section rule).
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Section labels to exclude. Pass `NA` to exclude unmatched rows.
#'   Call with no arguments to clear the filter.
#' @return Modified spec
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

S7::method(set_spec_section_filter, AnySpec) <- function(spec, ...) {
  values <- c(...)
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

#' Set section rules for a spec
#'
#' Controls how rows are grouped into sections. Pass formula expressions
#' where the LHS is a condition and the RHS is the section label.
#'
#' For TableSpec, rules are evaluated against parameter columns
#' (e.g., `kind == "THETA" ~ "Structural Parameters"`).
#'
#' For SummarySpec, rules are evaluated row-by-row against summary columns
#' including `tags` (e.g., `"base" %in% tags ~ "Base Models"`).
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Section rule formulas
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec
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

S7::method(set_spec_sections, AnySpec) <- function(
  spec,
  ...,
  overwrite = FALSE
) {
  new_rules <- section_rules(...)
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
#' Controls how parameter names are displayed in the table.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @param source One of "name", "display", or "nonmem". If NULL, keeps current value.
#' @param append_omega_with_theta Logical. If TRUE, append associated theta
#'   names to omega parameters. If NULL, keeps current value.
#' @return Modified spec
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_parameter_names(source = "nonmem", append_omega_with_theta = FALSE)
set_spec_parameter_names <- S7::new_generic(
  "set_spec_parameter_names",
  "spec",
  function(spec, ..., source = NULL, append_omega_with_theta = NULL) {
    S7::S7_dispatch()
  }
)

S7::method(set_spec_parameter_names, TableSpec) <- function(
  spec,
  source = NULL,
  append_omega_with_theta = NULL
) {
  opts <- spec@parameter_names
  if (!is.null(source)) {
    opts@source <- source
  }
  if (!is.null(append_omega_with_theta)) {
    opts@append_omega_with_theta <- append_omega_with_theta
  }
  spec@parameter_names <- opts
  spec
}

#' Set CI options for a TableSpec
#'
#' Controls confidence interval rendering options.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @param level Confidence interval level (0-1, exclusive)
#' @param merge Logical. If TRUE, merge CI bounds into a single column
#' @param pattern sprintf pattern for merged CI display (must contain two %%s)
#' @param missing_text Text to show for missing CI values
#' @return Modified spec
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
#' Controls how NA values are displayed in the table.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @param text Text to substitute for NA values
#' @param apply_to Which columns to apply missing text to: "all", "numeric",
#'   or "character"
#' @return Modified spec
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
#' Controls which transforms are applied for display by parameter kind.
#'
#' @param spec A TableSpec object
#' @param ... Additional arguments passed to methods.
#' @param theta Columns to transform for theta parameters
#' @param omega Columns to transform for omega parameters
#' @param sigma Columns to transform for sigma parameters
#' @return Modified spec
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
#' Controls which parameters appear in the output table. Pass filter
#' expressions like `!fixed`, `diagonal`.
#'
#' @param spec A TableSpec object
#' @param ... Filter rule expressions
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_filter(!fixed, diagonal)
set_spec_filter <- S7::new_generic(
  "set_spec_filter",
  "spec",
  function(spec, ..., overwrite = FALSE) S7::S7_dispatch()
)

S7::method(set_spec_filter, TableSpec) <- function(
  spec,
  ...,
  overwrite = FALSE
) {
  new_rules <- filter_rules(...)
  if (overwrite) {
    spec@row_filter <- new_rules
  } else {
    spec@row_filter <- c(spec@row_filter, new_rules)
  }
  spec
}

#' Set variability rules for a TableSpec
#'
#' Controls how the variability display column is constructed.
#'
#' @param spec A TableSpec object
#' @param ... Variability rule formulas
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec
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

S7::method(set_spec_variability, TableSpec) <- function(
  spec,
  ...,
  overwrite = FALSE
) {
  new_rules <- variability_rules(...)
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
#' Controls how time columns are formatted.
#'
#' @param spec A SummarySpec object
#' @param format One of "seconds", "minutes", "hours", or "auto"
#' @param ... Additional arguments passed to methods.
#' @return Modified spec
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
#' Filters which models appear in the summary table by name.
#'
#' @param spec A SummarySpec object
#' @param models Character vector of model names, or NULL for all models
#' @param ... Additional arguments passed to methods.
#' @return Modified spec
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
#' Filters which models appear in the summary table by tags.
#'
#' @param spec A SummarySpec object
#' @param ... Additional arguments passed to methods.
#' @param tags Character vector of tags to include, or NULL for no inclusion
#'   filter. Only models with at least one matching tag are kept.
#' @param exclude Character vector of tags to exclude, or NULL for no exclusion
#'   filter. Models with any matching tag are removed. Applied after `tags`.
#' @return Modified spec
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
#' Controls whether models without completed runs are excluded.
#'
#' @param spec A SummarySpec object
#' @param remove Logical value
#' @param ... Additional arguments passed to methods.
#' @return Modified spec
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
#' Controls which models appear in the summary table based on filter
#' expressions evaluated against summary columns.
#'
#' @param spec A SummarySpec object
#' @param ... Filter rule expressions
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_summary_filter(ofv < 1000)
set_spec_summary_filter <- S7::new_generic(
  "set_spec_summary_filter",
  "spec",
  function(spec, ..., overwrite = FALSE) S7::S7_dispatch()
)

S7::method(set_spec_summary_filter, SummarySpec) <- function(
  spec,
  ...,
  overwrite = FALSE
) {
  new_rules <- summary_filter_rules(...)
  if (overwrite) {
    spec@summary_filter <- new_rules
  } else {
    spec@summary_filter <- c(spec@summary_filter, new_rules)
  }
  spec
}
