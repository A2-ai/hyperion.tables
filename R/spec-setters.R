# ==============================================================================
# Spec Modifier Functions
# ==============================================================================
# S7 generics and methods for modifying TableSpec and SummarySpec objects.

# ==============================================================================
# Column Operations (Both Specs)
# ==============================================================================

#' Normalize a user-supplied column vector for a spec
#'
#' Internal hook called by column setters before storing user input. The
#' TableSpec method expands the `"ci"` alias into `c("ci_low", "ci_high")`;
#' the SummarySpec method passes the input through unchanged.
#' @noRd
normalize_columns <- S7::new_generic("normalize_columns", "spec")

S7::method(normalize_columns, TableSpec) <- function(spec, cols) {
  expand_ci_alias(cols)
}

S7::method(normalize_columns, SummarySpec) <- function(spec, cols) {
  cols
}

#' Add columns to a spec
#'
#' @description
#' `add_spec_columns()` is an S7 generic that appends columns to the spec's
#' `add_columns` list. These columns will be added to the default column set.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Column names as unnamed character strings. For TableSpec, the
#'   alias `"ci"` expands to the configured confidence-interval bounds columns.
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

S7::method(add_spec_columns, BaseSpec) <- function(spec, ...) {
  dots <- capture_unnamed_dots(..., .enquo = FALSE)
  cols <- normalize_columns(spec, unlist(dots))
  spec@add_columns <- unique(c(spec@add_columns, cols))
  spec
}

#' Drop columns from a spec
#'
#' @description
#' `drop_spec_columns()` is an S7 generic that adds columns to the spec's
#' `drop_columns` list. These columns will be excluded from the output table.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Column names as unnamed character strings. Named arguments are
#'   ignored with a warning.
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

S7::method(drop_spec_columns, BaseSpec) <- function(spec, ...) {
  dots <- capture_unnamed_dots(..., .enquo = FALSE)
  cols <- unlist(dots)
  spec@drop_columns <- unique(c(spec@drop_columns, cols))
  spec
}

#' Set columns for a spec
#'
#' `set_spec_columns()` replaces the spec's columns list entirely. This
#' overrides the default column set.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Column names as unnamed character strings. For TableSpec, the
#'   alias `"ci"` expands to the configured confidence-interval bounds columns.
#' @return Modified spec.
#' @seealso [get_spec_columns()], [add_spec_columns()], [drop_spec_columns()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_columns("name", "estimate", "rse")
set_spec_columns <- function(spec, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  dots <- capture_unnamed_dots(..., .enquo = FALSE)
  cols <- normalize_columns(spec, unlist(dots))
  spec@columns <- cols
  spec
}

# ==============================================================================
# Common Setters (Both Specs)
# ==============================================================================

#' Set the title for a spec
#'
#' `set_spec_title()` sets the table header title.
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
set_spec_title <- function(spec, title, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@title <- title
  spec
}

#' Set significant figures for a spec
#'
#' `set_spec_sigfig()` sets the number of significant figures for numeric
#' formatting.
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
set_spec_sigfig <- function(spec, n, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@n_sigfig <- n
  spec
}

#' Set decimal places for OFV values
#'
#' `set_spec_ofv_decimals()` controls the number of decimal places for OFV and
#' dOFV values. Use `NA` to keep significant-figure formatting.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param n Non-negative integer or NA.
#' @param ... Not used.
#' @return Modified spec.
#' @seealso [get_spec_ofv_decimals()].
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_ofv_decimals(2)
set_spec_ofv_decimals <- function(spec, n, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@n_decimals_ofv <- n
  spec
}

#' Set hide_empty_columns for a spec
#'
#' `set_spec_hide_empty()` controls whether empty columns are automatically
#' hidden.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param hide Logical value.
#' @param ... Not used.
#' @return Modified spec.
#' @seealso [get_spec_hide_empty()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_hide_empty(FALSE)
set_spec_hide_empty <- function(spec, hide, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@hide_empty_columns <- hide
  spec
}

#' Set p-value formatting for a spec
#'
#' `set_spec_pvalue()` controls how p-values are displayed in the table.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Not used.
#' @param threshold Numeric threshold below which p-values display as "< threshold",
#'   or NULL to disable threshold display.
#' @param scientific Logical. If TRUE, use scientific notation for p-values.
#' @return Modified spec.
#' @seealso [get_spec_pvalue()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_pvalue(threshold = 0.001, scientific = TRUE)
set_spec_pvalue <- function(spec, ..., threshold, scientific) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
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
#' `set_spec_footnotes()` controls the order of footnote sections, or disables
#' footnotes entirely.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param order Character vector of footnote sections in desired order, or NULL
#'   to disable footnotes. For TableSpec: "summary_info", "equations", "abbreviations".
#'   For SummarySpec: only "abbreviations" is valid.
#' @param ... Not used.
#' @return Modified spec.
#' @seealso [get_spec_footnotes()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_footnotes(c("abbreviations", "equations"))
#'
#' # Disable footnotes
#' spec <- TableSpec() |>
#'   set_spec_footnotes(NULL)
set_spec_footnotes <- function(spec, order, ...) {
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  spec@footnote_order <- order
  spec
}

#' Set section filter for a spec
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `set_spec_section_filter()` was deprecated in hyperion.tables 0.5.0 and
#' will be removed in 0.6.0. Use [set_spec_sections()] with `keep` or
#' `exclude` instead. Calling with no filter arguments still clears the
#' existing filter for backward compatibility.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param exclude Character vector of section labels to drop, optionally
#'   including `NA` to also drop unmatched rows.
#' @param keep Character vector of section labels to keep (everything else
#'   is dropped), optionally including `NA` to also keep unmatched rows.
#' @return Modified spec.
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_sections(
#'     "base" %in% tags ~ "Base Models",
#'     TRUE ~ "Other"
#'   ) |>
#'   set_spec_section_filter(exclude = "Other")
set_spec_section_filter <- function(spec, ..., exclude = NULL, keep = NULL) {
  lifecycle::deprecate_warn(
    "0.5.0",
    "set_spec_section_filter()",
    "set_spec_sections()",
    details = paste0(
      "`set_spec_section_filter()` will be removed in hyperion.tables 0.6.0. ",
      "Use `set_spec_sections(keep = ...)` or ",
      "`set_spec_sections(exclude = ...)` instead."
    )
  )
  if (!S7::S7_inherits(spec, BaseSpec)) {
    rlang::abort("`spec` must be a <TableSpec> or <SummarySpec> object.")
  }
  # 0.4.0 accepted unnamed section labels as exclusions; fold them into
  # `exclude` so positional multi-label calls keep working.
  dots <- rlang::list2(...)
  if (length(dots) > 0) {
    exclude <- c(exclude, unlist(dots, use.names = FALSE))
  }
  if (is.null(exclude) && is.null(keep)) {
    return(set_spec_sections(spec, keep = character(0)))
  }
  set_spec_sections(spec, exclude = exclude, keep = keep)
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

#' Set section assignments for a spec
#'
#' @description
#' Controls how rows are grouped into sections and in what order. Rules
#' passed via `...` or `sections =` are formulas like
#' `kind == "THETA" ~ "Structural"`, evaluated via [dplyr::case_when()].
#'
#' Display ordering is also configured here. `order` is a character
#' vector of section labels giving the display order. Sections not
#' listed land after, in encounter order. To filter to just the sections
#' in `order`, pass `keep = order`.
#'
#' Defaults of `NULL` for `order`, `keep`, and `exclude` mean "leave alone."
#' To clear the order pass `order = character(0)`. To clear the current section
#' filter pass `keep = character(0)` or `exclude = character(0)`.
#'
#' TableSpec and SummarySpec methods take additional arguments — see the
#' method-specific sections below.
#'
#' @section TableSpec method:
#' `set_spec_sections(<TableSpec>)` accepts two extra arguments for assigning
#' parameters to sections, beyond the rules passed via `...`:
#'
#' - `parameters` — a named list keyed by section label, with character
#'   vectors of parameter names as values. E.g.
#'   `parameters = list("Covariate Parameters" = c("CAP-D1", "WT-V2/F"))`.
#'   `NULL` leaves assignments alone; `list()` clears them.
#' - `file` — path to a TOML where each entry can carry a `section = "..."`
#'   field, matched by parameter name. Read once and folded into assignments.
#'
#' If both are passed and conflict (same parameter, different sections), a
#' warning is emitted and the inline `parameters` value wins.
#'
#' @section SummarySpec method:
#' `set_spec_sections(<SummarySpec>)` accepts one extra argument for assigning
#' models to sections, beyond the rules passed via `...`:
#'
#' - `models` — a named list keyed by section label, with character vectors of
#'   model names as values. E.g.
#'   `models = list("Selected Models" = c("run001", "run002"))`. `NULL` leaves
#'   assignments alone; `list()` clears them.
#'
#' @param spec A TableSpec or SummarySpec object.
#' @param ... Section rule formulas (LHS condition ~ RHS label). Equivalent
#'   to passing them via `sections =`; kept for backward compatibility.
#' @param sections A list of section rule formulas, typically built with
#'   [section_rules()]. Combined with any rules passed via `...` (the
#'   `sections` arg comes first).
#' @param overwrite If `FALSE` (default), append to existing rules. If
#'   `TRUE`, replace them.
#' @param order Character vector of section labels in display order.
#' @param keep Character vector of section labels to keep (everything else is
#'   dropped), optionally including `NA` to also keep unmatched rows. Mutually
#'   exclusive with `exclude`. Pass `character(0)` to clear the current filter.
#' @param exclude Character vector of section labels to drop, optionally
#'   including `NA` to also drop unmatched rows. Mutually exclusive with `keep`.
#'   Pass `character(0)` to clear the current filter.
#' @return Modified spec.
#' @seealso [get_spec_sections()], [get_spec_parameter_sections()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_sections(
#'     kind == "THETA" ~ "Structural Parameters",
#'     kind == "OMEGA" ~ "Interindividual Variability",
#'     kind == "SIGMA" ~ "Residual Error",
#'     parameters = list(
#'       "Covariate Parameters" = c("CAP-D1", "WT-V2/F", "PSO-CL/F")
#'     ),
#'     order = c(
#'       "Structural Parameters",
#'       "Covariate Parameters",
#'       "Interindividual Variability",
#'       "Residual Error"
#'     )
#'   )
#'
#' summary_spec <- SummarySpec() |>
#'   set_spec_sections(
#'     "base" %in% tags ~ "Base Models",
#'     "key" %in% tags ~ "Key Models",
#'     models = list("Selected Models" = c("run001", "run002")),
#'     order = c("Base Models", "Key Models", "Selected Models")
#'   )
set_spec_sections <- S7::new_generic(
  "set_spec_sections",
  "spec",
  function(
    spec,
    ...,
    sections = NULL,
    overwrite = FALSE,
    order = NULL,
    keep = NULL,
    exclude = NULL
  ) {
    S7::S7_dispatch()
  }
)

#' @noRd
build_next_rules <- function(current_rules, sections_arg, dots, overwrite) {
  rule_dots <- capture_unnamed_dots(!!!dots)
  dot_rules <- section_rules(!!!rule_dots)
  all_new <- c(sections_arg %||% list(), dot_rules)
  if (overwrite) {
    all_new
  } else {
    c(current_rules, all_new)
  }
}

#' @noRd
drop_named_dots <- function(dots, names, alt = NULL) {
  dot_names <- rlang::names2(dots)
  dropped <- unique(dot_names[dot_names %in% names])
  if (length(dropped) > 0) {
    msg <- sprintf(
      "Ignoring unsupported named argument(s): %s",
      paste(dropped, collapse = ", ")
    )
    if (!is.null(alt)) {
      msg <- c(msg, i = alt)
    }
    rlang::warn(msg)
  }
  dots[!(dot_names %in% names)]
}

#' @noRd
with_section_options_error <- function(assignment_arg, expr) {
  tryCatch(
    force(expr),
    error = function(err) {
      msg <- conditionMessage(err)
      friendly <- if (grepl("@order", msg, fixed = TRUE)) {
        sub("^.*@order ", "Invalid `order`: ", msg)
      } else if (grepl("@filter$keep", msg, fixed = TRUE)) {
        sub("^.*@filter\\$keep ", "Invalid `keep`: ", msg)
      } else if (grepl("@filter$exclude", msg, fixed = TRUE)) {
        sub("^.*@filter\\$exclude ", "Invalid `exclude`: ", msg)
      } else if (grepl("@assignments", msg, fixed = TRUE)) {
        translate_assignments_error(msg, assignment_arg)
      } else {
        NULL
      }
      if (is.null(friendly)) {
        stop(err)
      }
      rlang::abort(friendly, call = rlang::call2("set_spec_sections"))
    }
  )
}

#' @noRd
translate_assignments_error <- function(msg, arg_name) {
  if (grepl("multiple sections", msg, fixed = TRUE)) {
    items <- sub(".*multiple sections: ", "", msg)
    sprintf(
      "Invalid `%s`: lists the same item under multiple sections: %s",
      arg_name,
      items
    )
  } else if (grepl("duplicate section labels", msg, fixed = TRUE)) {
    sprintf("Invalid `%s`: has duplicate section labels.", arg_name)
  } else if (grepl("non-empty, non-NA character vectors", msg, fixed = TRUE)) {
    sprintf(
      "Invalid `%s`: values must be non-empty, non-NA character vectors.",
      arg_name
    )
  } else {
    sprintf(
      "Invalid `%s`: must be a named list keyed by section label, e.g. `%s = list(\"Section A\" = c(\"item1\", \"item2\"))`.",
      arg_name,
      arg_name
    )
  }
}

#' Resolve order/keep/exclude with "leave alone" semantics
#'
#' Reads NULL args as "preserve current". Passing both `keep` and `exclude`
#' is an error. Passing one (with non-empty value) sets that mode; passing
#' empty character clears the filter.
#' @noRd
resolve_section_meta <- function(current, order, keep, exclude) {
  if (!is.null(keep) && !is.null(exclude)) {
    rlang::abort(
      "`keep` and `exclude` are mutually exclusive; pass at most one.",
      call = rlang::call2("set_spec_sections")
    )
  }
  next_order <- if (is.null(order)) current@order else order
  next_filter <- current@filter
  if (!is.null(keep)) {
    keep <- normalize_filter_labels(keep)
    next_filter <- if (length(keep) == 0L) list() else list(keep = keep)
  } else if (!is.null(exclude)) {
    exclude <- normalize_filter_labels(exclude)
    next_filter <- if (length(exclude) == 0L) {
      list()
    } else {
      list(exclude = exclude)
    }
  }
  list(order = next_order, filter = next_filter)
}

#' Coerce bare logical NA to character NA so `exclude = NA` is accepted
#' @noRd
normalize_filter_labels <- function(v) {
  if (is.logical(v) && all(is.na(v))) rep(NA_character_, length(v)) else v
}

#' Apply resolved meta onto a freshly-constructed SectionOptions
#' @noRd
apply_section_meta <- function(sections, meta) {
  sections@order <- meta$order
  sections@filter <- meta$filter
  sections
}

S7::method(set_spec_sections, SummarySpec) <- function(
  spec,
  ...,
  sections = NULL,
  overwrite = FALSE,
  models = NULL,
  order = NULL,
  keep = NULL,
  exclude = NULL
) {
  dots <- drop_named_dots(
    rlang::enquos(...),
    c("parameters", "file"),
    alt = "Did you mean `models`? `parameters`/`file` apply to parameter tables."
  )
  current <- spec@sections
  meta <- resolve_section_meta(current, order, keep, exclude)
  with_section_options_error("models", {
    next_sections <- SectionOptions(
      rules = build_next_rules(current@rules, sections, dots, overwrite),
      assignments = current@assignments,
      order = NULL,
      filter = list()
    )
    if (!is.null(models)) {
      next_sections@assignments <- merge_section_assignments(
        next_sections@assignments,
        models
      )
    }
    next_sections <- apply_section_meta(next_sections, meta)
  })
  spec@sections <- next_sections
  spec
}

S7::method(set_spec_sections, TableSpec) <- function(
  spec,
  ...,
  sections = NULL,
  overwrite = FALSE,
  parameters = NULL,
  file = NULL,
  order = NULL,
  keep = NULL,
  exclude = NULL
) {
  dots <- drop_named_dots(
    rlang::enquos(...),
    "models",
    alt = "Did you mean `parameters`? `models` applies to summary tables."
  )
  current <- spec@sections
  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1L) {
      rlang::abort("`file` must be a single character path or NULL.")
    }
    if (is.na(file)) {
      rlang::abort(
        "`file` must be a path; pass `parameters = list()` to clear assignments."
      )
    }
  }
  meta <- resolve_section_meta(current, order, keep, exclude)

  with_section_options_error("parameters", {
    next_sections <- SectionOptions(
      rules = build_next_rules(current@rules, sections, dots, overwrite),
      assignments = current@assignments,
      order = NULL,
      filter = list()
    )
    # Preserve inline items accumulated by prior calls so a later
    # set_spec_sections() does not reset the unmatched-override tracking.
    next_sections@inline_items <- current@inline_items

    if (!is.null(file)) {
      path <- normalizePath(file, mustWork = TRUE)
      file_assign <- toml_lookup_to_assignments(read_lookup_toml(path))
      if (length(file_assign) > 0L) {
        next_sections@assignments <- merge_section_assignments(
          next_sections@assignments,
          file_assign
        )
      }
    }

    if (!is.null(parameters)) {
      next_sections@assignments <- merge_section_assignments(
        next_sections@assignments,
        parameters,
        warn_on_conflict = TRUE
      )
      if (length(parameters) == 0L) {
        next_sections@inline_items <- character(0)
      } else if (is_valid_assignments_input(parameters)) {
        next_sections@inline_items <- unique(c(
          next_sections@inline_items,
          unlist(parameters, use.names = FALSE)
        ))
      }
    }

    next_sections <- apply_section_meta(next_sections, meta)
  })

  spec@sections <- next_sections
  spec
}

# ==============================================================================
# TableSpec-Only Setters
# ==============================================================================

#' Set parameter name options for a TableSpec
#'
#' `set_spec_parameter_names()` controls how parameter names are displayed in
#' the table. Operates on `TableSpec` only.
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
set_spec_parameter_names <- function(spec, ..., source = NULL) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
  opts <- spec@parameter_names
  if (!is.null(source)) {
    opts@source <- source
  }
  spec@parameter_names <- opts
  spec
}

#' Set CI options for a TableSpec
#'
#' `set_spec_ci()` controls confidence interval rendering options. Operates on
#' `TableSpec` only.
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
set_spec_ci <- function(
  spec,
  ...,
  level = NULL,
  merge = NULL,
  pattern = NULL,
  missing_text = NULL
) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
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
#' `set_spec_missing()` controls how NA values are displayed in the table.
#' Operates on `TableSpec` only.
#'
#' @param spec A TableSpec object.
#' @param ... Not used.
#' @param text Text to substitute for NA values.
#' @param apply_to Which columns to apply missing text to: "all", "numeric",
#'   or "character".
#' @return Modified spec.
#' @seealso [get_spec_missing()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_missing("-", apply_to = "numeric")
set_spec_missing <- function(spec, text = NULL, ..., apply_to = NULL) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
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
#' `set_spec_transforms()` controls which transforms are applied for display by
#' parameter kind. Operates on `TableSpec` only.
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
set_spec_transforms <- function(
  spec,
  ...,
  theta = NULL,
  omega = NULL,
  sigma = NULL
) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
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
#' `set_spec_filter()` controls which parameters appear in the output table.
#' Pass filter expressions like `!fixed`, `diagonal`. Operates on `TableSpec`
#' only.
#'
#' @param spec A TableSpec object.
#' @param ... Filter expressions evaluated against parameter rows (e.g.
#'   `!fixed`, `diagonal`). Captured as quosures and combined via
#'   [filter_rules()].
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec.
#' @seealso [get_spec_filter()].
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_filter(!fixed, diagonal)
set_spec_filter <- function(spec, ..., overwrite = FALSE) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
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
#' `set_spec_variability()` controls how the variability display column is
#' constructed. Operates on `TableSpec` only.
#'
#' @param spec A TableSpec object.
#' @param ... Rule formulas (LHS condition ~ RHS expression) defining how
#'   the variability column is constructed. Combined via [variability_rules()].
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
set_spec_variability <- function(spec, ..., overwrite = FALSE) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("`spec` must be a <TableSpec> object.")
  }
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
#' `set_spec_time_format()` controls how time columns are formatted. Operates
#' on `SummarySpec` only.
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
set_spec_time_format <- function(spec, format, ...) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  spec@time_format <- format
  spec
}

#' Set models to include for a SummarySpec
#'
#' `set_spec_models()` filters which models appear in the summary table by
#' name. Operates on `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param models Character vector of model names, or NULL for all models.
#' @param ... Not used.
#' @return Modified spec.
#' @seealso [get_spec_models()].
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_models(c("run001", "run002", "run003"))
set_spec_models <- function(spec, models, ...) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  spec@models_to_include <- models
  spec
}

#' Set tag filter for a SummarySpec
#'
#' `set_spec_tag_filter()` filters which models appear in the summary table by
#' tags. Operates on `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param ... Not used.
#' @param include Character vector of tags to include, or NULL for no inclusion
#'   filter. Only models with at least one matching tag are kept.
#' @param exclude Character vector of tags to exclude, or NULL for no exclusion
#'   filter. Models with any matching tag are removed. Applied after `include`.
#' @return Modified spec.
#' @seealso [get_spec_tag_filter()].
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_tag_filter(include = c("final", "approved"))
#'
#' # Exclude models tagged "failed"
#' spec <- SummarySpec() |>
#'   set_spec_tag_filter(exclude = "failed")
set_spec_tag_filter <- function(spec, ..., include, exclude) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  if (!missing(include)) {
    spec@tag_filter <- include
  }
  if (!missing(exclude)) {
    spec@tag_exclude <- exclude
  }
  spec
}

#' Set remove_unrun_models for a SummarySpec
#'
#' `set_spec_remove_unrun()` controls whether models without completed runs
#' are excluded. Operates on `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param remove Logical value.
#' @param ... Not used.
#' @return Modified spec.
#' @seealso [get_spec_remove_unrun()].
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_remove_unrun(FALSE)
set_spec_remove_unrun <- function(spec, remove, ...) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  spec@remove_unrun_models <- remove
  spec
}

#' Set summary filter rules for a SummarySpec
#'
#' `set_spec_summary_filter()` controls which models appear in the summary
#' table based on filter expressions evaluated against summary columns.
#' Operates on `SummarySpec` only.
#'
#' @param spec A SummarySpec object.
#' @param ... Filter expressions evaluated against summary columns (e.g.
#'   `ofv < 1000`). Captured as quosures and combined via
#'   [summary_filter_rules()].
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec.
#' @seealso [get_spec_summary_filter()].
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_summary_filter(ofv < 1000)
set_spec_summary_filter <- function(spec, ..., overwrite = FALSE) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("`spec` must be a <SummarySpec> object.")
  }
  rule_dots <- capture_unnamed_dots(...)
  new_rules <- summary_filter_rules(!!!rule_dots)
  if (overwrite) {
    spec@summary_filter <- new_rules
  } else {
    spec@summary_filter <- c(spec@summary_filter, new_rules)
  }
  spec
}
