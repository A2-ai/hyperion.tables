# ==============================================================================
# Spec Modifier Functions
# ==============================================================================
# Pipe-friendly functions for modifying TableSpec and SummarySpec objects.
# All functions return a new copy of the spec (immutable).

# ==============================================================================
# Internal Helper: Clone a spec with modified properties
# ==============================================================================

#' Clone a spec with modified properties
#'
#' Creates a new spec object with the same properties as the original,
#' except for those specified in `...`. This enables immutable modifications.
#' Uses R's copy-on-modify semantics.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Named arguments for properties to modify
#' @return A new spec object of the same type
#' @noRd
clone_spec <- function(spec, ...) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }

  mods <- list(...)
  new_spec <- spec

  for (nm in names(mods)) {
    S7::prop(new_spec, nm) <- mods[[nm]]
  }

  # SummarySpec merges add_columns into columns during construction.
  # We need to replicate that behavior when add_columns changes.
  if (
    S7::S7_inherits(new_spec, SummarySpec) && "add_columns" %in% names(mods)
  ) {
    new_spec@columns <- merge_summary_columns(
      new_spec@columns,
      new_spec@add_columns
    )
  }

  new_spec
}

# ==============================================================================
# Column Operations (Both Specs)
# ==============================================================================

#' Add columns to a spec
#'
#' Appends columns to the spec's add_columns list. These columns will be
#' added to the default column set.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Column names to add (unquoted or character strings)
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   add_spec_columns("shrinkage", "cv")
#'
#' sum_spec <- SummarySpec() |>
#'   add_spec_columns("estimation_time")
add_spec_columns <- function(spec, ...) {
  cols <- c(...)

  if (S7::S7_inherits(spec, TableSpec)) {
    valid <- c(valid_table_columns(), "ci", "pct_change")
  } else if (S7::S7_inherits(spec, SummarySpec)) {
    valid <- valid_summary_columns()
  } else {
    stop("spec must be a TableSpec or SummarySpec object")
  }

  invalid <- setdiff(cols, valid)
  if (length(invalid) > 0) {
    stop("Invalid columns: ", paste(invalid, collapse = ", "))
  }

  new_add <- unique(c(spec@add_columns, cols))
  clone_spec(spec, add_columns = new_add)
}

#' Drop columns from a spec
#'
#' Adds columns to the spec's drop_columns list. These columns will be
#' excluded from the output table.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Column names to drop (unquoted or character strings)
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   drop_spec_columns("unit", "symbol")
#'
#' sum_spec <- SummarySpec() |>
#'   drop_spec_columns("description")
drop_spec_columns <- function(spec, ...) {
  cols <- c(...)

  if (S7::S7_inherits(spec, TableSpec)) {
    # TableSpec allows comparison suffixes
    comparison_cols <- comparison_suffix_columns()
    comparison_drop_cols <- c(
      paste0(comparison_cols, "_1"),
      paste0(comparison_cols, "_2"),
      paste0(comparison_cols, "_left"),
      paste0(comparison_cols, "_right")
    )
    ci_aliases <- c("ci", "ci_1", "ci_2", "ci_left", "ci_right")
    valid <- c(
      valid_table_columns(),
      comparison_drop_cols,
      "pct_change",
      ci_aliases
    )

    # Also allow numeric suffixes via pattern
    comparison_pattern <- paste0(
      "^(",
      paste(comparison_cols, collapse = "|"),
      ")_\\d+$"
    )
    ci_num_pattern <- "^ci_\\d+$"
    pct_change_pattern <- "^pct_change_\\d+$"

    is_valid <- function(col) {
      col %in%
        valid ||
        grepl(comparison_pattern, col) ||
        grepl(ci_num_pattern, col) ||
        grepl(pct_change_pattern, col)
    }

    invalid <- cols[!vapply(cols, is_valid, logical(1))]
  } else if (S7::S7_inherits(spec, SummarySpec)) {
    valid <- valid_summary_columns()
    invalid <- setdiff(cols, valid)
  } else {
    stop("spec must be a TableSpec or SummarySpec object")
  }

  if (length(invalid) > 0) {
    stop("Invalid columns: ", paste(invalid, collapse = ", "))
  }

  new_drop <- unique(c(spec@drop_columns, cols))
  clone_spec(spec, drop_columns = new_drop)
}

#' Set columns for a spec
#'
#' Replaces the spec's columns list entirely. This overrides the default
#' column set.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param ... Column names to include (unquoted or character strings)
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_columns("name", "estimate", "rse")
set_spec_columns <- function(spec, ...) {
  cols <- c(...)

  if (S7::S7_inherits(spec, TableSpec)) {
    valid <- c(valid_table_columns(), "ci", "pct_change")
  } else if (S7::S7_inherits(spec, SummarySpec)) {
    valid <- valid_summary_columns()
  } else {
    stop("spec must be a TableSpec or SummarySpec object")
  }

  invalid <- setdiff(cols, valid)
  if (length(invalid) > 0) {
    stop("Invalid columns: ", paste(invalid, collapse = ", "))
  }

  clone_spec(spec, columns = cols)
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
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_title("Parameter Estimates")
set_spec_title <- function(spec, title) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }
  if (!is.character(title) || length(title) != 1) {
    stop("title must be a single character string")
  }
  clone_spec(spec, title = title)
}

#' Set significant figures for a spec
#'
#' Sets the number of significant figures for numeric formatting.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param n Positive integer for significant figures
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_sigfig(4)
set_spec_sigfig <- function(spec, n) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }
  if (!is.numeric(n) || length(n) != 1 || n < 1 || n != floor(n)) {
    stop("n must be a positive whole number")
  }

  clone_spec(spec, n_sigfig = n)
}

#' Set hide_empty_columns for a spec
#'
#' Controls whether empty columns are automatically hidden.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param hide Logical value
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_hide_empty(FALSE)
set_spec_hide_empty <- function(spec, hide) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }

  if (!is.logical(hide) || length(hide) != 1 || is.na(hide)) {
    stop("hide must be TRUE or FALSE")
  }
  clone_spec(spec, hide_empty_columns = hide)
}

#' Set p-value formatting for a spec
#'
#' Controls how p-values are displayed in the table.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param threshold Numeric threshold below which p-values display as "< threshold",
#'   or NULL to disable threshold display
#' @param scientific Logical. If TRUE, use scientific notation for p-values
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_pvalue(threshold = 0.001, scientific = TRUE)
set_spec_pvalue <- function(spec, threshold = NULL, scientific = NULL) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }

  mods <- list()

  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1) {
      stop("threshold must be a single numeric value or NULL")
    }
    mods$pvalue_threshold <- threshold
  }

  if (!is.null(scientific)) {
    if (
      !is.logical(scientific) || length(scientific) != 1 || is.na(scientific)
    ) {
      stop("scientific must be TRUE or FALSE")
    }
    mods$pvalue_scientific <- scientific
  }

  if (length(mods) == 0) {
    return(spec)
  }

  do.call(clone_spec, c(list(spec = spec), mods))
}

#' Set footnote order for a spec
#'
#' Controls the order of footnote sections, or disables footnotes entirely.
#'
#' @param spec A TableSpec or SummarySpec object
#' @param order Character vector of footnote sections in desired order, or NULL
#'   to disable footnotes. For TableSpec: "summary_info", "equations", "abbreviations".
#'   For SummarySpec: only "abbreviations" is valid.
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_footnotes(c("abbreviations", "equations"))
#'
#' # Disable footnotes
#' spec <- TableSpec() |>
#'   set_spec_footnotes(NULL)
set_spec_footnotes <- function(spec, order) {
  if (
    !S7::S7_inherits(spec, TableSpec) && !S7::S7_inherits(spec, SummarySpec)
  ) {
    stop("spec must be a TableSpec or SummarySpec object")
  }

  if (!is.null(order)) {
    if (!is.character(order)) {
      stop("order must be a character vector or NULL")
    }

    if (S7::S7_inherits(spec, TableSpec)) {
      valid <- c("summary_info", "equations", "abbreviations")
    } else {
      valid <- "abbreviations"
    }

    invalid <- setdiff(order, valid)
    if (length(invalid) > 0) {
      stop("Invalid footnote sections: ", paste(invalid, collapse = ", "))
    }
  }

  clone_spec(spec, footnote_order = order)
}

# ==============================================================================
# TableSpec-Only Setters
# ==============================================================================

#' Set name source for a TableSpec
#'
#' Controls which name field is used from ModelComments.
#'
#' @param spec A TableSpec object
#' @param source One of "name", "display", or "nonmem_name"
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_name_source("nonmem_name")
set_spec_name_source <- function(spec, source) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }
  valid <- c("name", "display", "nonmem_name")
  if (!source %in% valid) {
    stop("source must be one of: ", paste(valid, collapse = ", "))
  }
  clone_spec(spec, name_source = source)
}

#' Set CI options for a TableSpec
#'
#' Controls confidence interval rendering options.
#'
#' @param spec A TableSpec object
#' @param level Confidence interval level (0-1, exclusive)
#' @param merge Logical. If TRUE, merge CI bounds into a single column
#' @param pattern sprintf pattern for merged CI display (must contain two %%s)
#' @param missing_text Text to show for missing CI values
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_ci(level = 0.90, merge = TRUE, pattern = "(%s, %s)")
set_spec_ci <- function(
  spec,
  level = NULL,
  merge = NULL,
  pattern = NULL,
  missing_text = NULL
) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }

  # Start with current CI options
  ci_args <- list(
    level = spec@ci@level,
    merge = spec@ci@merge,
    pattern = spec@ci@pattern,
    missing_text = spec@ci@missing_text
  )

  # Apply modifications

  if (!is.null(level)) ci_args$level <- level
  if (!is.null(merge)) ci_args$merge <- merge
  if (!is.null(pattern)) ci_args$pattern <- pattern
  if (!is.null(missing_text)) ci_args$missing_text <- missing_text

  new_ci <- do.call(CIOptions, ci_args)
  clone_spec(spec, ci = new_ci)
}

#' Set missing value handling for a TableSpec
#'
#' Controls how NA values are displayed in the table.
#'
#' @param spec A TableSpec object
#' @param text Text to substitute for NA values
#' @param apply_to Which columns to apply missing text to: "all", "numeric",
#'   or "character"
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_missing("-", apply_to = "numeric")
set_spec_missing <- function(spec, text = NULL, apply_to = NULL) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }

  mods <- list()

  if (!is.null(text)) {
    if (!is.character(text) || length(text) != 1) {
      stop("text must be a single character string")
    }
    mods$missing_text <- text
  }

  if (!is.null(apply_to)) {
    valid <- c("all", "numeric", "character")
    if (!apply_to %in% valid) {
      stop("apply_to must be one of: ", paste(valid, collapse = ", "))
    }
    mods$missing_apply_to <- apply_to
  }

  if (length(mods) == 0) {
    return(spec)
  }

  do.call(clone_spec, c(list(spec = spec), mods))
}

#' Set display transforms for a TableSpec
#'
#' Controls which transforms are applied for display by parameter kind.
#'
#' @param spec A TableSpec object
#' @param theta Columns to transform for theta parameters
#' @param omega Columns to transform for omega parameters
#' @param sigma Columns to transform for sigma parameters
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_transforms(theta = "all", omega = c("estimate", "cv"))
set_spec_transforms <- function(
  spec,
  theta = NULL,
  omega = NULL,
  sigma = NULL
) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }

  valid_cols <- c(
    "all",
    "estimate",
    "cv",
    "rse",
    "ci",
    "ci_low",
    "ci_high",
    "symbol"
  )
  transforms <- spec@display_transforms

  if (!is.null(theta)) {
    invalid <- setdiff(theta, valid_cols)
    if (length(invalid) > 0) {
      stop(
        "Invalid transform columns for theta: ",
        paste(invalid, collapse = ", ")
      )
    }
    transforms$theta <- theta
  }

  if (!is.null(omega)) {
    invalid <- setdiff(omega, valid_cols)
    if (length(invalid) > 0) {
      stop(
        "Invalid transform columns for omega: ",
        paste(invalid, collapse = ", ")
      )
    }
    transforms$omega <- omega
  }

  if (!is.null(sigma)) {
    invalid <- setdiff(sigma, valid_cols)
    if (length(invalid) > 0) {
      stop(
        "Invalid transform columns for sigma: ",
        paste(invalid, collapse = ", ")
      )
    }
    transforms$sigma <- sigma
  }

  clone_spec(spec, display_transforms = transforms)
}

# ==============================================================================
# TableSpec Rule Modifiers
# ==============================================================================

#' Set section rules for a TableSpec
#'
#' Controls how parameters are grouped into sections. Pass formula expressions
#' like `kind == "THETA" ~ "Structural Parameters"`.
#'
#' @param spec A TableSpec object
#' @param ... Section rule formulas
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_sections(
#'     kind == "THETA" ~ "Structural",
#'     kind == "OMEGA" ~ "IIV"
#'   )
set_spec_sections <- function(spec, ..., overwrite = FALSE) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }

  new_rules <- section_rules(...)

  if (overwrite) {
    final_rules <- new_rules
  } else {
    final_rules <- c(spec@sections, new_rules)
  }

  clone_spec(spec, sections = final_rules)
}

#' Set row filter rules for a TableSpec
#'
#' Controls which parameters appear in the output table. Pass filter
#' expressions like `!fixed`, `diagonal`.
#'
#' @param spec A TableSpec object
#' @param ... Filter rule expressions
#' @param overwrite If FALSE (default), append to existing rules.
#'   If TRUE, replace all existing rules.
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_filter(!fixed, diagonal)
set_spec_filter <- function(spec, ..., overwrite = FALSE) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }

  new_rules <- filter_rules(...)

  if (overwrite) {
    final_rules <- new_rules
  } else {
    final_rules <- c(spec@row_filter, new_rules)
  }

  clone_spec(spec, row_filter = final_rules)
}

#' Set variability rules for a TableSpec
#'
#' Controls how the variability display column is constructed.
#' Always replaces existing rules.
#'
#' @param spec A TableSpec object
#' @param ... Variability rule formulas
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- TableSpec() |>
#'   set_spec_variability(
#'     fixed ~ "(Fixed)",
#'     !is.na(cv) ~ sprintf("CV = %s%%", cv),
#'     TRUE ~ NA_character_
#'   )
set_spec_variability <- function(spec, ...) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }

  new_rules <- variability_rules(...)
  clone_spec(spec, variability_rules = new_rules)
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
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_time_format("minutes")
set_spec_time_format <- function(spec, format) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    stop("spec must be a SummarySpec object")
  }
  valid <- c("seconds", "minutes", "hours", "auto")
  if (!format %in% valid) {
    stop("format must be one of: ", paste(valid, collapse = ", "))
  }
  clone_spec(spec, time_format = format)
}

#' Set models to include for a SummarySpec
#'
#' Filters which models appear in the summary table by name.
#'
#' @param spec A SummarySpec object
#' @param models Character vector of model names, or NULL for all models
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_models(c("run001", "run002", "run003"))
set_spec_models <- function(spec, models) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    stop("spec must be a SummarySpec object")
  }
  if (!is.null(models) && !is.character(models)) {
    stop("models must be a character vector or NULL")
  }
  clone_spec(spec, models_to_include = models)
}

#' Set tag filter for a SummarySpec
#'
#' Filters which models appear in the summary table by tags.
#'
#' @param spec A SummarySpec object
#' @param tags Character vector of tags to filter by, or NULL for no filtering
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_tag_filter(c("final", "approved"))
set_spec_tag_filter <- function(spec, tags) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    stop("spec must be a SummarySpec object")
  }
  if (!is.null(tags) && !is.character(tags)) {
    stop("tags must be a character vector or NULL")
  }
  clone_spec(spec, tag_filter = tags)
}

#' Set remove_unrun_models for a SummarySpec
#'
#' Controls whether models without completed runs are excluded.
#'
#' @param spec A SummarySpec object
#' @param remove Logical value
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_remove_unrun(FALSE)
set_spec_remove_unrun <- function(spec, remove) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    stop("spec must be a SummarySpec object")
  }
  if (!is.logical(remove) || length(remove) != 1 || is.na(remove)) {
    stop("remove must be TRUE or FALSE")
  }
  clone_spec(spec, remove_unrun_models = remove)
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
#' @return Modified spec (copy)
#' @export
#' @examples
#' spec <- SummarySpec() |>
#'   set_spec_summary_filter(ofv < 1000)
set_spec_summary_filter <- function(spec, ..., overwrite = FALSE) {
  if (!S7::S7_inherits(spec, SummarySpec)) {
    stop("spec must be a SummarySpec object")
  }

  new_rules <- summary_filter_rules(...)

  if (overwrite) {
    final_rules <- new_rules
  } else {
    final_rules <- c(spec@summary_filter, new_rules)
  }

  clone_spec(spec, summary_filter = final_rules)
}
