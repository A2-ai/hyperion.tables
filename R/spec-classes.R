#' @include spec-options.R
NULL

# ==============================================================================
# BaseSpec S7 Class - abstract parent for TableSpec and SummarySpec
# ==============================================================================

#' Abstract parent class for spec types
#'
#' `BaseSpec` is an abstract S7 class that serves as the common parent for
#' [TableSpec] and [SummarySpec]. It cannot be instantiated directly. Common
#' properties, validators, and method dispatches that apply to both spec types
#' are centralized here so the child classes carry only their class-specific
#' configuration.
#'
#' @noRd
BaseSpec <- S7::new_class(
  "BaseSpec",
  abstract = TRUE,
  properties = list(
    title = S7::new_property(
      class = S7::class_character,
      default = ""
    ),
    sections = S7::new_property(
      class = SectionOptions,
      default = SectionOptions()
    ),
    columns = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    add_columns = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    drop_columns = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    allowed_footnotes = S7::new_property(
      class = S7::class_character,
      getter = function(self) character(0)
    ),
    hide_empty_columns = S7::new_property(
      class = S7::class_logical,
      default = TRUE
    ),
    n_sigfig = S7::new_property(
      class = S7::class_numeric,
      default = 3
    ),
    n_decimals_ofv = S7::new_property(
      class = S7::class_numeric,
      default = 3
    ),
    pvalue_scientific = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),
    pvalue_threshold = S7::new_property(
      class = S7::class_numeric | NULL,
      default = NULL
    ),
    footnote_order = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    )
  ),
  validator = function(self) {
    if (
      length(self@n_sigfig) != 1 ||
        self@n_sigfig < 1 ||
        self@n_sigfig != floor(self@n_sigfig)
    ) {
      return(sprintf(
        "@n_sigfig must be a positive whole number. Got: %s",
        self@n_sigfig
      ))
    }

    ofv_msg <- validate_ofv_decimals(self@n_decimals_ofv)
    if (!is.null(ofv_msg)) {
      return(ofv_msg)
    }

    if (
      length(self@hide_empty_columns) != 1 || is.na(self@hide_empty_columns)
    ) {
      return(sprintf(
        "@hide_empty_columns must be TRUE or FALSE. Got: %s",
        self@hide_empty_columns
      ))
    }

    if (length(self@pvalue_scientific) != 1 || is.na(self@pvalue_scientific)) {
      return(sprintf(
        "@pvalue_scientific must be TRUE or FALSE. Got: %s",
        self@pvalue_scientific
      ))
    }

    pvalue_msg <- validate_pvalue_threshold(self@pvalue_threshold)
    if (!is.null(pvalue_msg)) {
      return(pvalue_msg)
    }

    for (slot in c("columns", "add_columns", "drop_columns")) {
      value <- S7::prop(self, slot)
      if (!is.null(value) && !is.character(value)) {
        return(sprintf(
          "@%s must be NULL or a character vector. Got: %s",
          slot,
          class(value)[1]
        ))
      }
    }

    if (!is.null(self@footnote_order)) {
      if (length(self@footnote_order) == 0) {
        return("@footnote_order must be NULL or have at least one section")
      }
      bad <- setdiff(self@footnote_order, self@allowed_footnotes)
      if (length(bad) > 0) {
        return(sprintf(
          "@footnote_order must be in: %s\n  Got: %s",
          paste(self@allowed_footnotes, collapse = ", "),
          paste(bad, collapse = ", ")
        ))
      }
    }
  }
)

# ==============================================================================
# TableSpec S7 Class
# ==============================================================================

#' Default variability rules for TableSpec
#' @noRd
default_variability_rules <- function() {
  variability_rules(
    fixed ~ "(Fixed)",
    !is.na(corr) ~ sprintf("(Corr = %s)", corr),
    !is.na(cv) & cv != 0 ~ sprintf("(CV = %s%%)", cv),
    !is.na(sd) ~ sprintf("(SD = %s)", sd),
    TRUE ~ NA_character_
  )
}

#' Table specification for parameter tables
#'
#' @param title Character. Title for the parameter table header. Default is
#'   "Model Parameters".
#' @param parameter_names ParameterNameOptions object controlling how parameter names
#'   are displayed. Controls which name field to use ("name", "display", or "nonmem").
#'   Defaults to `ParameterNameOptions()`.
#' @param sections A `SectionOptions` object, or `NULL` for an empty section
#'   configuration. Prefer configuring sections with [set_spec_sections()] in
#'   a pipe for user-facing code.
#' @param columns Character vector of columns to include in output.
#' @param add_columns Character vector of columns to append to the column list.
#'   Useful for comparisons when you want to add columns like "pct_change"
#'   without overriding `columns`.
#' @param drop_columns Character vector of columns to exclude from output, or
#'   NULL (default) to include all columns.
#' @param hide_empty_columns Logical. If TRUE, columns that are all NA/empty
#'   are automatically hidden unless explicitly requested via `columns` or
#'   `add_columns`. Default is TRUE.
#' @param row_filter Filter rules created with `filter_rules()`.
#' @param display_transforms Named list specifying which transforms to apply
#'   for display. Names are parameter kinds (theta, omega, sigma), values are
#'   which columns to transform ("all", "estimate", "cv", "rse", "ci", "symbol").
#' @param variability_rules Rules created with `variability_rules()` to define
#'   the variability display column. Default uses built-in formatting.
#' @param n_sigfig Number of significant figures for numeric formatting in the
#'   output table. Must be a positive integer. Default is 3.
#' @param n_decimals_ofv Number of decimal places for OFV values in summary
#'   footnotes. Use NA to keep significant-figure formatting. Default is 3
#' @param pvalue_scientific Logical. If TRUE, p-values are formatted
#'   in scientific notation. If FALSE (default), uses significant figures from n_sigfig.
#' @param pvalue_threshold Numeric or NULL. If set, p-values below this threshold
#'   are displayed as "< threshold" (e.g., "< 0.05"). Default is NULL (no threshold).
#' @param ci CIOptions object controlling CI merge behavior and missing-value
#'   display. Defaults to `CIOptions()`.
#' @param missing_text Text to substitute for NA values after formatting. Default is "".
#' @param missing_apply_to Which columns to apply missing text to: "all", "numeric", or "character".
#'   Default is "all".
#' @param footnote_order Character vector controlling the order of footnote sections,
#'   or NULL to disable footnotes. Valid values: "summary_info", "equations",
#'   "abbreviations". Default is c("summary_info", "equations", "abbreviations").
#'
#' @export
TableSpec <- S7::new_class(
  "TableSpec",
  parent = BaseSpec,
  properties = list(
    parameter_names = S7::new_property(
      class = ParameterNameOptions,
      default = ParameterNameOptions()
    ),
    row_filter = S7::new_property(
      class = S7::class_list,
      default = list()
    ),
    display_transforms = S7::new_property(
      class = S7::class_list,
      default = list()
    ),
    variability_rules = S7::new_property(
      class = S7::class_list | NULL,
      default = NULL
    ),
    ci = S7::new_property(
      class = CIOptions,
      default = CIOptions()
    ),
    missing_text = S7::new_property(
      class = S7::class_character,
      default = ""
    ),
    missing_apply_to = S7::new_property(
      class = S7::class_character,
      default = "all"
    ),
    default_columns = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        c(
          "name",
          "symbol",
          "unit",
          "estimate",
          "variability",
          "ci_low",
          "ci_high",
          "rse",
          "shrinkage"
        )
      }
    ),
    allowed_footnotes = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        c("summary_info", "equations", "abbreviations")
      }
    )
  ),
  validator = function(self) {
    valid_kinds <- c("theta", "omega", "sigma")
    valid_transform_cols <- c(
      "all",
      "estimate",
      "cv",
      "rse",
      "ci",
      "symbol"
    )
    dt <- self@display_transforms
    if (!all(names(dt) %in% valid_kinds)) {
      bad <- setdiff(names(dt), valid_kinds)
      return(sprintf(
        "@display_transforms names must be in: %s\n  Got: %s",
        paste(valid_kinds, collapse = ", "),
        paste(bad, collapse = ", ")
      ))
    }

    col_values <- unlist(dt)
    if (length(col_values) > 0 && !all(col_values %in% valid_transform_cols)) {
      bad <- setdiff(col_values, valid_transform_cols)
      return(sprintf(
        "@display_transforms values must be in: %s\n  Got: %s",
        paste(valid_transform_cols, collapse = ", "),
        paste(bad, collapse = ", ")
      ))
    }

    if (
      length(self@variability_rules) > 0 &&
        !all(vapply(self@variability_rules, rlang::is_formula, logical(1)))
    ) {
      return("@variability_rules must be created with variability_rules()")
    }

    if (
      length(self@row_filter) > 0 &&
        !all(vapply(self@row_filter, rlang::is_quosure, logical(1)))
    ) {
      return("@row_filter rules must be created with filter_rules()")
    }

    valid_columns <- table_spec_valid_columns()
    columns_msg <- validate_columns_in_set(
      self@columns,
      valid_columns,
      "@columns"
    )
    if (!is.null(columns_msg)) {
      return(columns_msg)
    }

    if (!is.null(self@add_columns)) {
      if (!is.character(self@add_columns)) {
        return(sprintf(
          "@add_columns must be NULL or a character vector. Got: %s",
          class(self@add_columns)[1]
        ))
      }
      add_msg <- validate_columns_in_set(
        self@add_columns,
        valid_columns,
        "@add_columns"
      )
      if (!is.null(add_msg)) {
        return(add_msg)
      }
    }
    drop_msg <- validate_table_drop_columns(self@drop_columns)
    if (!is.null(drop_msg)) {
      return(drop_msg)
    }

    if (!S7::S7_inherits(self@parameter_names, ParameterNameOptions)) {
      return("@parameter_names must be a ParameterNameOptions object.")
    }

    if (!S7::S7_inherits(self@ci, CIOptions)) {
      return("@ci must be a CIOptions object.")
    }

    if (length(self@missing_text) != 1 || is.na(self@missing_text)) {
      return(sprintf(
        "@missing_text must be a single character string. Got: %s",
        self@missing_text
      ))
    }

    valid_missing_apply <- c("all", "numeric", "character")
    if (
      length(self@missing_apply_to) != 1 ||
        !self@missing_apply_to %in% valid_missing_apply
    ) {
      return(sprintf(
        "@missing_apply_to must be in: %s\n  Got: %s",
        paste(valid_missing_apply, collapse = ", "),
        self@missing_apply_to
      ))
    }
  },
  constructor = function(
    title = "Model Parameters",
    parameter_names = ParameterNameOptions(),
    sections = SectionOptions(),
    columns = NULL,
    add_columns = NULL,
    drop_columns = NULL,
    hide_empty_columns = TRUE,
    row_filter = filter_rules(),
    display_transforms = list(),
    variability_rules = NULL,
    n_sigfig = 3,
    ci = CIOptions(),
    n_decimals_ofv = 3,
    pvalue_scientific = FALSE,
    pvalue_threshold = NULL,
    missing_text = "",
    missing_apply_to = "all",
    footnote_order = c("summary_info", "equations", "abbreviations")
  ) {
    if (!is.list(display_transforms)) {
      rlang::abort(paste0(
        "@display_transforms must be a list, not a ",
        class(display_transforms)[1]
      ))
    }

    if (length(display_transforms) > 0 && !is.null(names(display_transforms))) {
      names(display_transforms) <- tolower(names(display_transforms))
    }

    for (kind in c("theta", "omega", "sigma")) {
      if (!kind %in% names(display_transforms)) {
        display_transforms[[kind]] <- "all"
      }
    }

    columns <- expand_ci_alias(columns)
    add_columns <- expand_ci_alias(add_columns)

    if (is.null(variability_rules)) {
      variability_rules <- default_variability_rules()
    }

    if (length(variability_rules) > 0 && length(drop_columns) > 0) {
      referenced <- character(0)
      for (rule in variability_rules) {
        formula <- rlang::eval_tidy(rule)
        if (!rlang::is_formula(formula)) {
          next
        }
        referenced <- unique(c(referenced, all.vars(rlang::f_lhs(formula))))
      }
      dropped_used <- intersect(referenced, drop_columns)
      if (length(dropped_used) > 0) {
        rlang::warn(paste0(
          "variability_rules reference dropped columns: ",
          paste(dropped_used, collapse = ", "),
          ". These rules will still run; adjust variability_rules or drop_columns if unintended."
        ))
      }
    }

    if (length(variability_rules) > 0) {
      want_components <- any(
        c("cv", "corr", "sd") %in%
          c(columns, add_columns %||% character(0))
      )
      if (want_components) {
        rlang::warn(paste0(
          "variability_rules will be ignored when cv/corr/sd are included in columns or add_columns. ",
          "Remove variability_rules or omit cv/corr/sd to use the variability column."
        ))
      }
    }

    S7::new_object(
      S7::S7_object(),
      sections = sections,
      display_transforms = display_transforms,
      variability_rules = variability_rules,
      row_filter = row_filter,
      columns = columns,
      drop_columns = drop_columns,
      n_sigfig = n_sigfig,
      add_columns = add_columns,
      n_decimals_ofv = n_decimals_ofv,
      parameter_names = parameter_names,
      title = title,
      hide_empty_columns = hide_empty_columns,
      pvalue_scientific = pvalue_scientific,
      pvalue_threshold = pvalue_threshold,
      ci = ci,
      missing_text = missing_text,
      missing_apply_to = missing_apply_to,
      footnote_order = footnote_order
    )
  }
)

# ==============================================================================
# SummarySpec S7 Class
# ==============================================================================

#' Summary specification for run summary tables
#'
#' @param title Character. Title for the table header. Default is
#'   "Run Summary".
#' @param models_to_include Character vector of model names to include in the
#'   table (with or without .mod/.ctl extensions), or NULL (default).
#' @param tag_filter Character vector of tags, or NULL (default). Only models
#'   with at least one matching tag are included.
#' @param tag_exclude Character vector of tags to exclude, or NULL (default).
#'   Models with any matching tag are removed. Applied after tag_filter.
#' @param summary_filter Filter rules created with `summary_filter_rules()`.
#' @param remove_unrun_models Logical. If TRUE (default), models without
#'   completed runs are excluded from the table.
#' @param sections A `SectionOptions` object, or `NULL` for an empty section
#'   configuration. Prefer configuring sections with [set_spec_sections()] in
#'   a pipe for user-facing code.
#' @param columns Character vector of columns to include. Valid columns:
#'   "based_on", "description", "n_parameters", "problem",
#'   "number_data_records", "number_subjects", "number_obs",
#'   "estimation_method", "estimation_time", "covariance_time",
#'   "postprocess_time", "function_evaluations", "significant_digits",
#'   "ofv", "dofv", "condition_number", "termination_status", "pvalue", "df".
#'   Note: "pvalue" and "df" require "dofv" to be calculated; pvalue uses the
#'   Likelihood Ratio Test (LRT) assuming nested models.
#' @param add_columns Character vector of columns to append to the default
#'   `columns` list, or NULL (default).
#' @param drop_columns Character vector of columns to exclude from output, or
#'   NULL (default).
#' @param hide_empty_columns Logical. If TRUE, columns with all NA values are
#'   hidden. Default is TRUE.
#' @param n_sigfig Number of significant figures for numeric formatting.
#'   Default is 3.
#' @param n_decimals_ofv Number of decimal places for OFV and dOFV values.
#'   Default is 3.
#' @param time_format Format for time columns. Options: "seconds" (default),
#'   "minutes", "hours", "auto" (auto-scale based on magnitude).
#' @param pvalue_scientific Logical. If TRUE, p-values are formatted
#'   in scientific notation (e.g., 1.23e-04). If FALSE (default), uses significant figures
#'   from n_sigfig.
#' @param pvalue_threshold Numeric or NULL. If set, p-values below this threshold
#'   are displayed as "< threshold" (e.g., "< 0.05"). Default is NULL (no threshold).
#' @param footnote_order Character vector controlling the order of footnote sections,
#'   or NULL to disable footnotes. Valid value for SummarySpec: "abbreviations".
#'   Default is c("abbreviations").
#'
#' @export
SummarySpec <- S7::new_class(
  "SummarySpec",
  parent = BaseSpec,
  properties = list(
    models_to_include = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    tag_filter = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    tag_exclude = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    summary_filter = S7::new_property(
      class = S7::class_list,
      default = list()
    ),
    remove_unrun_models = S7::new_property(
      class = S7::class_logical,
      default = TRUE
    ),
    time_format = S7::new_property(
      class = S7::class_character,
      default = "seconds"
    ),
    default_columns = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        c(
          "based_on",
          "description",
          "n_parameters",
          "condition_number",
          "ofv",
          "dofv",
          "pvalue"
        )
      }
    ),
    allowed_footnotes = S7::new_property(
      class = S7::class_character,
      getter = function(self) "abbreviations"
    )
  ),
  validator = function(self) {
    valid_fields <- summary_spec_valid_columns()
    columns_msg <- validate_columns_in_set(
      self@columns,
      valid_fields,
      "@columns"
    )
    if (!is.null(columns_msg)) {
      return(columns_msg)
    }

    if (!is.null(self@add_columns)) {
      add_msg <- validate_columns_in_set(
        self@add_columns,
        valid_fields,
        "@add_columns"
      )
      if (!is.null(add_msg)) {
        return(add_msg)
      }
    }

    if (!self@time_format %in% c("seconds", "minutes", "hours", "auto")) {
      return(sprintf(
        "@time_format must be 'seconds', 'minutes', 'hours', or 'auto'. Got: '%s'",
        self@time_format
      ))
    }

    if (
      length(self@remove_unrun_models) != 1 || is.na(self@remove_unrun_models)
    ) {
      return("@remove_unrun_models must be TRUE or FALSE")
    }

    if (
      length(self@summary_filter) > 0 &&
        !all(vapply(self@summary_filter, rlang::is_quosure, logical(1)))
    ) {
      return(
        "@summary_filter rules must be created with summary_filter_rules()"
      )
    }

    drop_msg <- validate_columns_in_set(
      self@drop_columns,
      valid_fields,
      "@drop_columns"
    )
    if (!is.null(drop_msg)) {
      return(drop_msg)
    }
  },
  constructor = function(
    title = "Run Summary",
    models_to_include = NULL,
    tag_filter = NULL,
    tag_exclude = NULL,
    summary_filter = summary_filter_rules(),
    remove_unrun_models = TRUE,
    sections = SectionOptions(),
    columns = NULL,
    add_columns = NULL,
    drop_columns = NULL,
    hide_empty_columns = TRUE,
    n_sigfig = 3,
    n_decimals_ofv = 3,
    time_format = "seconds",
    pvalue_scientific = FALSE,
    pvalue_threshold = NULL,
    footnote_order = "abbreviations"
  ) {
    S7::new_object(
      S7::S7_object(),
      sections = sections,
      summary_filter = summary_filter,
      models_to_include = models_to_include,
      add_columns = add_columns,
      columns = columns,
      drop_columns = drop_columns,
      n_sigfig = n_sigfig,
      n_decimals_ofv = n_decimals_ofv,
      time_format = time_format,
      title = title,
      hide_empty_columns = hide_empty_columns,
      remove_unrun_models = remove_unrun_models,
      tag_filter = tag_filter,
      tag_exclude = tag_exclude,
      pvalue_scientific = pvalue_scientific,
      pvalue_threshold = pvalue_threshold,
      footnote_order = footnote_order
    )
  }
)
