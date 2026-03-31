# Include spec.R for TableSpec and
# SummarySpec to use sections_property()
#' @include spec.R
NULL

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
#' @param append_omega_with_theta Logical. If TRUE (default), append associated theta
#'   names to omega parameters (e.g., "OM1 TVCL" or "OMEGA(1,1)-THETA1")
#'
#' @section Properties:
#' The following properties are available on a `ParameterNameOptions` object:
#' \itemize{
#'   \item `source` - Which name field to use ("name", "display", or "nonmem").
#'   \item `append_omega_with_theta` - Whether to append theta info to omega names.
#' }
#'
#' @export
ParameterNameOptions <- S7::new_class(
  "ParameterNameOptions",
  properties = list(
    source = S7::new_property(
      class = S7::class_character,
      default = "name"
    ),
    append_omega_with_theta = S7::new_property(
      class = S7::class_logical,
      default = TRUE
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
    if (
      length(self@append_omega_with_theta) != 1 ||
        is.na(self@append_omega_with_theta)
    ) {
      return(sprintf(
        "@append_omega_with_theta must be TRUE or FALSE. Got: %s",
        self@append_omega_with_theta
      ))
    }
  }
)

# ==============================================================================
# TableSpec S7 Class
# ==============================================================================

#' Table specification for parameter tables
#'
#' @param title Character. Title for the parameter table header. Default is
#'   "Model Parameters".
#' @param parameter_names ParameterNameOptions object controlling how parameter names
#'   are displayed. Controls which name field to use ("name", "display", or "nonmem")
#'   and whether to append theta info to omega names. Defaults to `ParameterNameOptions()`.
#' @param columns Character vector of columns to include in output.
#' @param add_columns Character vector of columns to append to the column list.
#'   Useful for comparisons when you want to add columns like "pct_change"
#'   without overriding `columns`.
#' @param drop_columns Character vector of columns to exclude from output, or
#'   NULL (default) to include all columns.
#' @param hide_empty_columns Logical. If TRUE, columns that are all NA/empty
#'   are automatically hidden unless explicitly requested via `columns` or
#'   `add_columns`. Default is TRUE.
#' @param sections Section rules created with `section_rules()`.
#' @param section_filter Character vector of section labels to exclude from the
#'   table. Use `NA` to also exclude rows that don't match any section rule.
#'   Default is NULL (no filtering). See `set_spec_section_filter()`.
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
  properties = list(
    title = S7::new_property(
      class = S7::class_character,
      default = "Model Parameters"
    ),
    parameter_names = S7::new_property(
      class = ParameterNameOptions,
      default = ParameterNameOptions()
    ),
    columns = S7::new_property(
      class = S7::class_character,
      default = c(
        "name",
        "symbol",
        "unit",
        "estimate",
        "variability",
        "ci_low",
        "ci_high",
        "rse",
        "shrinkage"
      ),
      setter = function(self, value) {
        S7::prop(self, "columns") <- value
        # if @columns <- is set or mutated we
        # set .columns_provided to true for
        # hide_empty_columns
        self@.columns_provided <- TRUE
        self
      }
    ),
    add_columns = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL,
      setter = function(self, value) {
        S7::prop(self, "add_columns") <- value
        self
      }
    ),
    drop_columns = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    hide_empty_columns = S7::new_property(
      class = S7::class_logical,
      default = TRUE
    ),
    sections = sections_property(),
    section_filter = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
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
      class = S7::class_list,
      default = list()
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
    footnote_order = S7::new_property(
      class = S7::class_character | NULL,
      default = c("summary_info", "equations", "abbreviations")
    ),
    .columns_provided = S7::new_property(
      # Internal: TRUE when user explicitly supplies columns.
      class = S7::class_logical,
      default = FALSE
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
      "ci_low",
      "ci_high",
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

    if (!all(vapply(self@sections, rlang::is_formula, logical(1)))) {
      return("@section rules must be created with section_rules()")
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

    if (!S7::S7_inherits(self@parameter_names, ParameterNameOptions)) {
      return("@parameter_names must be a ParameterNameOptions object.")
    }

    if (
      length(self@hide_empty_columns) != 1 || is.na(self@hide_empty_columns)
    ) {
      return(sprintf(
        "@hide_empty_columns must be TRUE or FALSE. Got: %s",
        self@hide_empty_columns
      ))
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

    if (
      length(self@.columns_provided) != 1 ||
        is.na(self@.columns_provided)
    ) {
      return(sprintf(
        "@.columns_provided must be TRUE or FALSE. Got: %s",
        self@.columns_provided
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

    footnote_msg <- validate_table_footnote_order(self@footnote_order)
    if (!is.null(footnote_msg)) {
      return(footnote_msg)
    }
  },
  constructor = function(
    title = "Model Parameters",
    parameter_names = ParameterNameOptions(),
    columns = NULL,
    add_columns = NULL,
    drop_columns = NULL,
    hide_empty_columns = TRUE,
    sections = section_rules(),
    section_filter = NULL,
    row_filter = filter_rules(),
    display_transforms = list(),
    variability_rules = default_variability_rules(),
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

    columns_provided <- !is.null(columns)
    if (is.null(columns)) {
      columns <- c(
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
    columns <- expand_ci_alias(columns)
    add_columns <- expand_ci_alias(add_columns)

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

    spec <- S7::new_object(
      S7::S7_object(),
      display_transforms = display_transforms,
      variability_rules = variability_rules,
      sections = sections,
      section_filter = section_filter,
      row_filter = row_filter,
      columns = columns,
      drop_columns = drop_columns,
      n_sigfig = n_sigfig,
      add_columns = add_columns,
      n_decimals_ofv = n_decimals_ofv,
      parameter_names = parameter_names,
      title = title,
      hide_empty_columns = hide_empty_columns,
      .columns_provided = columns_provided,
      pvalue_scientific = pvalue_scientific,
      pvalue_threshold = pvalue_threshold,
      ci = ci,
      missing_text = missing_text,
      missing_apply_to = missing_apply_to,
      footnote_order = footnote_order
    )
    # setter is called for columns which flips columns provided.
    # this reverts it back to what ever it was.
    spec@.columns_provided <- columns_provided
    spec
  }
)

# ==============================================================================
# HyperionTable S7 Class - Intermediate Table Representation
# ==============================================================================

#' HyperionTable - Intermediate representation for table rendering
#'
#' A declarative table specification that can be rendered to multiple output
#' formats (gt, flextable). Captures all styling intent in a format-agnostic way.
#'
#' @param data Data frame containing the table data
#' @param table_type Character string: "parameter", "comparison", or "summary"
#' @param groupname_col Column name for row grouping (NULL for no grouping)
#' @param hide_cols Character vector of columns to hide
#' @param col_labels Named list mapping column names to display labels
#' @param title Table title (NULL for no title)
#' @param spanners List of spanner specifications for column grouping
#' @param numeric_cols Character vector of columns to format as numeric
#' @param n_sigfig Number of significant figures for numeric formatting
#' @param ci CIOptions object controlling CI merge behavior.
#' @param ci_merges List of CI merge specifications
#' @param ci_missing_rows Integer vector of rows with missing CI values
#' @param missing_text Text to show for other missing values (default "")
#' @param bold_locations Character vector of locations to bold
#' @param borders List of border specifications
#' @param footnotes List of footnote specifications (in order)
#' @param source_spec Original TableSpec/SummarySpec (for reference)
#'
#' @return A HyperionTable S7 object
#' @noRd
HyperionTable <- S7::new_class(
  "HyperionTable",
  properties = list(
    # Core data
    data = S7::class_data.frame,

    # Metadata
    table_type = S7::new_property(
      class = S7::class_character,
      default = "parameter"
    ),

    # Structure
    groupname_col = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    hide_cols = S7::new_property(
      class = S7::class_character,
      default = character(0)
    ),

    # Labels & Headers
    col_labels = S7::new_property(
      class = S7::class_list,
      default = list()
    ),
    title = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    spanners = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Formatting
    numeric_cols = S7::new_property(
      class = S7::class_character,
      default = character(0)
    ),
    n_sigfig = S7::new_property(
      class = S7::class_numeric,
      default = 3
    ),
    ci = S7::new_property(
      class = CIOptions,
      default = CIOptions()
    ),
    ci_merges = S7::new_property(
      class = S7::class_list,
      default = list()
    ),
    ci_missing_rows = S7::new_property(
      # Row indices where CI missing text should show "-"
      class = S7::class_integer,
      default = integer(0)
    ),
    missing_text = S7::new_property(
      class = S7::class_character,
      default = ""
    ),
    missing_apply_to = S7::new_property(
      class = S7::class_character,
      default = "all"
    ),

    # Styling
    bold_locations = S7::new_property(
      # "column_labels", "title", "row_groups", "spanners"
      class = S7::class_character,
      default = c("column_labels")
    ),
    borders = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Footnotes (in order)
    footnotes = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Reference to original spec
    source_spec = S7::new_property(
      class = S7::class_any,
      default = NULL
    )
  ),
  validator = function(self) {
    valid_types <- c("parameter", "comparison", "summary")
    if (!self@table_type %in% valid_types) {
      return(sprintf(
        "@table_type must be one of: %s. Got: %s",
        paste(valid_types, collapse = ", "),
        self@table_type
      ))
    }

    valid_bold <- c("column_labels", "title", "row_groups", "spanners")
    bad_bold <- setdiff(self@bold_locations, valid_bold)
    if (length(bad_bold) > 0) {
      return(sprintf(
        "@bold_locations must be in: %s. Got: %s",
        paste(valid_bold, collapse = ", "),
        paste(bad_bold, collapse = ", ")
      ))
    }
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
#' @param sections Section rules created with `section_rules()`.
#' @param section_filter Character vector of section labels to exclude from the
#'   table. Use `NA` to also exclude models that don't match any section rule.
#'   Default is NULL (no filtering). See `set_spec_section_filter()`.
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
  properties = list(
    title = S7::new_property(
      class = S7::class_character,
      default = "Run Summary"
    ),
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
    sections = sections_property(),
    section_filter = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
    ),
    columns = S7::new_property(
      class = S7::class_character,
      default = c(
        "based_on",
        "description",
        "n_parameters",
        "condition_number",
        "ofv",
        "dofv",
        "pvalue"
      ),
      setter = function(self, value) {
        S7::prop(self, "columns") <- value
        self
      }
    ),
    add_columns = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL,
      setter = function(self, value) {
        S7::prop(self, "add_columns") <- value
        self
      }
    ),
    drop_columns = S7::new_property(
      class = S7::class_character | NULL,
      default = NULL
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
    time_format = S7::new_property(
      class = S7::class_character,
      default = "seconds"
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
      default = c("abbreviations")
    )
  ),
  validator = function(self) {
    if (
      length(self@sections) > 0 &&
        !all(vapply(self@sections, rlang::is_formula, logical(1)))
    ) {
      return("@section rules must be created with section_rules()")
    }

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
      length(self@n_sigfig) != 1 ||
        self@n_sigfig < 1 ||
        self@n_sigfig != floor(self@n_sigfig)
    ) {
      return(sprintf(
        "@n_sigfig must be a positive whole number. Got: %s",
        self@n_sigfig
      ))
    }

    ofv_msg <- validate_ofv_decimals(self@n_decimals_ofv, "@n_decimals_ofv")
    if (!is.null(ofv_msg)) {
      return(ofv_msg)
    }

    if (
      length(self@hide_empty_columns) != 1 || is.na(self@hide_empty_columns)
    ) {
      return("@hide_empty_columns must be TRUE or FALSE")
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

    footnote_msg <- validate_summary_footnote_order(self@footnote_order)
    if (!is.null(footnote_msg)) {
      return(footnote_msg)
    }
  },
  constructor = function(
    title = "Run Summary",
    models_to_include = NULL,
    tag_filter = NULL,
    tag_exclude = NULL,
    summary_filter = summary_filter_rules(),
    remove_unrun_models = TRUE,
    columns = NULL,
    add_columns = NULL,
    drop_columns = NULL,
    hide_empty_columns = TRUE,
    n_sigfig = 3,
    n_decimals_ofv = 3,
    time_format = "seconds",
    pvalue_scientific = FALSE,
    pvalue_threshold = NULL,
    sections = section_rules(),
    section_filter = NULL,
    footnote_order = "abbreviations"
  ) {
    columns <- merge_summary_columns(columns, add_columns)

    S7::new_object(
      S7::S7_object(),
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
      sections = sections,
      section_filter = section_filter,
      footnote_order = footnote_order
    )
  }
)

#' @noRd
AnySpec <- S7::new_union(TableSpec, SummarySpec)
