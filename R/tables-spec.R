# =============================================================================
# User-facing DSL functions
# ==============================================================================

#' Create section assignment rules
#'
#' Creates rules for assigning parameters to named sections in the output table.
#' Rules are evaluated after name transformation, so you can match on the final
#' display name or use the preserved `nonmem_name` and `user_name` columns.
#'
#' @param ... Formula expressions like `kind == "THETA" ~ "Structural Parameters"`
#'
#' @section Available columns:
#' The following columns are available for use in section rules:
#' \itemize{
#'   \item `nonmem_name` - NONMEM identifier ("THETA1", "OMEGA(1,1)")
#'   \item `user_name` - User name from control file comments ("CL", "OM1")
#'   \item `name` - Display name (depends on `parameter_names` setting)
#'   \item `kind` - Parameter type: "THETA", "OMEGA", or "SIGMA"
#'   \item `diagonal` - TRUE for diagonal matrix elements (variance), FALSE for off-diagonal (covariance)
#'   \item `fixed` - TRUE if parameter is fixed
#' }
#'
#' @return List of quosures for use in TableSpec
#' @examples
#' section_rules(
#'   grepl("~", user_name) ~ "Covariate Effects",
#'   kind == "THETA" ~ "Structural Parameters",
#'   kind == "OMEGA" & diagonal ~ "Between-Subject Variability",
#'   kind == "SIGMA" ~ "Residual Variability"
#' )
#' @export
section_rules <- function(...) {
  rlang::enquos(...)
}

#' Create row filter rules
#'
#' Creates rules for filtering which parameters appear in the output table.
#' Rules are evaluated after name transformation.
#'
#' @param ... Filter expressions like `!fixed`, `diagonal`
#'
#' @section Available columns:
#' The following columns are available for use in filter rules:
#' \itemize{
#'   \item `nonmem_name` - NONMEM identifier ("THETA1", "OMEGA(1,1)")
#'   \item `user_name` - User name from control file comments ("CL", "OM1")
#'   \item `name` - Display name (depends on `parameter_names` setting)
#'   \item `kind` - Parameter type: "THETA", "OMEGA", or "SIGMA"
#'   \item `diagonal` - TRUE for diagonal matrix elements (variance), FALSE for off-diagonal (covariance)
#'   \item `fixed` - TRUE if parameter is fixed
#' }
#'
#' @return List of quosures for use in TableSpec
#' @examples
#' filter_rules(
#'   !fixed,
#'   diagonal,
#'   kind != "SIGMA"
#' )
#' @export
filter_rules <- function(...) {
  rlang::enquos(...)
}

#' Create variability display rules
#'
#' Creates rules for constructing the `variability` display column. Rules are
#' evaluated with `case_when()`.
#'
#' @param ... Formula expressions like `fixed ~ "(Fixed)"` or
#'   `!is.na(cv) ~ sprintf("(CV = %s%%)", cv)`
#'
#' @return List of quosures for use in TableSpec
#' @examples
#' variability_rules(
#'   fixed ~ "(Fixed)",
#'   !is.na(corr) ~ sprintf("(Corr = %s)", corr),
#'   !is.na(cv) & cv != 0 ~ sprintf("(CV = %s%%)", cv),
#'   !is.na(sd) ~ sprintf("(SD = %s)", sd),
#'   TRUE ~ NA_character_
#' )
#' @export
variability_rules <- function(...) {
  rlang::enquos(...)
}

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

# ==============================================================================
# TableSpec S7 Class
# ==============================================================================

#' @noRd
expand_ci_alias <- function(cols) {
  if (is.null(cols) || length(cols) == 0) {
    return(cols)
  }
  if ("ci" %in% cols) {
    replace_idx <- which(cols == "ci")
    cols <- unlist(
      lapply(seq_along(cols), function(i) {
        if (i %in% replace_idx) c("ci_low", "ci_high") else cols[[i]]
      }),
      use.names = FALSE
    )
    cols <- cols[!duplicated(cols)]
  }
  cols
}

#' @noRd
valid_table_columns <- function() {
  c(
    "kind",
    "name",
    "random_effect",
    "description",
    "symbol",
    "unit",
    "estimate",
    "stderr",
    "diagonal",
    "ci_low",
    "ci_high",
    "fixed",
    "variability",
    "cv",
    "corr",
    "sd",
    "rse",
    "shrinkage"
  )
}

#' @noRd
comparison_suffix_columns <- function() {
  c(
    "symbol",
    "unit",
    "estimate",
    "rse",
    "ci_low",
    "ci_high",
    "variability",
    "stderr",
    "fixed",
    "shrinkage"
  )
}

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
    sections = S7::new_property(
      class = S7::class_list,
      default = list(),
      setter = function(self, value) {
        if (length(value) > 0) {
          labels <- vapply(
            value,
            function(r) {
              rlang::f_rhs(rlang::eval_tidy(r))
            },
            character(1)
          )
          # Keep last rule for each label (later rules win)
          dups <- duplicated(labels, fromLast = TRUE)
          value <- value[!dups]
        }
        self@sections <- value
        self
      }
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
        if (!rlang::is_formula(formula)) next
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
