# ==============================================================================
# User-facing DSL functions
# ==============================================================================

#' Create filter rules for summary table filtering
#'
#' Creates rules for filtering which models appear in the summary table.
#' Rules are evaluated against the summary data frame.
#'
#' @param ... Filter expressions like `"final" %in% tags`, `!is.null(description)`
#'
#' @section Available columns:
#' The following columns are available for use in filter rules. Filters are
#' evaluated against the summary data frame, so columns must be included in
#' `SummarySpec@columns` to be available.
#' \itemize{
#'   \item `model` - Model name (e.g., "run001")
#'   \item `based_on` - Reference model name(s)
#'   \item `description` - Model description
#'   \item `n_parameters` - Number of non-fixed parameters
#'   \item `problem` - Problem description from run details
#'   \item `number_data_records` - Number of data records
#'   \item `number_subjects` - Number of subjects
#'   \item `number_obs` - Number of observations
#'   \item `estimation_method` - Estimation method
#'   \item `estimation_time` - Estimation time
#'   \item `covariance_time` - Covariance time
#'   \item `postprocess_time` - Postprocess time
#'   \item `function_evaluations` - Function evaluations
#'   \item `significant_digits` - Significant digits
#'   \item `ofv` - Objective function value
#'   \item `dofv` - Change in OFV
#'   \item `condition_number` - Condition number
#'   \item `termination_status` - Termination status
#'   \item `pvalue` - LRT p-value
#'   \item `df` - Degrees of freedom
#' }
#'
#' @return List of quosures for use in SummarySpec
#' @examples
#' summary_filter_rules(
#'   ofv < 0,
#'   estimation_method == "FOCE",
#'   model %in% c("run001", "run003")
#' )
#' @export
summary_filter_rules <- function(...) {
  rlang::enquos(...)
}

# ==============================================================================
# SummarySpec S7 Class
# ==============================================================================

#' @noRd
valid_summary_columns <- function() {
  c(
    # From tree metadata
    "based_on",
    "description",
    # From parameters
    "n_parameters",
    # From run_details
    "problem",
    "number_data_records",
    "number_subjects",
    "number_obs",
    "estimation_method",
    "estimation_time",
    "covariance_time",
    "postprocess_time",
    "function_evaluations",
    "significant_digits",
    # From minimization_results
    "ofv",
    "dofv",
    "condition_number",
    "termination_status",
    # Computed LRT fields
    "pvalue",
    "df"
  )
}

#' @noRd
merge_summary_columns <- function(columns, add_columns) {
  if (is.null(columns)) {
    columns <- c(
      "based_on",
      "description",
      "n_parameters",
      "condition_number",
      "ofv",
      "dofv",
      "pvalue"
    )
  }
  if (!is.null(add_columns)) {
    columns <- unique(c(columns, add_columns))
  }
  columns
}

#' Summary specification for run summary tables
#'
#' @param title Character. Title for the table header. Default is
#'   "Run Summary".
#' @param models_to_include Character vector of model names to include in the
#'   table (with or without .mod/.ctl extensions), or NULL (default).
#' @param tag_filter Character vector of tags, or NULL (default). Only models
#'   with at least one matching tag are included.
#' @param summary_filter Filter rules created with `summary_filter_rules()`.
#' @param remove_unrun_models Logical. If TRUE (default), models without
#'   completed runs are excluded from the table.
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
    summary_filter = S7::new_property(
      class = S7::class_list,
      default = list()
    ),
    remove_unrun_models = S7::new_property(
      class = S7::class_logical,
      default = TRUE
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
      pvalue_scientific = pvalue_scientific,
      pvalue_threshold = pvalue_threshold,
      footnote_order = footnote_order
    )
  }
)

# ==============================================================================
# Apply spec to lineage tree
# ==============================================================================

#' Apply summary specification to lineage tree
#'
#' Filters models, loads summaries, and prepares data for table generation.
#'
#' @param tree A hyperion_nonmem_tree object from `get_model_lineage()`
#' @param spec A SummarySpec object
#'
#' @return Data frame with summary_spec attribute, ready for make_summary_table()
#' @export
apply_summary_spec <- function(tree, spec = SummarySpec()) {
  if (!inherits(tree, "hyperion_nonmem_tree")) {
    rlang::abort(
      "tree must be a hyperion_nonmem_tree object from get_model_lineage()"
    )
  }
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("spec must be a SummarySpec object")
  }

  if (length(tree$nodes) == 0) {
    rlang::abort("tree must contain at least one model.")
  }

  source_dir <- tryCatch(
    error = function(e) {
      rlang::abort(
        "Could not resolve `tree$source_dir` path from pharos.toml"
      )
    },
    hyperion::from_config_relative(tree$source_dir)
  )

  metadata_df <- build_metadata_df(tree) |>
    filter_metadata(spec)

  # Topologically sort models
  sorted_names <- topological_sort_models(metadata_df, tree)

  # Load models
  models <- load_models(sorted_names, source_dir)

  # Build summary data frame (pass metadata for based_on/description)
  df <- build_summary_df(models, sorted_names, metadata_df, spec)

  # Apply summary_filter rules to summary columns
  if (length(spec@summary_filter) > 0) {
    for (f in spec@summary_filter) {
      df <- df |>
        dplyr::filter(!!f)
    }
  }

  # Apply drop_columns
  if (!is.null(spec@drop_columns)) {
    df <- df[, setdiff(names(df), spec@drop_columns), drop = FALSE]
  }

  attr(df, "summary_spec") <- spec
  df
}

#' Filter metadata data frame by tag and model name
#' @noRd
filter_metadata <- function(metadata_df, spec) {
  if (!is.null(spec@tag_filter)) {
    metadata_df <- metadata_df |>
      dplyr::rowwise() |>
      dplyr::filter(any(spec@tag_filter %in% .data$tags)) |>
      dplyr::ungroup()
  }

  if (!is.null(spec@models_to_include)) {
    include_stems <- tolower(tools::file_path_sans_ext(spec@models_to_include))
    metadata_df <- metadata_df |>
      dplyr::filter(
        tolower(tools::file_path_sans_ext(.data$name)) %in% include_stems
      )
  }

  if (nrow(metadata_df) == 0) {
    parts <- "No models remain after filtering."
    if (!is.null(spec@tag_filter)) {
      parts <- c(
        parts,
        sprintf("tag_filter: %s", paste(spec@tag_filter, collapse = ", "))
      )
    }
    if (!is.null(spec@models_to_include)) {
      parts <- c(
        parts,
        sprintf(
          "models_to_include: %s",
          paste(spec@models_to_include, collapse = ", ")
        )
      )
    }
    rlang::abort(paste(parts, collapse = "\n"))
  }

  metadata_df
}

# ==============================================================================
# Helper functions
# ==============================================================================

#' Build metadata data frame from tree nodes
#' @noRd
build_metadata_df <- function(tree) {
  nodes <- tree$nodes

  rows <- lapply(names(nodes), function(name) {
    node <- nodes[[name]]
    data.frame(
      name = name,
      description = node$description %||% NA_character_,
      tags = I(list(node$tags %||% character(0))),
      based_on = I(list(as.character(unlist(node$based_on %||% list())))),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

#' Topological sort of models based on based_on relationships
#' @noRd
topological_sort_models <- function(metadata_df, tree) {
  names_to_sort <- sort(metadata_df$name)

  # Build adjacency: parent -> children
  # A model's parents are in based_on
  parent_of <- list()
  for (i in seq_len(nrow(metadata_df))) {
    name <- metadata_df$name[i]
    based_on <- metadata_df$based_on[[i]]
    parent_of[[name]] <- based_on
  }

  # Kahn's algorithm for topological sort
  # Count incoming edges (parents)
  in_degree <- integer(length(names_to_sort))
  names(in_degree) <- names_to_sort
  for (name in names_to_sort) {
    parents <- parent_of[[name]]
    # Only count parents that are in our filtered set
    in_degree[name] <- sum(parents %in% names_to_sort)
  }

  # Start with nodes that have no parents (in our filtered set)
  queue <- sort(names_to_sort[in_degree == 0])
  sorted <- character(0)

  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    sorted <- c(sorted, current)

    # Find children of current (models where current is in based_on)
    for (name in names_to_sort) {
      if (name %in% sorted) next
      if (current %in% parent_of[[name]]) {
        in_degree[name] <- in_degree[name] - 1
        if (in_degree[name] == 0) {
          queue <- sort(c(queue, name))
        }
      }
    }
  }

  # If not all nodes sorted, there's a cycle - just append remaining
  remaining <- sort(setdiff(names_to_sort, sorted))
  c(sorted, remaining)
}

#' Load models for given model names
#' @noRd
load_models <- function(model_names, source_dir) {
  models <- list()
  for (name in model_names) {
    model_path <- file.path(source_dir, name)
    mod <- tryCatch(
      read_model(model_path),
      error = function(e) {
        rlang::warn(paste0("could not load model: ", model_path))
        NULL
      }
    )
    models[name] <- list(mod)
  }

  if (all(vapply(models, is.null, logical(1)))) {
    rlang::abort("No models could be loaded.")
  }

  models
}

#' Filter unrun model rows and remove the .unrun column
#' @noRd
filter_unrun_rows <- function(df, remove = TRUE) {
  if (!".unrun" %in% names(df)) return(df)
  if (remove) {
    df <- df[!df$.unrun, , drop = FALSE]
  }
  df[, setdiff(names(df), ".unrun"), drop = FALSE]
}

#' Remove internal columns and reorder to match spec column order
#' @noRd
select_output_columns <- function(df, spec, needs_dofv) {
  # Remove internal columns
  internal_cols <- c(".name", ".based_on_raw")
  if (!needs_dofv && "based_on" %in% spec@columns) {
    df <- df[, setdiff(names(df), internal_cols), drop = FALSE]
  } else if (needs_dofv) {
    df <- df[, setdiff(names(df), ".name"), drop = FALSE]
    df <- df[, setdiff(names(df), ".based_on_raw"), drop = FALSE]
  }

  # Remove internal columns that weren't requested (but we needed for calculations)
  # Keep df if pvalue is requested (used for display merging)
  keep_df_for_pvalue <- "pvalue" %in% spec@columns
  internal_calc_cols <- c(
    "number_obs",
    "ofv",
    "n_parameters",
    "dofv",
    "df",
    "pvalue"
  )
  for (col in internal_calc_cols) {
    if (col == "df" && keep_df_for_pvalue) next
    if (needs_dofv && col %in% names(df) && !col %in% spec@columns) {
      df <- df[, setdiff(names(df), col), drop = FALSE]
    }
  }

  # Reorder columns to match spec@columns order
  # Include df after pvalue if pvalue is requested (for merge)
  col_order <- c("model", intersect(spec@columns, names(df)))
  if (keep_df_for_pvalue && "df" %in% names(df) && !"df" %in% col_order) {
    pvalue_idx <- which(col_order == "pvalue")
    if (length(pvalue_idx) > 0) {
      col_order <- append(col_order, "df", after = pvalue_idx)
    }
  }
  df[, col_order, drop = FALSE]
}

#' Build summary data frame from loaded models
#' @noRd
build_summary_df <- function(models, model_names, metadata_df, spec) {
  needs_dofv <- any(c("dofv", "pvalue", "df") %in% spec@columns)

  run_detail_cols <- c(
    "problem",
    "number_data_records",
    "number_subjects",
    "number_obs",
    "estimation_method",
    "estimation_time",
    "covariance_time",
    "postprocess_time",
    "function_evaluations",
    "significant_digits"
  )
  min_result_cols <- c("ofv", "condition_number", "termination_status")

  needed_from_run_details <- intersect(spec@columns, run_detail_cols)
  needed_from_min_results <- intersect(spec@columns, min_result_cols)
  if (needs_dofv) {
    needed_from_run_details <- unique(c(needed_from_run_details, "number_obs"))
    needed_from_min_results <- unique(c(needed_from_min_results, "ofv"))
  }
  needs_summary <- length(needed_from_run_details) > 0 ||
    length(needed_from_min_results) > 0 ||
    "n_parameters" %in% spec@columns ||
    needs_dofv

  meta_idx <- match(model_names, metadata_df$name)

  rows <- lapply(seq_along(model_names), function(i) {
    name <- model_names[i]
    mod <- models[[name]]

    row <- list(
      model = tools::file_path_sans_ext(basename(name)),
      .name = name,
      .unrun = is.null(mod) || !identical(attr(mod, "run_status"), "run")
    )

    # Metadata columns (from tree, not model)
    if ("based_on" %in% spec@columns || needs_dofv) {
      parents <- metadata_df$based_on[[meta_idx[i]]]
      row$.based_on_raw <- list(parents)
      row$based_on <- if (length(parents) == 0) NA_character_ else
        paste(tools::file_path_sans_ext(basename(parents)), collapse = ", ")
    }
    if ("description" %in% spec@columns) {
      row$description <- metadata_df$description[meta_idx[i]]
    }

    # Ensure requested model-derived columns are present even when summaries fail
    na_defaults <- list(
      problem = NA_character_,
      number_data_records = NA_real_,
      number_subjects = NA_real_,
      number_obs = NA_real_,
      estimation_method = NA_character_,
      estimation_time = NA_real_,
      covariance_time = NA_real_,
      postprocess_time = NA_real_,
      function_evaluations = NA_real_,
      significant_digits = NA_real_,
      ofv = NA_real_,
      condition_number = NA_real_,
      termination_status = NA_character_
    )
    if ("n_parameters" %in% spec@columns || needs_dofv) {
      row$n_parameters <- NA_integer_
    }
    for (col in needed_from_run_details) {
      row[[col]] <- na_defaults[[col]]
    }
    for (col in needed_from_min_results) {
      row[[col]] <- na_defaults[[col]]
    }

    # Model-derived columns — summary(mod) once per model
    if (!is.null(mod) && needs_summary) {
      mod_sum <- tryCatch(summary(mod), error = function(e) {
        rlang::warn(paste0("could not summarize model: ", name))
        NULL
      })

      if (is.null(mod_sum)) {
        row$.unrun <- TRUE
      }

      if (!is.null(mod_sum)) {
        # Prefer run_name from summary for display (handles .ctl/custom names)
        if (!is.null(mod_sum$run_name)) {
          row$model <- mod_sum$run_name
        }

        # n_parameters
        if ("n_parameters" %in% spec@columns || needs_dofv) {
          params <- mod_sum$parameters
          row$n_parameters <- if (
            !is.null(params) && nrow(params) > 0 && "fixed" %in% names(params)
          ) {
            as.integer(sum(!params$fixed, na.rm = TRUE))
          } else {
            NA_integer_
          }
        }

        # run_details (last row)
        if (length(needed_from_run_details) > 0) {
          rd <- mod_sum$run_details
          if (!is.null(rd) && nrow(rd) > 0) {
            last <- rd[nrow(rd), , drop = FALSE]
            for (col in intersect(needed_from_run_details, names(last))) {
              row[[col]] <- last[[col]]
            }
          }
        }

        # minimization_results (last row)
        if (length(needed_from_min_results) > 0) {
          mr <- mod_sum$minimization_results
          if (!is.null(mr) && nrow(mr) > 0) {
            last <- mr[nrow(mr), , drop = FALSE]
            for (col in intersect(needed_from_min_results, names(last))) {
              row[[col]] <- last[[col]]
            }
          }
        }
      }
    }

    row
  })

  df <- dplyr::bind_rows(rows)

  # Downstream pipeline
  df <- filter_unrun_rows(df, remove = spec@remove_unrun_models)

  # Fail explicitly if all rows removed by unrun filter
  if (nrow(df) == 0 && isTRUE(spec@remove_unrun_models)) {
    rlang::abort(
      "All models were filtered out by remove_unrun_models. No run models remain."
    )
  }

  if (needs_dofv) {
    # Ensure dofv-required columns exist (may be absent if all summaries failed)
    for (col in c("ofv", "number_obs", "n_parameters")) {
      if (!col %in% names(df)) {
        df[[col]] <- if (col == "n_parameters") NA_integer_ else NA_real_
      }
    }
    df <- calculate_dofv(df, spec)
  }

  df <- format_time_columns(df, spec)
  time_unit <- attr(df, "summary_time_unit")
  df <- select_output_columns(df, spec, needs_dofv)
  if (!is.null(time_unit)) {
    attr(df, "summary_time_unit") <- time_unit
  }

  df
}

#' Calculate dOFV, df, and p-value for each model vs its parent
#' @noRd
calculate_dofv <- function(df, spec) {
  # Create lookups by .name
  ofv_lookup <- stats::setNames(df$ofv, df$.name)
  nobs_lookup <- stats::setNames(df$number_obs, df$.name)
  npar_lookup <- stats::setNames(df$n_parameters, df$.name)

  needs_pvalue <- "pvalue" %in% spec@columns || "df" %in% spec@columns

  results <- lapply(seq_len(nrow(df)), function(i) {
    parents <- df$.based_on_raw[[i]]

    # No parent - no comparison stats
    if (length(parents) == 0) {
      return(list(dofv = NA_real_, df = NA_integer_, pvalue = NA_real_))
    }

    if (length(parents) > 1) {
      rlang::warn(
        sprintf(
          "model '%s' has multiple parents (%s); using '%s' for dOFV/LRT",
          df$.name[i],
          paste(parents, collapse = ", "),
          parents[1]
        )
      )
    }

    parent_name <- parents[1]

    # Check if parent is in our data
    if (!parent_name %in% names(ofv_lookup)) {
      return(list(dofv = NA_real_, df = NA_integer_, pvalue = NA_real_))
    }

    parent_ofv <- ofv_lookup[[parent_name]]
    parent_nobs <- nobs_lookup[[parent_name]]
    parent_npar <- npar_lookup[[parent_name]]
    model_ofv <- df$ofv[i]
    model_nobs <- df$number_obs[i]
    model_npar <- df$n_parameters[i]

    # Check if number_obs matches
    if (is.na(model_nobs) || is.na(parent_nobs) || model_nobs != parent_nobs) {
      rlang::inform(c(
        sprintf(
          "dOFV not calculated for '%s' vs '%s': observation counts differ (%s vs %s).",
          df$.name[i],
          parent_name,
          if (is.na(model_nobs)) "NA" else as.character(model_nobs),
          if (is.na(parent_nobs)) "NA" else as.character(parent_nobs)
        ),
        i = "dOFV is only calculated when the number of observations matches the reference model."
      ))
      return(list(dofv = NA_real_, df = NA_integer_, pvalue = NA_real_))
    }

    # Calculate dofv
    if (is.na(model_ofv) || is.na(parent_ofv)) {
      return(list(dofv = NA_real_, df = NA_integer_, pvalue = NA_real_))
    }

    dofv <- model_ofv - parent_ofv

    # Calculate df and p-value if requested
    df_val <- NA_integer_
    pvalue <- NA_real_

    if (needs_pvalue && !is.na(model_npar) && !is.na(parent_npar)) {
      df_calc <- as.integer(model_npar - parent_npar)
      # Only calculate p-value if df > 0 (more complex model)
      if (!is.na(df_calc) && df_calc > 0) {
        df_val <- df_calc
        pvalue <- lrt_pvalue(-dofv, df_val)
      }
    }

    list(dofv = dofv, df = df_val, pvalue = pvalue)
  })

  df$dofv <- vapply(results, function(x) x$dofv, numeric(1))
  df$df <- vapply(results, function(x) x$df, integer(1))
  df$pvalue <- vapply(results, function(x) x$pvalue, numeric(1))

  df
}

#' Format time columns based on spec
#' @noRd
format_time_columns <- function(df, spec) {
  time_cols <- intersect(
    c("estimation_time", "covariance_time", "postprocess_time"),
    names(df)
  )

  if (length(time_cols) == 0 || spec@time_format == "seconds") {
    return(df)
  }

  if (spec@time_format == "auto") {
    all_missing <- all(vapply(
      time_cols,
      function(col) all(is.na(df[[col]])),
      logical(1)
    ))
    if (all_missing) {
      attr(df, "summary_time_unit") <- "s"
      return(df)
    }
    max_vals <- vapply(
      time_cols,
      function(col) max(df[[col]], na.rm = TRUE),
      numeric(1)
    )
    max_val <- max(max_vals, na.rm = TRUE)
    if (is.na(max_val) || max_val < 60) {
      unit <- "s"
      divisor <- 1
    } else if (max_val < 3600) {
      unit <- "min"
      divisor <- 60
    } else {
      unit <- "h"
      divisor <- 3600
    }
    for (col in time_cols) {
      df[[col]] <- df[[col]] / divisor
    }
    attr(df, "summary_time_unit") <- unit
    return(df)
  }

  for (col in time_cols) {
    df[[col]] <- format_time_value(df[[col]], spec@time_format)
  }

  df
}

#' Format time values based on format setting
#' @noRd
format_time_value <- function(seconds, format) {
  if (all(is.na(seconds))) return(seconds)

  switch(
    format,
    "minutes" = seconds / 60,
    "hours" = seconds / 3600,
    "auto" = seconds,
    seconds
  )
}

# ==============================================================================
# GT table building
# ==============================================================================

#' Extract SummarySpec from a summary data frame
#'
#' Retrieves the `SummarySpec` attached to a data frame (e.g., from
#' `apply_summary_spec()`). Returns NULL if none is found.
#'
#' @param data Data frame carrying a `summary_spec` attribute
#'
#' @return A SummarySpec object or NULL
#' @export
get_summary_spec <- function(data) {
  spec <- attr(data, "summary_spec")
  if (is.null(spec)) {
    return(NULL)
  }
  if (!S7::S7_inherits(spec, SummarySpec)) {
    rlang::abort("Attached summary_spec is not a SummarySpec object")
  }
  spec
}

#' Build label map for summary table columns
#' @noRd
build_summary_label_map <- function() {
  list(
    model = "Model",
    based_on = "Reference",
    description = "Description",
    n_parameters = "No. Params",
    problem = "Problem",
    number_data_records = "Records",
    number_subjects = "Subjects",
    number_obs = "Observations",
    estimation_method = "Method",
    estimation_time = "Est. Time",
    covariance_time = "Cov. Time",
    postprocess_time = "Post Time",
    function_evaluations = "Func. Evals",
    significant_digits = "Sig. Digits",
    ofv = "OFV",
    dofv = gt::md("$\\Delta$OFV"),
    condition_number = "Cond. No.",
    termination_status = "Termination",
    pvalue = "p-value",
    df = "df"
  )
}

#' Create summary table from prepared data
#'
#' Creates a formatted table from summary data prepared by apply_summary_spec().
#' Supports multiple output formats: gt (default), flextable, or the
#' intermediate HyperionTable object.
#'
#' @param data Data frame from apply_summary_spec()
#' @param output Output format: "gt" (default), "flextable", or "data" for
#'   the intermediate HyperionTable object.
#'
#' @return A gt table, flextable, or HyperionTable object depending on `output`
#' @export
make_summary_table <- function(
  data,
  output = c("gt", "flextable", "data")
) {
  output <- match.arg(output)

  if (output == "flextable" && !requireNamespace("flextable", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'flextable' is required for flextable output. ",
      "Install it with 'rv add flextable'"
    ))
  }

  spec <- attr(data, "summary_spec")
  if (is.null(spec)) {
    rlang::abort(
      "SummarySpec not found. Run apply_summary_spec(tree, spec) first."
    )
  }

  # Create intermediate representation
  htable <- hyperion_summary_table(data, spec)

  # Return based on output format
  if (output == "data") {
    return(htable)
  } else if (output == "flextable") {
    return(render_to_flextable(htable))
  }

  # Default: gt output
  render_to_gt(htable)
}

#' Render summary table as gt (internal)
#'
#' Preserves the original gt rendering logic for backwards compatibility.
#'
#' @param data Data frame from apply_summary_spec()
#' @param spec SummarySpec object
#' @return gt table object
#' @noRd
render_gt_summary_table <- function(data, spec) {
  htable <- hyperion_summary_table(data, spec)
  render_to_gt(htable)
}

#' Get time format suffix for column labels
#' @noRd
get_time_suffix <- function(time_format, data) {
  if (time_format == "seconds") {
    return("s")
  } else if (time_format == "minutes") {
    return("min")
  } else if (time_format == "hours") {
    return("h")
  } else if (time_format == "auto") {
    unit <- attr(data, "summary_time_unit")
    if (!is.null(unit)) {
      return(unit)
    }
    # Determine which format was used based on data values
    time_cols <- intersect(
      c("estimation_time", "covariance_time", "postprocess_time"),
      names(data)
    )
    if (length(time_cols) == 0) return("s")

    max_vals <- vapply(
      time_cols,
      function(col) max(data[[col]], na.rm = TRUE),
      numeric(1)
    )
    max_val <- max(max_vals, na.rm = TRUE)

    if (is.na(max_val) || max_val == 0) {
      return("s")
    }
    if (max_val < 1) {
      return("h")
    }
    if (max_val < 60) {
      return("min")
    }
    return("s")
  }

  NULL
}
