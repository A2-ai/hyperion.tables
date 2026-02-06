# ==============================================================================
# Apply spec to parameter data
# ==============================================================================

#' Apply table specification to parameter data
#'
#' Enriches parameter data with transforms, CIs, sections, and display names.
#'
#' @param params Data frame from get_parameters()
#' @param spec A TableSpec object
#' @param info ModelComments object from get_model_parameter_info(), or NULL.
#'   If NULL, features that require ModelComments (transforms, units,
#'   descriptions, custom name sources) will not be available and warnings
#'   will be issued if requested.
#' @importFrom rlang .data
#'
#' @return Enriched data frame ready for table building
#' @export
apply_table_spec <- function(params, spec, info = NULL) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("spec must be a TableSpec object")
  }
  if (!is.null(info) && !S7::S7_inherits(info, ModelComments)) {
    rlang::abort("info must be a ModelComments object or NULL")
  }

  dt_kinds <- build_display_transforms(spec)
  col_values <- unlist(spec@display_transforms)

  # Build dt_* column expressions
  dt_exprs <- lapply(names(dt_kinds), function(group) {
    kinds <- dt_kinds[[group]]
    rlang::expr(dplyr::if_else(
      .data$kind %in% !!kinds,
      .data$transforms,
      "identity"
    ))
  }) |>
    stats::setNames(paste0("dt_", names(dt_kinds)))

  # Helper to get the right dt column for a given output column
  dt_for <- function(col) {
    if (col %in% col_values) paste0("dt_", col) else "dt_all"
  }

  # Handle transforms and unit based on whether info is provided
  if (!is.null(info)) {
    transforms_vec <- get_parameter_transform(info, params$name, params$kind)
    unit_vec <- get_parameter_unit(info, params$name, params$kind)
  } else {
    transforms_vec <- rep("identity", nrow(params))
    unit_vec <- rep(NA_character_, nrow(params))
  }

  df <- params |>
    dplyr::mutate(
      transforms = transforms_vec,
      unit = unit_vec,
      !!!dt_exprs,
      cv = compute_cv(.data$estimate, .data$kind, .data[[dt_for("cv")]]),
      rse = compute_rse(
        .data$estimate,
        .data$stderr,
        .data$kind,
        .data[[dt_for("rse")]]
      ),
      ci_low = compute_ci(
        .data$estimate,
        .data$stderr,
        spec@ci@level,
        .data[[dt_for("ci")]]
      )$lower,
      ci_high = compute_ci(
        .data$estimate,
        .data$stderr,
        spec@ci@level,
        .data[[dt_for("ci")]]
      )$upper,
      estimate = transform_value(.data$estimate, .data[[dt_for("estimate")]]),
      symbol = param_symbol_md(
        .data$kind,
        .data$random_effect,
        .data[[dt_for("symbol")]]
      )
    )

  # Add description column FIRST (before name transformation)
  # This ensures we match on original/untransformed names
  want_description <- "description" %in%
    c(spec@columns, spec@add_columns %||% character(0)) &&
    !"description" %in% spec@drop_columns
  if (want_description) {
    if (is.null(info)) {
      rlang::warn(paste0(
        "description requires a ModelComments object. ",
        "Descriptions will not be available."
      ))
      df$description <- NA_character_
    } else {
      df <- enrich_description(df, info)
    }
  }

  # Add nonmem_name and user_name columns for filtering/sectioning
  if (!is.null(info)) {
    # get_parameter_names returns df with rownames = nonmem_name, columns = name, display
    labels <- get_parameter_names(info)

    # Match params to ModelComments by the current name (could be nonmem or user name)
    match_idx <- match(df$name, rownames(labels)) # Try nonmem_name first
    if (all(is.na(match_idx))) {
      match_idx <- match(df$name, labels$name) # Try user_name
    }

    df$nonmem_name <- rownames(labels)[match_idx]
    df$user_name <- labels$name[match_idx]

    # Fallback to current name if no match
    df$nonmem_name <- ifelse(is.na(df$nonmem_name), df$name, df$nonmem_name)
    df$user_name <- ifelse(is.na(df$user_name), df$name, df$user_name)
  } else {
    # No ModelComments - use current name for both
    df$nonmem_name <- df$name
    df$user_name <- df$name
  }

  # Apply name replacement based on spec@parameter_names
  if (!is.null(info)) {
    df <- apply_name_source(df, info, spec@parameter_names)
  } else if (spec@parameter_names@source != "nonmem") {
    rlang::warn(paste0(
      "parameter_names source '",
      spec@parameter_names@source,
      "' requires a ModelComments object. ",
      "Using NONMEM names instead."
    ))
  }

  # Apply section rules AFTER name transformation (consistent with row_filter)
  df <- df |>
    dplyr::mutate(
      section = build_section(dplyr::pick(dplyr::everything()), spec)
    )

  # Apply row filter AFTER name transformation so users can filter on display names
  if (length(spec@row_filter) > 0) {
    for (f in spec@row_filter) {
      df <- df |>
        dplyr::filter(!!f)
    }
  }

  attr(df, "table_spec") <- spec
  df
}

# ==============================================================================
# TableSpec helper functions
# ==============================================================================

#' Build display transform mapping from spec
#' @noRd
build_display_transforms <- function(spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("spec must be a TableSpec object")
  }

  dt <- spec@display_transforms
  groups <- unique(unlist(dt))

  dt_kinds <- lapply(groups, function(group) {
    kinds <- names(dt)[vapply(
      dt,
      function(x) {
        !is.null(x) && ("all" %in% x || group %in% x)
      },
      logical(1)
    )]
    toupper(kinds)
  }) |>
    stats::setNames(groups)

  # Always provide dt_all as a fallback transform mapping for every kind
  if (!"all" %in% names(dt_kinds)) {
    dt_kinds[["all"]] <- toupper(names(dt))
  }

  dt_kinds
}

#' Build section assignments using case_when
#' @noRd
build_section <- function(data, spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("spec must be a TableSpec object")
  }

  rules <- spec@sections
  if (length(rules) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  # Convert quosures to case_when format
  # Each quosure wraps a formula like: kind == "THETA" ~ "Structural model parameters"
  args <- lapply(rules, function(q) {
    rlang::eval_tidy(q, data = data)
  })

  dplyr::case_when(!!!args)
}

#' Get section order from spec
#' @noRd
get_section_order <- function(spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("spec must be a TableSpec object")
  }

  vapply(
    spec@sections,
    function(rule) {
      rlang::f_rhs(rlang::eval_tidy(rule))
    },
    character(1)
  )
}

#' @noRd
comment_keys_for <- function(nonmem, comment, include_associated_theta = TRUE) {
  keys <- c(nonmem)

  if (!is.null(comment@name)) {
    keys <- c(keys, comment@name)

    if (
      include_associated_theta &&
        S7::S7_inherits(comment, OmegaComment) &&
        !is.null(comment@associated_theta)
    ) {
      theta_str <- paste(comment@associated_theta, collapse = "-")
      keys <- c(keys, paste0(comment@name, " (", theta_str, ")"))
    }
  }

  if (!is.null(comment@display)) {
    keys <- c(keys, comment@display)
  }

  keys
}

#' @noRd
build_name_lookup <- function(info, parameter_names) {
  source <- parameter_names@source
  append_omega <- parameter_names@append_omega_with_theta

  # Helper to get raw name from a comment based on source
  get_raw_name <- function(cmt, nonmem_name) {
    if (source == "nonmem") {
      nonmem_name
    } else if (
      source == "display" &&
        !is.null(cmt@display) &&
        !is.na(cmt@display)
    ) {
      cmt@display
    } else if (!is.null(cmt@name) && !is.na(cmt@name)) {
      cmt@name
    } else {
      nonmem_name
    }
  }

  # Helper to get theta label based on source
  # theta_name is the @name from associated_theta (e.g., "TVV")
  get_theta_label <- function(theta_name) {
    # Find the theta comment by searching for matching @name
    theta_cmt <- NULL
    theta_nonmem <- NULL
    for (nm in names(info@theta)) {
      if (
        !is.na(info@theta[[nm]]@name) && info@theta[[nm]]@name == theta_name
      ) {
        theta_cmt <- info@theta[[nm]]
        theta_nonmem <- nm
        break
      }
    }

    if (is.null(theta_cmt)) {
      # Not found - return as-is

      return(theta_name)
    }

    if (source == "nonmem") {
      return(theta_nonmem)
    } else if (source == "display") {
      # Use display if available, otherwise name
      if (!is.null(theta_cmt@display) && !is.na(theta_cmt@display)) {
        return(theta_cmt@display)
      }
    }
    # Default: use the theta's @name
    theta_name
  }

  build_lookup_rows <- function(comments, kind_label) {
    lapply(names(comments), function(nonmem) {
      cmt <- comments[[nonmem]]
      target <- get_raw_name(cmt, nonmem)

      # For omega with append_omega_with_theta = TRUE, add theta info
      if (
        append_omega &&
          S7::S7_inherits(cmt, OmegaComment) &&
          !is.null(cmt@associated_theta)
      ) {
        theta_lbls <- vapply(
          cmt@associated_theta,
          get_theta_label,
          character(1)
        )
        names(theta_lbls) <- cmt@associated_theta

        target <- format_omega_display_name(
          name = target,
          associated_theta = cmt@associated_theta,
          theta_labels = theta_lbls
        )
      }

      keys <- comment_keys_for(nonmem, cmt, include_associated_theta = TRUE)

      data.frame(
        key = keys,
        display = target,
        kind = kind_label,
        stringsAsFactors = FALSE
      )
    }) |>
      dplyr::bind_rows()
  }

  dplyr::bind_rows(
    build_lookup_rows(info@theta, "THETA"),
    build_lookup_rows(info@omega, "OMEGA"),
    build_lookup_rows(info@sigma, "SIGMA")
  ) |>
    dplyr::distinct(.data$key, .data$kind, .keep_all = TRUE)
}

#' Apply name source replacement
#'
#' Replaces parameter names based on the parameter_names settings.
#'
#' @param df Data frame with name and kind columns
#' @param info ModelComments object
#' @param parameter_names ParameterNameOptions object
#' @return Data frame with names replaced
#' @noRd
apply_name_source <- function(df, info, parameter_names) {
  lookup <- build_name_lookup(info, parameter_names)

  df |>
    dplyr::mutate(
      .match_idx = match(
        paste(.data$name, .data$kind),
        paste(lookup$key, lookup$kind)
      ),
      .display = lookup$display[.data$.match_idx],
      name = dplyr::coalesce(.data$.display, .data$name)
    ) |>
    dplyr::select(-".match_idx", -".display")
}

#' Enrich description column from ModelComments
#'
#' Adds a description column by matching parameter names to ModelComments.
#'
#' @param df Data frame with name and kind columns
#' @param info ModelComments object
#' @return Data frame with description column added
#' @noRd
enrich_description <- function(df, info) {
  build_desc_rows <- function(comments, kind_label) {
    lapply(names(comments), function(nonmem) {
      cmt <- comments[[nonmem]]
      desc <- cmt@description
      if (is.null(desc)) desc <- NA_character_

      keys <- comment_keys_for(nonmem, cmt, include_associated_theta = TRUE)

      data.frame(
        key = keys,
        description = desc,
        kind = kind_label,
        stringsAsFactors = FALSE
      )
    }) |>
      dplyr::bind_rows()
  }

  lookup <- dplyr::bind_rows(
    build_desc_rows(info@theta, "THETA"),
    build_desc_rows(info@omega, "OMEGA"),
    build_desc_rows(info@sigma, "SIGMA")
  ) |>
    dplyr::distinct(.data$key, .data$kind, .keep_all = TRUE)

  df |>
    dplyr::mutate(
      .match_idx = match(
        paste(.data$name, .data$kind),
        paste(lookup$key, lookup$kind)
      ),
      description = lookup$description[.data$.match_idx]
    ) |>
    dplyr::select(-".match_idx")
}
