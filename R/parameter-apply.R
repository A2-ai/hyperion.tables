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
  validate_apply_params(params)

  df <- compute_derived_columns(params, spec, info)
  df <- maybe_enrich_description(df, spec, info)
  df <- resolve_name_columns(df, spec, info)
  df <- apply_sections_and_filters(df, spec)

  attach_hyperion_spec(df, spec, TableSpec)
}

#' Compute derived columns (transforms, CV, RSE, CI, symbol)
#' @noRd
compute_derived_columns <- function(params, spec, info) {
  dt_kinds <- build_display_transforms(spec)
  col_values <- unlist(spec@display_transforms)

  dt_exprs <- lapply(names(dt_kinds), function(group) {
    kinds <- dt_kinds[[group]]
    rlang::expr(dplyr::if_else(
      .data$kind %in% !!kinds,
      .data$transforms,
      "identity"
    ))
  }) |>
    stats::setNames(paste0("dt_", names(dt_kinds)))

  dt_for <- function(col) {
    if (col %in% col_values) paste0("dt_", col) else "dt_all"
  }

  if (!is.null(info)) {
    transforms_vec <- get_parameter_transform(info, params$name, params$kind)
    unit_vec <- get_parameter_unit(info, params$name, params$kind)
    transforms_vec <- coalesce_missing_transforms(transforms_vec, params$name)
  } else {
    transforms_vec <- rep("identity", nrow(params))
    unit_vec <- rep(NA_character_, nrow(params))
  }

  params |>
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
}

#' Validate the params data frame passed to apply_table_spec
#'
#' The spec side has thorough validators; this guards the data side so a wrong
#' input fails with an actionable message instead of a deep dplyr error.
#' @noRd
validate_apply_params <- function(params) {
  if (!is.data.frame(params)) {
    rlang::abort(c(
      "`params` must be a data frame.",
      i = "Did you pass the result of `hyperion::get_parameters(model)`?"
    ))
  }
  required <- c("name", "kind", "estimate", "stderr", "fixed", "diagonal", "random_effect")
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    rlang::abort(c(
      sprintf("`params` is missing required column(s): %s", paste(missing, collapse = ", ")),
      i = "Did you pass the result of `hyperion::get_parameters(model)`?"
    ))
  }
  invisible(params)
}

#' Coalesce NA transforms to "identity" with a warning naming the parameters
#'
#' `hyperion::get_parameter_transform()` can return NA (e.g. a name collision
#' across kinds). Passing NA into the compute functions crashes with an opaque
#' index error, so degrade to an untransformed display and tell the user.
#' @noRd
coalesce_missing_transforms <- function(transforms_vec, names_vec) {
  na_idx <- which(is.na(transforms_vec))
  if (length(na_idx) > 0) {
    rlang::warn(sprintf(
      "No transform found for parameter(s) %s; displaying untransformed values.",
      paste(names_vec[na_idx], collapse = ", ")
    ))
    transforms_vec[na_idx] <- "identity"
  }
  transforms_vec
}

#' Add description column if requested (before name transformation)
#' @noRd
maybe_enrich_description <- function(df, spec, info) {
  want_description <- "description" %in%
    get_spec_columns(spec) &&
    !"description" %in% spec@drop_columns

  if (!want_description) {
    return(df)
  }

  if (is.null(info)) {
    rlang::warn(paste0(
      "description requires a ModelComments object. ",
      "Descriptions will not be available."
    ))
    df$description <- NA_character_
    return(df)
  }

  enrich_description(df, info)
}

#' Resolve nonmem_name/user_name columns and apply name source
#' @noRd
resolve_name_columns <- function(df, spec, info) {
  if (!is.null(info)) {
    labels <- get_parameter_names(info)

    # Match each row independently by NONMEM name, then by display name, keyed on
    # (name, kind) so a THETA and an OMEGA/SIGMA sharing a display name do not
    # collide. An all-or-nothing fallback would corrupt every commented row as
    # soon as a single parameter is uncommented (its name is its NONMEM name).
    labels_kind <- toupper(sub("^([A-Za-z]+).*", "\\1", rownames(labels)))
    query_key <- paste(df$name, df$kind)
    by_nonmem <- match(query_key, paste(rownames(labels), labels_kind))
    by_user <- match(query_key, paste(labels$name, labels_kind))
    match_idx <- ifelse(is.na(by_nonmem), by_user, by_nonmem)

    df$nonmem_name <- rownames(labels)[match_idx]
    df$user_name <- labels$name[match_idx]

    df$nonmem_name <- dplyr::if_else(
      is.na(df$nonmem_name),
      df$name,
      df$nonmem_name
    )
    df$user_name <- dplyr::if_else(is.na(df$user_name), df$name, df$user_name)

    df <- apply_name_source(df, info, spec@parameter_names)
  } else {
    df$nonmem_name <- df$name
    df$user_name <- df$name

    if (spec@parameter_names@source != "nonmem") {
      rlang::warn(paste0(
        "parameter_names source '",
        spec@parameter_names@source,
        "' requires a ModelComments object. ",
        "Using NONMEM names instead."
      ))
    }
  }

  df
}

#' Filter rows by section filter
#'
#' Reads `spec@sections@filter` (a list with at most one named entry, `keep`
#' or `exclude`). `NA` inside the value also targets rows whose section
#' didn't match any rule. Empty list = no filter.
#'
#' @param df Data frame with a `section` column
#' @param spec A TableSpec or SummarySpec
#' @return Filtered data frame
#' @noRd
filter_sections <- function(df, spec) {
  filter <- spec@sections@filter
  if (length(filter) == 0L || !"section" %in% names(df)) {
    return(df)
  }
  mode <- names(filter)
  labels <- filter[[1]]
  has_na <- any(is.na(labels))
  named <- labels[!is.na(labels)]
  n_before <- nrow(df)
  available_sections <- unique(stats::na.omit(df$section))

  if (mode == "exclude") {
    if (length(named) > 0) {
      df <- dplyr::filter(df, !.data$section %in% named)
    }
    if (has_na) {
      df <- dplyr::filter(df, !is.na(.data$section))
    }
  } else if (mode == "keep") {
    cond <- df$section %in% named
    if (has_na) {
      cond <- cond | is.na(df$section)
    }
    df <- df[cond, , drop = FALSE]
  } else {
    rlang::abort(paste0(
      "Unknown section_filter mode: '",
      mode,
      "'. Expected 'exclude' or 'keep'."
    ))
  }

  unmatched <- setdiff(named, available_sections)
  if (n_before > 0L && length(unmatched) > 0L) {
    rlang::warn(paste0(
      "section_filter ",
      mode,
      " label(s) not present in the data: ",
      paste(shQuote(unmatched), collapse = ", "),
      ". Available sections: ",
      if (length(available_sections)) {
        paste(shQuote(available_sections), collapse = ", ")
      } else {
        "<none>"
      },
      "."
    ))
  }
  if (nrow(df) == 0L && n_before > 0L) {
    rlang::warn(paste0(
      "section_filter (",
      mode,
      " = ",
      paste(deparse(labels), collapse = " "),
      ") removed every row."
    ))
  }
  df
}

#' Apply section assignments and row filters
#' @noRd
apply_sections_and_filters <- function(df, spec) {
  df <- df |>
    dplyr::mutate(
      section = build_section(dplyr::pick(dplyr::everything()), spec)
    )

  df <- apply_lookup_section_overrides(df, spec)

  df <- filter_sections(df, spec)

  if (length(spec@row_filter) > 0) {
    for (f in spec@row_filter) {
      df <- df |>
        dplyr::filter(!!f)
    }
  }

  df
}

#' Override `section` per-parameter from merged assignments
#'
#' Walks `spec@sections@assignments` (file + inline already merged at
#' setter time). Parameter rows are matched by `user_name` first (the
#' comment-name like "TVCL"), falling back to `nonmem_name` ("THETA1").
#'
#' @noRd
apply_lookup_section_overrides <- function(df, spec) {
  assignments <- spec@sections@assignments
  if (length(assignments) == 0L) {
    return(df)
  }

  items <- unlist(assignments, use.names = FALSE)
  labels <- rep(names(assignments), lengths(assignments))
  section_map <- stats::setNames(labels, items)

  res <- assign_section_overrides(df, section_map)
  inline_unmatched <- intersect(res$unmatched, spec@sections@inline_items)
  if (length(inline_unmatched) > 0L) {
    rlang::warn(paste0(
      "Inline `parameters` section override(s) did not match any parameter: ",
      paste(shQuote(inline_unmatched), collapse = ", "),
      "."
    ))
  }
  res$data
}

#' Apply a name -> section map onto df$section, matching user_name first,
#' nonmem_name second. Returns the data frame and the indices of map keys
#' that did not match any row, so the caller can warn with appropriate
#' context (file vs inline).
#' @noRd
assign_section_overrides <- function(df, section_map) {
  match_keys <- if ("user_name" %in% names(df)) df$user_name else df$name
  hits <- match(match_keys, names(section_map))
  if ("nonmem_name" %in% names(df)) {
    miss <- is.na(hits)
    if (any(miss)) {
      hits[miss] <- match(df$nonmem_name[miss], names(section_map))
    }
  }
  matched <- !is.na(hits)
  if (any(matched)) {
    df$section[matched] <- unname(section_map[hits[matched]])
  }
  unmatched_keys <- setdiff(
    names(section_map),
    names(section_map)[unique(stats::na.omit(hits))]
  )
  list(data = df, unmatched = unmatched_keys)
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

  # dt_all is the fallback transform mapping for columns not named in any kind's
  # list. A column should be transformed only for kinds that opted in via "all";
  # if no kind used "all", unlisted columns must stay untransformed rather than
  # be transformed for every kind.
  if (!"all" %in% names(dt_kinds)) {
    dt_kinds[["all"]] <- character(0)
  }

  dt_kinds
}

#' Build section assignments using case_when
#' @noRd
build_section <- function(data, spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("spec must be a TableSpec object")
  }

  rules <- spec@sections@rules
  if (length(rules) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  # Convert quosures to case_when format
  # Each quosure wraps a formula like: kind == "THETA" ~ "Structural model parameters"
  args <- lapply(rules, function(q) {
    rlang::eval_tidy(q, data = data)
  })

  warn_multi_match_sections(args, data)

  dplyr::case_when(!!!args)
}

#' Resolve a section rule's label (formula RHS) to a character scalar
#'
#' The RHS may be a string literal or a non-literal expression (a variable or
#' call) that the SectionOptions validator explicitly supports. Evaluate it in
#' the formula's environment; fall back to deparsing if evaluation does not yield
#' a character scalar.
#' @noRd
resolve_rule_label <- function(f, env = rlang::f_env(f)) {
  rhs <- rlang::f_rhs(f)
  val <- tryCatch(eval(rhs, envir = env), error = function(e) NULL)
  if (is.character(val) && length(val) == 1L) {
    return(val)
  }
  rlang::as_label(rhs)
}

#' Warn when rows match multiple non-catch-all section rules
#' @noRd
warn_multi_match_sections <- function(formulas, data) {
  labels <- vapply(formulas, resolve_rule_label, character(1))
  is_catchall <- vapply(
    formulas,
    function(f) identical(rlang::f_lhs(f), TRUE),
    logical(1)
  )
  nc_idx <- which(!is_catchall)
  if (length(nc_idx) < 2) {
    return(invisible())
  }

  lhs_results <- lapply(nc_idx, function(j) {
    tryCatch(
      eval(
        rlang::f_lhs(formulas[[j]]),
        envir = data,
        enclos = rlang::f_env(formulas[[j]])
      ),
      error = function(e) rep(FALSE, nrow(data))
    )
  })

  nc_labels <- labels[nc_idx]

  # Only flag rows where matched labels are genuinely different
  multi_rows <- which(vapply(
    seq_len(nrow(data)),
    function(i) {
      matched <- vapply(lhs_results, function(r) isTRUE(r[i]), logical(1))
      length(unique(nc_labels[matched])) > 1
    },
    logical(1)
  ))
  if (length(multi_rows) == 0) {
    return(invisible())
  }

  msgs <- vapply(
    multi_rows,
    function(i) {
      matched <- vapply(lhs_results, function(r) isTRUE(r[i]), logical(1))
      row_id <- if ("name" %in% names(data)) data$name[i] else as.character(i)
      sprintf(
        "'%s' matches: %s",
        row_id,
        paste(sprintf("'%s'", nc_labels[matched]), collapse = ", ")
      )
    },
    character(1)
  )

  rlang::warn(c(
    "Rows matched multiple section rules; first match used:",
    stats::setNames(msgs, rep("*", length(msgs)))
  ))
}

#' Get section order from spec
#' @noRd
get_section_order <- function(spec) {
  vapply(
    spec@sections@rules,
    function(rule) {
      resolve_rule_label(rlang::eval_tidy(rule), rlang::quo_get_env(rule))
    },
    character(1)
  )
}

#' Resolve final section levels, honoring `set_spec_section_order()` if set,
#' otherwise falling back to spec rule declaration order with TOML-introduced
#' labels appended in encounter order. Returns a list with the (possibly
#' filtered) data and the level vector.
#' @noRd
resolve_section_levels <- function(data, spec) {
  override <- spec@sections@order
  if (length(override) > 0L) {
    levels <- as.character(override)
    extra <- setdiff(unique(stats::na.omit(data$section)), levels)
    levels <- c(levels, extra)
  } else {
    spec_levels <- unique(get_section_order(spec))
    extra <- setdiff(unique(stats::na.omit(data$section)), spec_levels)
    levels <- c(spec_levels, extra)
  }
  list(data = data, levels = levels)
}

#' @noRd
comment_keys_for <- function(nonmem, comment) {
  keys <- c(nonmem)

  if (!is.null(comment@name)) {
    keys <- c(keys, comment@name)
  }

  if (
    S7::S7_inherits(comment, OmegaComment) &&
      !is.null(comment@raw_name) &&
      !is.na(comment@raw_name)
  ) {
    keys <- c(keys, comment@raw_name)
  }

  if (!is.null(comment@display)) {
    keys <- c(keys, comment@display)
  }

  keys
}

#' @noRd
build_name_lookup <- function(info, parameter_names) {
  source <- parameter_names@source

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

  build_lookup_rows <- function(comments, kind_label) {
    lapply(names(comments), function(nonmem) {
      cmt <- comments[[nonmem]]
      target <- get_raw_name(cmt, nonmem)

      keys <- comment_keys_for(nonmem, cmt)

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
      if (is.null(desc)) {
        desc <- NA_character_
      }

      keys <- comment_keys_for(nonmem, cmt)

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
