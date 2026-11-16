# ==============================================================================
# SCM Summary Tables
# ==============================================================================
# ScmSummarySpec and the hooks apply_summary_spec() and make_summary_table()
# call for it. Any other SummarySpec passes through the hooks unchanged.

# ==============================================================================
# SCM Spec Classes
# ==============================================================================

#' Selected candidate display options for SCM summary tables
#'
#' Controls how an SCM summary table shows the candidate each round
#' selected: the covariate a forward round added, or the one a backward
#' round dropped (the fit tagged `"selected"`). The settings can be
#' combined. Usually set with [set_spec_scm_selected()].
#'
#' With `selected_only = TRUE` the table keeps the milestones (the base or
#' full model, the working full model and the reduced covariate model) and
#' the selected candidate of each round. The rounds are then grouped by
#' direction, under "Forward" and "Backward", instead of one section per
#' round. The working full model is left out when no backward round
#' dropped a covariate, since it then matches the reduced covariate model.
#'
#' @param column Logical. If TRUE (the default), the table has a Round
#'   Result column that reads "added" or "dropped" on the selected
#'   candidate's row. The column is omitted when the table shows no round
#'   candidates.
#' @param bold Logical. If TRUE, the selected candidate's row is rendered in
#'   bold. Default `FALSE`.
#' @param selected_only Logical. If TRUE, the table shows only the selected
#'   candidates, with the rounds grouped by direction. Default `FALSE`.
#'
#' @section Properties:
#' The following properties are available on a `ScmSelectedOptions` object:
#' \itemize{
#'   \item `column` - Whether the table has a Round Result column.
#'   \item `bold` - Whether the selected candidate's row is bold.
#'   \item `selected_only` - Whether the table shows only the selected
#'     candidates.
#' }
#'
#' @seealso [set_spec_scm_selected()], [scm_summary_spec()].
#' @export
#' @examples
#' ScmSelectedOptions()
#' ScmSelectedOptions(bold = TRUE)
#' ScmSelectedOptions(column = FALSE)
#' ScmSelectedOptions(selected_only = TRUE)
ScmSelectedOptions <- S7::new_class(
  "ScmSelectedOptions",
  package = "hyperion.tables",
  properties = list(
    column = S7::new_property(class = S7::class_logical, default = TRUE),
    bold = S7::new_property(class = S7::class_logical, default = FALSE),
    selected_only = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    )
  ),
  validator = function(self) {
    for (flag in c("column", "bold", "selected_only")) {
      value <- S7::prop(self, flag)
      if (length(value) != 1 || is.na(value)) {
        return(sprintf("@%s must be TRUE or FALSE.", flag))
      }
    }
  }
)

#' Summary specification for SCM run summary tables
#'
#' `ScmSummarySpec` is a child class of [SummarySpec] for the run summary
#' table of a stepwise covariate modeling (SCM) process. It works with
#' everything that takes a `SummarySpec`, including [apply_summary_spec()]
#' and the `set_spec_*()` modifiers, and adds the settings specific to an
#' SCM table: a section per round, the Round Result column, bold rows for
#' the selected candidates, a choice of rounds, and p-values for backward
#' steps. The SCM modifiers, [set_spec_scm_selected()] and
#' [set_spec_scm_rounds()], accept only a `ScmSummarySpec`. Applying one to
#' a lineage with no SCM fits is an error.
#'
#' Use [scm_summary_spec()] to create one.
#'
#' @param show_all_covariates Logical. If TRUE, the Description column also
#'   lists the covariates in each fit's model, after what the fit added or
#'   dropped. Default `FALSE`.
#' @param selected A [ScmSelectedOptions()] object controlling how the
#'   candidate each round selected is shown. Prefer
#'   [set_spec_scm_selected()] in a pipe for user-facing code.
#' @param rounds Rounds to show: `"all"` (the default) for every round; a
#'   character vector of round names (`"forward_round1"`) and directions
#'   (`"forward"`, `"backward"`); or `"none"` or `NULL` for no round, which
#'   leaves only the milestones. Prefer [set_spec_scm_rounds()] in a pipe
#'   for user-facing code.
#' @param other_section_label Section label for models matching no round.
#' @param ... Arguments passed to [SummarySpec()]. Those not given take the
#'   SCM table defaults: no title, the fits tagged `"scm"`, the columns
#'   Reference, Description, OFV, dOFV and p-value, and no footnotes.
#'
#' @seealso [scm_summary_spec()], [SummarySpec].
#' @export
#' @examples
#' spec <- ScmSummarySpec(show_all_covariates = TRUE)
#' S7::S7_inherits(spec, SummarySpec)
ScmSummarySpec <- S7::new_class(
  "ScmSummarySpec",
  parent = SummarySpec,
  properties = list(
    show_all_covariates = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),
    selected = S7::new_property(
      class = ScmSelectedOptions,
      default = ScmSelectedOptions()
    ),
    rounds = S7::new_property(
      class = S7::class_character,
      default = "all"
    ),
    other_section_label = S7::new_property(
      class = S7::class_character,
      default = "Other"
    )
  ),
  validator = function(self) {
    rounds_msg <- validate_scm_rounds(self@rounds)
    if (!is.null(rounds_msg)) {
      return(rounds_msg)
    }
    if (
      length(self@show_all_covariates) != 1 ||
        is.na(self@show_all_covariates)
    ) {
      return("@show_all_covariates must be TRUE or FALSE.")
    }
    if (
      length(self@other_section_label) != 1 ||
        is.na(self@other_section_label)
    ) {
      return("@other_section_label must be a single character string.")
    }
  },
  constructor = function(
    show_all_covariates = FALSE,
    selected = ScmSelectedOptions(),
    rounds = "all",
    other_section_label = "Other",
    ...
  ) {
    # Store `rounds = NULL` (no rounds) as "none"
    if (is.null(rounds)) {
      rounds <- "none"
    }
    # SCM table defaults for SummarySpec arguments not given
    summary_args <- list(...)
    defaults <- list(
      title = "",
      tag_filter = "scm",
      columns = c("based_on", "description", "ofv", "dofv", "pvalue"),
      footnote_order = NULL
    )
    for (name in setdiff(names(defaults), names(summary_args))) {
      summary_args[name] <- list(defaults[[name]])
    }
    S7::new_object(
      do.call(SummarySpec, summary_args),
      show_all_covariates = show_all_covariates,
      selected = selected,
      rounds = rounds,
      other_section_label = other_section_label
    )
  }
)

#' Validate the rounds of an ScmSummarySpec
#'
#' @param rounds "all", "none", or a character vector of round names and
#'   directions
#' @return NULL if valid, otherwise an error message
#' @noRd
validate_scm_rounds <- function(rounds) {
  if (identical(rounds, "all") || identical(rounds, "none")) {
    return(NULL)
  }
  is_round <- grepl("^(forward|backward)(_round[0-9]+)?$", rounds)
  if (length(rounds) == 0 || anyNA(rounds) || !all(is_round)) {
    bad <- rounds[is.na(rounds) | !is_round]
    return(paste0(
      "@rounds must name rounds (\"forward_round1\") or directions ",
      "(\"forward\", \"backward\"), or be \"all\" for every round or \"none\" for none.",
      if (length(bad) > 0) {
        paste0(" Got: ", paste0("'", bad, "'", collapse = ", "))
      }
    ))
  }
  NULL
}

#' @noRd
is_scm_summary_spec <- function(spec) {
  S7::S7_inherits(spec, ScmSummarySpec)
}

# ==============================================================================
# SCM Spec Preset
# ==============================================================================

#' Pre-configured ScmSummarySpec for an SCM run summary table
#'
#' `scm_summary_spec()` returns a [ScmSummarySpec] configured for the run
#' summary table of a stepwise covariate modeling (SCM) process: the fits
#' tagged `"scm"`, grouped into one section per round in the order the
#' process ran them, with the columns Reference, Description, OFV, dOFV and
#' p-value and no footnotes. The returned spec can be customized further
#' with the `set_spec_*()` modifiers.
#'
#' The sections are built when the spec is applied, from the round tags of
#' the lineage passed to [apply_summary_spec()] (`"base"` or `"full"`,
#' `"forward_round1"`, ..., `"working_full"`, `"reduced_covariate"`), so the
#' spec does not take the lineage itself. Section rules set with
#' [set_spec_sections()] replace the generated ones.
#'
#' Each fit is based on the model its round tested against, so dOFV and the
#' p-value are those of its round. Backward steps get a p-value too, with
#' the round's reference as the fuller model. The working full and reduced
#' covariate models are based on the reference fit.
#'
#' The Description column shows what each fit added or dropped (e.g., "add
#' CRCL_CL"). It is empty for the milestones (the base or full model, the
#' working full model and the reduced covariate model), which their section
#' already names. With `show_all_covariates = TRUE` it also lists the
#' covariates in each fit's model (e.g., "add CRCL_CL; covariates: WT_CL,
#' CRCL_CL"). The covariates come from the SCM process record, so this needs
#' a lineage from `hyperion::get_scm_lineage()`. For a lineage from
#' `hyperion::get_model_lineage()` nothing is added and a warning is raised.
#'
#' @param title Table title. `NULL` (the default) gives a table with no
#'   title; pass a string to set one.
#' @param show_all_covariates Logical. If TRUE, the Description column also
#'   lists the covariates in each fit's model, after what the fit added or
#'   dropped. Default `FALSE`.
#' @param selected A [ScmSelectedOptions()] object controlling how the
#'   candidate each round selected is shown: marked with a Round Result
#'   column (the default) or a bold row, or shown alone. See
#'   [set_spec_scm_selected()].
#' @param rounds Rounds to show: `"all"` (the default) for every round; a
#'   character vector of round names (`"forward_round1"`) and directions
#'   (`"forward"`, `"backward"`); or `"none"` or `NULL` for no round, which
#'   leaves only the milestones. The milestones are always shown, and every
#'   row keeps the dOFV and p-value it has in the full table. See
#'   [set_spec_scm_rounds()].
#' @param other_section_label Section label for models matching no round.
#'
#' @return A [ScmSummarySpec] object.
#' @seealso [ScmSummarySpec], [set_spec_scm_selected()], [set_spec_scm_rounds()],
#'   [ScmSelectedOptions()], [apply_summary_spec()], [make_summary_table()].
#' @export
#' @examples \dontrun{
#' # From the SCM process record
#' scm <- hyperion::read_scm("model/nonmem/scm/run001scm.toml")
#' scm |>
#'   hyperion::get_scm_lineage() |>
#'   apply_summary_spec(scm_summary_spec(title = "Covariate search")) |>
#'   make_summary_table()
#'
#' # From the fits' metadata, like any other lineage
#' hyperion::get_model_lineage() |>
#'   apply_summary_spec(scm_summary_spec()) |>
#'   make_summary_table()
#'
#' # List the covariates in each fit's model
#' scm_summary_spec(show_all_covariates = TRUE)
#'
#' # Show the selected candidates in bold, or without the Round Result column
#' scm_summary_spec(selected = ScmSelectedOptions(bold = TRUE))
#' scm_summary_spec(selected = ScmSelectedOptions(column = FALSE))
#'
#' # Show only some rounds, or only the milestones
#' scm_summary_spec(rounds = c("forward_round1", "forward_round2"))
#' scm_summary_spec(rounds = "backward")
#' scm_summary_spec(rounds = "none")
#'
#' # Show only the selected candidates
#' scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE))
#'
#' # Set the same options on an existing spec
#' scm_summary_spec() |> set_spec_scm_selected(selected_only = TRUE)
#'
#' # Customize further with modifiers
#' spec <- scm_summary_spec() |>
#'   set_spec_pvalue(scientific = TRUE)
#' # Or without the p-value
#' spec <- scm_summary_spec() |>
#'   set_spec_columns("based_on", "description", "ofv", "dofv")
#' }
scm_summary_spec <- function(
  title = NULL,
  show_all_covariates = FALSE,
  selected = ScmSelectedOptions(),
  rounds = "all",
  other_section_label = "Other"
) {
  if (
    inherits(title, "hyperion_nonmem_tree") ||
      inherits(title, "hyperion_nonmem_scm")
  ) {
    rlang::abort(c(
      "`scm_summary_spec()` does not take the lineage or the SCM process.",
      i = "Pass the lineage to `apply_summary_spec()`: `lineage |> apply_summary_spec(scm_summary_spec())`."
    ))
  }
  if (!is.null(title) && !rlang::is_string(title)) {
    rlang::abort("`title` must be a single character string or NULL.")
  }
  if (!rlang::is_bool(show_all_covariates)) {
    rlang::abort("`show_all_covariates` must be TRUE or FALSE.")
  }
  if (!S7::S7_inherits(selected, ScmSelectedOptions)) {
    rlang::abort("`selected` must be a <ScmSelectedOptions> object.")
  }
  if (!is.null(rounds) && !is.character(rounds)) {
    rlang::abort(
      "`rounds` must be \"all\", \"none\" or NULL, or a character vector of round names."
    )
  }
  if (!rlang::is_string(other_section_label)) {
    rlang::abort("`other_section_label` must be a single character string.")
  }

  ScmSummarySpec(
    show_all_covariates = show_all_covariates,
    selected = selected,
    rounds = rounds,
    other_section_label = other_section_label,
    title = title %||% ""
  )
}

# ==============================================================================
# SCM Setters and Getters
# ==============================================================================

#' Set how an SCM summary table shows the selected candidates
#'
#' `set_spec_scm_selected()` sets how the table shows the candidate each
#' round selected: marked with a Round Result column, with its row in bold,
#' or shown alone without the other candidates. Settings that are not given
#' keep their current value. See [ScmSelectedOptions()] for details.
#' Operates on [ScmSummarySpec] only.
#'
#' @param spec A ScmSummarySpec object.
#' @param ... Not used.
#' @param column Logical. If TRUE, the table has a Round Result column that
#'   reads "added" or "dropped" on the selected candidate's row.
#' @param bold Logical. If TRUE, the selected candidate's row is rendered in
#'   bold.
#' @param selected_only Logical. If TRUE, the table shows only the selected
#'   candidates, with the rounds grouped by direction.
#' @return Modified spec.
#' @seealso [get_spec_scm_selected()], [ScmSelectedOptions()],
#'   [scm_summary_spec()].
#' @export
#' @examples
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_selected(bold = TRUE)
#' # Without the Round Result column
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_selected(column = FALSE)
#' # Only the selected candidates
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_selected(selected_only = TRUE)
set_spec_scm_selected <- function(
  spec,
  ...,
  column = NULL,
  bold = NULL,
  selected_only = NULL
) {
  if (!is_scm_summary_spec(spec)) {
    rlang::abort("`spec` must be a <ScmSummarySpec> object.")
  }
  rlang::check_dots_empty()
  selected_args <- list(
    column = spec@selected@column,
    bold = spec@selected@bold,
    selected_only = spec@selected@selected_only
  )
  if (!is.null(column)) {
    selected_args$column <- column
  }
  if (!is.null(bold)) {
    selected_args$bold <- bold
  }
  if (!is.null(selected_only)) {
    selected_args$selected_only <- selected_only
  }
  spec@selected <- do.call(ScmSelectedOptions, selected_args)
  spec
}

#' Get how an SCM summary table shows the selected candidates
#'
#' `get_spec_scm_selected()` returns the ScmSelectedOptions object.
#' Operates on [ScmSummarySpec] only.
#'
#' @param spec A ScmSummarySpec object.
#' @param ... Not used.
#' @return ScmSelectedOptions object.
#' @seealso [set_spec_scm_selected()].
#' @export
#' @examples
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_selected(bold = TRUE)
#' get_spec_scm_selected(spec)
get_spec_scm_selected <- function(spec, ...) {
  if (!is_scm_summary_spec(spec)) {
    rlang::abort("`spec` must be a <ScmSummarySpec> object.")
  }
  spec@selected
}

#' Set the rounds an SCM summary table shows
#'
#' `set_spec_scm_rounds()` sets which rounds the table shows, by name or by
#' direction. The milestones (the base or full model, the working full
#' model and the reduced covariate model) are always shown; `"none"` shows
#' only them. Rounds are removed after dOFV and the p-value are computed, so
#' every row keeps the values it has in the full table. A warning is raised
#' for a round the lineage does not have. Operates on [ScmSummarySpec] only.
#'
#' @param spec A ScmSummarySpec object.
#' @param ... Round names (e.g., `"forward_round1"`) and directions
#'   (`"forward"`, `"backward"`), as strings or a character vector. `"all"`,
#'   or no argument, shows every round. `"none"`, or `NULL`, shows only the
#'   milestones.
#' @return Modified spec.
#' @seealso [get_spec_scm_rounds()], [scm_summary_spec()].
#' @export
#' @examples
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_rounds("forward_round1", "forward_round2")
#' # Every backward round
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_rounds("backward")
#' # Only the milestones
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_rounds("none")
#' # Every round again
#' spec <- set_spec_scm_rounds(spec, "all")
set_spec_scm_rounds <- function(spec, ...) {
  if (!is_scm_summary_spec(spec)) {
    rlang::abort("`spec` must be a <ScmSummarySpec> object.")
  }
  # No argument means every round; an explicit NULL means no rounds
  if (...length() == 0) {
    spec@rounds <- "all"
    return(spec)
  }
  rounds <- unlist(list(...), use.names = FALSE)
  if (is.null(rounds)) {
    spec@rounds <- "none"
    return(spec)
  }
  if (!is.character(rounds)) {
    rlang::abort(
      "Rounds must be given as strings, such as \"forward_round1\" or \"forward\"."
    )
  }
  spec@rounds <- unique(rounds)
  spec
}

#' Get the rounds an SCM summary table shows
#'
#' `get_spec_scm_rounds()` returns the rounds the table shows: `"all"` for
#' every round, or `"none"` for only the milestones.
#' Operates on [ScmSummarySpec] only.
#'
#' @param spec A ScmSummarySpec object.
#' @param ... Not used.
#' @return Character vector: `"all"`, `"none"`, or round names and directions.
#' @seealso [set_spec_scm_rounds()].
#' @export
#' @examples
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_rounds("forward")
#' get_spec_scm_rounds(spec)
get_spec_scm_rounds <- function(spec, ...) {
  if (!is_scm_summary_spec(spec)) {
    rlang::abort("`spec` must be a <ScmSummarySpec> object.")
  }
  spec@rounds
}

# ==============================================================================
# SCM Hooks for apply_summary_spec() and make_summary_table()
# ==============================================================================

#' Add section rules for the rounds of an SCM lineage
#'
#' Adds one section rule per round, in the order the process ran them,
#' unless the spec already has rules. With `selected_only`, the rounds of a
#' direction share a section.
#'
#' @param spec A SummarySpec object; returned unchanged unless it is a
#'   ScmSummarySpec
#' @param tree A hyperion_nonmem_tree object
#' @return The spec with section rules set
#' @noRd
scm_spec_sections <- function(spec, tree) {
  if (!is_scm_summary_spec(spec) || length(spec@sections@rules) > 0) {
    return(spec)
  }
  rounds <- scm_lineage_rounds(tree)
  if (length(rounds) == 0) {
    rlang::abort(
      "The lineage has no SCM fits: no model is tagged \"scm\" with a round."
    )
  }
  # Rules are evaluated per row against `tags` and need only base R
  label <- if (spec@selected@selected_only) {
    scm_direction_label
  } else {
    scm_round_label
  }
  labels <- vapply(rounds, label, character(1))
  rules <- lapply(unique(labels), function(section) {
    members <- rounds[labels == section]
    rlang::new_formula(
      if (length(members) == 1) {
        rlang::call2("%in%", members, rlang::sym("tags"))
      } else {
        rlang::call2("any", rlang::call2("%in%", members, rlang::sym("tags")))
      },
      section,
      env = baseenv()
    )
  })
  rules <- c(
    rules,
    list(rlang::new_formula(TRUE, spec@other_section_label, env = baseenv()))
  )
  rlang::inject(set_spec_sections(spec, !!!rules))
}

#' Apply SCM settings to summary rows
#'
#' Sets the Description column, adds the Round Result column and the bold
#' flag, and filters the rows to the selected candidates and rounds.
#'
#' @param df Summary data frame with `tags` and `.name` columns
#' @param spec A SummarySpec object; `df` is returned unchanged unless it is
#'   a ScmSummarySpec
#' @param tree A hyperion_nonmem_tree object
#' @return Data frame of the rows to show
#' @noRd
scm_summary_rows <- function(df, spec, tree) {
  if (!is_scm_summary_spec(spec)) {
    return(df)
  }
  tags <- unclass(df$tags)
  has_tag <- function(wanted) {
    vapply(tags, function(t) any(wanted %in% t), logical(1))
  }
  has_round <- function(direction) {
    vapply(
      tags,
      function(t) any(grepl(paste0("^", direction, "_round"), t)),
      logical(1)
    )
  }
  is_stage <- has_tag(scm_stage_tags)
  is_selected <- has_tag("selected") & !is_stage
  added <- is_selected & has_round("forward")
  dropped <- is_selected & has_round("backward")

  if ("description" %in% names(df)) {
    df$description <- scm_descriptions(df, spec, tree, is_stage)
  }

  keep <- rep(TRUE, nrow(df))
  if (spec@selected@selected_only) {
    # Drop the working full model when no backward step dropped a
    # covariate: it then matches the reduced covariate model
    keep <- is_selected | is_stage
    if (!any(dropped)) {
      keep <- keep & !has_tag("working_full")
    }
    if (!any(keep)) {
      rlang::abort("The lineage has no selected SCM fits to show.")
    }
  }
  if (!identical(spec@rounds, "all")) {
    keep <- keep & scm_rows_in_rounds(df, spec@rounds)
  }

  # Add the Round Result column only when round candidates are shown
  if (spec@selected@column && any(keep & !is_stage)) {
    df$round_result <- NA_character_
    df$round_result[added] <- "added"
    df$round_result[dropped] <- "dropped"
  }
  if (spec@selected@bold) {
    df$.bold <- is_selected
  }
  df[keep, , drop = FALSE]
}

#' Find the summary rows in the requested rounds
#'
#' Stage fits (base, working full, reduced covariate) are in no round and
#' are always kept. Warns about rounds the lineage does not have.
#'
#' @param df Summary data frame with a `tags` column
#' @param rounds "none", or a character vector of round names and directions
#' @return Logical vector, TRUE for the rows to keep
#' @noRd
scm_rows_in_rounds <- function(df, rounds) {
  tags <- unclass(df$tags)
  row_round <- vapply(
    tags,
    function(t) {
      round <- grep("^(forward|backward)_round[0-9]+$", t, value = TRUE)
      if (length(round) == 0) NA_character_ else round[1]
    },
    character(1)
  )
  if (identical(rounds, "none")) {
    return(is.na(row_round))
  }
  row_direction <- sub("_round[0-9]+$", "", row_round)
  missing <- setdiff(rounds, c(row_round, row_direction))
  if (length(missing) > 0) {
    rlang::warn(paste0(
      "The lineage has no fits in: ",
      paste0("'", missing, "'", collapse = ", "),
      "."
    ))
  }
  is.na(row_round) | row_round %in% rounds | row_direction %in% rounds
}

#' Build the Description column of an SCM summary
#'
#' A fit's description is what it added or dropped; a stage fit has none,
#' since its section names it. With `show_all_covariates`, the covariates
#' in each fit's model are appended when the lineage carries them.
#'
#' @param df Summary data frame with `description` and `.name` columns
#' @param spec A ScmSummarySpec object
#' @param tree A hyperion_nonmem_tree object
#' @param is_stage Logical vector, TRUE for stage fits
#' @return Character vector of descriptions
#' @noRd
scm_descriptions <- function(df, spec, tree, is_stage) {
  step <- df$description
  step[is_stage] <- ""
  if (!spec@show_all_covariates) {
    return(step)
  }
  covariates <- scm_lineage_covariates(tree)[df$.name]
  known <- !vapply(covariates, is.null, logical(1))
  if (!any(known)) {
    rlang::warn(c(
      "The lineage does not say which covariates each model has, so the descriptions list none.",
      i = "`show_all_covariates = TRUE` needs the lineage from `hyperion::get_scm_lineage()`."
    ))
    return(step)
  }
  listed <- vapply(
    covariates,
    function(x) {
      if (length(x) == 0) "none" else paste(x, collapse = ", ")
    },
    character(1)
  )
  listed <- paste0("covariates: ", listed)
  out <- step
  with_step <- known & !is.na(step) & nzchar(step)
  out[known] <- listed[known]
  out[with_step] <- paste0(step[with_step], "; ", listed[with_step])
  out
}

#' Get the covariates of each model in a lineage
#'
#' @param tree A hyperion_nonmem_tree object
#' @return List of character vectors keyed by model path without extension;
#'   NULL for a node that carries no covariates
#' @noRd
scm_lineage_covariates <- function(tree) {
  covariates <- lapply(tree$nodes, function(node) {
    if (is.null(node$model$covariates)) {
      NULL
    } else {
      as.character(unlist(node$model$covariates))
    }
  })
  names(covariates) <- vapply(
    tree$nodes,
    function(node) tools::file_path_sans_ext(node$name),
    character(1)
  )
  covariates
}

#' Set the column labels specific to an SCM summary table
#'
#' Labels the Round Result column and uses a plain-text delta OFV label.
#' The default LaTeX label needs KaTeX or markdown rendering in the heading
#' cell and can show a scroll bar in Quarto.
#'
#' @param label_map Named list of column labels
#' @param data Summary data frame
#' @param spec A SummarySpec object; `label_map` is returned unchanged
#'   unless it is a ScmSummarySpec
#' @return Updated label map
#' @noRd
scm_summary_labels <- function(label_map, data, spec) {
  if (!is_scm_summary_spec(spec)) {
    return(label_map)
  }
  if ("round_result" %in% names(data)) {
    label_map[["round_result"]] <- "Round Result"
  }
  if ("dofv" %in% names(label_map)) {
    label_map[["dofv"]] <- "\u0394OFV"
  }
  label_map
}

# Tags of the fits that are not round candidates. The reference fit is
# tagged `base` or `full` by pharos and `reference` by
# hyperion::get_scm_lineage().
scm_stage_tags <- c(
  "base",
  "full",
  "reference",
  "working_full",
  "reduced_covariate"
)

#' Get the rounds of an SCM lineage
#'
#' @param tree A hyperion_nonmem_tree object
#' @return Character vector of round tags, in the order their fits appear
#' @noRd
scm_lineage_rounds <- function(tree) {
  marker_tags <- c("scm", "selected", "retry")
  rounds <- character()
  for (node in tree$nodes) {
    tags <- as.character(unlist(node$model$tags))
    if (!"scm" %in% tags) {
      next
    }
    rounds <- c(rounds, setdiff(tags, marker_tags))
  }
  unique(rounds)
}

#' Get the section label for a round
#'
#' @param round Round or stage tag (e.g., "forward_round2")
#' @return Section label (e.g., "Forward round 2")
#' @noRd
scm_round_label <- function(round) {
  if (round %in% names(scm_stage_labels)) {
    return(scm_stage_labels[[round]])
  }
  m <- scm_round_parts(round)
  if (length(m) == 3) {
    return(sprintf("%s round %s", tools::toTitleCase(m[2]), m[3]))
  }
  round
}

#' Get the section label for a round's direction
#'
#' @param round Round or stage tag (e.g., "forward_round2")
#' @return Section label (e.g., "Forward")
#' @noRd
scm_direction_label <- function(round) {
  if (round %in% names(scm_stage_labels)) {
    return(scm_stage_labels[[round]])
  }
  m <- scm_round_parts(round)
  if (length(m) == 3) {
    return(tools::toTitleCase(m[2]))
  }
  round
}

scm_stage_labels <- c(
  base = "Base model",
  full = "Full model",
  reference = "Reference model",
  working_full = "Working full model",
  reduced_covariate = "Reduced covariate model"
)

#' Split a round tag into its direction and number
#'
#' @param round Round tag (e.g., "forward_round2")
#' @return c(round, direction, number), or character() for a stage tag
#' @noRd
scm_round_parts <- function(round) {
  regmatches(round, regexec("^(forward|backward)_round([0-9]+)$", round))[[1]]
}
