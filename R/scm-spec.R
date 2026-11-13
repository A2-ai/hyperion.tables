# ==============================================================================
# SCM summary tables
#
# Everything a stepwise covariate modeling table adds to a run summary lives
# here: the spec is a SummarySpec with SCM settings of its own, and
# apply_summary_spec() and make_summary_table() ask the functions below what
# an SCM spec changes. Any other SummarySpec passes through them untouched.
# ==============================================================================

#' How an SCM summary table shows the candidate each round selected
#'
#' `ScmSelectedOptions()` is what [scm_summary_spec()] takes as `selected`.
#' It is usually set with [set_spec_scm_selected()].
#' Each round of an SCM process selects at most one candidate, the fit
#' tagged `"selected"`: the covariate a forward round added, or the one a
#' backward round dropped. The table marks it with a Round Result column
#' (the default), with a bold row, or with both, and it can show the
#' selected candidates alone. The settings combine freely.
#'
#' With `selected_only = TRUE` the table keeps the models that mattered:
#' the base (or full) model, the working full and reduced covariate models,
#' and in each round the candidate it selected, so a round that selected
#' none shows no row. Since each round then has at most one row, the rounds are
#' grouped by direction instead of sectioned one by one: the reference
#' model, then the forward steps under "Forward", the working full model,
#' the backward steps under "Backward", and the reduced covariate model.
#' When no backward round dropped anything, the working full model says
#' nothing the reduced covariate model does not, so it is left out too.
#'
#' @param column Logical. If TRUE (the default), the table has a Round
#'   Result column, which reads "added" or "dropped" on the selected
#'   candidate's row and is empty elsewhere. A table that shows no round's
#'   candidates has no result to report, so it has no such column.
#' @param bold Logical. If TRUE, the selected candidate's row is rendered in
#'   bold. Default `FALSE`.
#' @param selected_only Logical. If TRUE, the table shows only the
#'   selected candidates, and no others, with the rounds grouped by
#'   direction. Default `FALSE` shows every candidate.
#'
#' @section Properties:
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
#' `ScmSummarySpec` is the [SummarySpec] of a stepwise covariate modeling
#' table. It is a child class of `SummarySpec`, as `SummarySpec` is of
#' `BaseSpec`: everything that takes a `SummarySpec` takes it, including
#' [apply_summary_spec()] and the `set_spec_*()` modifiers, and it adds the
#' settings only an SCM table has. Only a `ScmSummarySpec` gets the SCM
#' behavior (a section per round, Round Result column, bold rows, only
#' the selected candidates, p-values for backward steps), and the SCM
#' functions,
#' [set_spec_scm_selected()] and [get_spec_scm_selected()], accept nothing
#' else, so an ordinary run summary cannot pick it up by accident. Applied
#' to a lineage with no SCM fits, it is an error.
#'
#' [scm_summary_spec()] is the usual way to make one and describes what the
#' table shows.
#'
#' @param show_all_covariates Logical. If TRUE, the Description column
#'   also lists the covariates each fit's model has, after what the fit
#'   added or dropped. Default `FALSE`.
#' @param selected A [ScmSelectedOptions()] object: how the table shows the
#'   candidate each round selected. Prefer [set_spec_scm_selected()] in a
#'   pipe for user-facing code.
#' @param rounds The rounds the table shows, as a character vector of round
#'   names (`"forward_round1"`) and directions (`"forward"`, `"backward"`);
#'   `NULL` (the default) for every round; or `NA` for none, which leaves
#'   only the milestones. Prefer [set_spec_scm_rounds()] in a pipe for
#'   user-facing code.
#' @param other_section_label Section label for a model matching no round.
#' @param ... Arguments of [SummarySpec()]. Those not given take the SCM
#'   table's defaults: no title, the fits tagged `"scm"`, the columns
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
      class = S7::class_character | NULL,
      default = NULL
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
    rounds = NULL,
    other_section_label = "Other",
    ...
  ) {
    # `rounds = NA` is held as a character NA, as the rounds are.
    if (scm_milestones_only(rounds)) {
      rounds <- NA_character_
    }
    # What an SCM table has in place of a run summary's defaults
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

# TRUE when `rounds` is NA: the table shows no round, only the milestones.
scm_milestones_only <- function(rounds) {
  length(rounds) == 1 && is.na(rounds)
}

# NULL when `rounds` is every round (NULL), none (NA), or names rounds and
# directions; otherwise what is wrong with it.
validate_scm_rounds <- function(rounds) {
  if (is.null(rounds) || scm_milestones_only(rounds)) {
    return(NULL)
  }
  is_round <- grepl("^(forward|backward)(_round[0-9]+)?$", rounds)
  if (length(rounds) == 0 || anyNA(rounds) || !all(is_round)) {
    bad <- rounds[is.na(rounds) | !is_round]
    return(paste0(
      "@rounds must name rounds (\"forward_round1\") or directions ",
      "(\"forward\", \"backward\"), or be NULL for every round or NA for none.",
      if (length(bad) > 0) paste0(" Got: ", paste0("'", bad, "'", collapse = ", "))
    ))
  }
  NULL
}

is_scm_summary_spec <- function(spec) {
  S7::S7_inherits(spec, ScmSummarySpec)
}

#' Pre-configured ScmSummarySpec for a stepwise covariate modeling process
#'
#' `scm_summary_spec()` returns a [ScmSummarySpec] for the run summary table of
#' an SCM process: the fits tagged `"scm"`, in one section per round, from
#' the round tag pharos writes to each fit's metadata (`"base"` or `"full"`,
#' `"forward_round1"`, ..., `"working_full"`, `"reduced_covariate"`), in the
#' order the process ran them. It does not need the lineage: the sections
#' are worked out from the rounds of the lineage [apply_summary_spec()] is
#' given, as the rules you would write by hand with [set_spec_sections()],
#' `"base" %in% tags ~ "Base model"`, `"forward_round1" %in% tags ~ "Forward
#' round 1"`, ..., for however many rounds the process ran.
#'
#' Every fit is based on the model its round tested against, so dOFV is the
#' round's own; the working full and reduced covariate models are based on
#' the reference fit, so theirs is against it. The columns are Reference,
#' Description, OFV, dOFV and p-value. The backward steps get a p-value
#' too, with the round's reference as the fuller model. The table has no
#' footnotes. A [ScmSummarySpec] is a [SummarySpec], so the `set_spec_*()`
#' modifiers customize it further.
#'
#' The Description column shows each fit's description, which for an SCM
#' fit is what it added or dropped: "add CRCL_CL". The milestones (the base
#' or full model, the working full model and the reduced covariate model)
#' add or drop nothing and their section already names them, so theirs is
#' empty. `show_all_covariates = TRUE` also lists the covariates each fit's
#' model has: "add CRCL_CL; covariates: WT_CL, CRCL_CL", and "covariates:
#' WT_CL, CRCL_CL" for a milestone.
#'
#' The covariates a model has are not in its description or its metadata.
#' They come from the SCM process record, which
#' `hyperion::get_scm_lineage()` puts on each fit of the lineage it builds.
#' A lineage from `hyperion::get_model_lineage()` has none, so for it
#' `show_all_covariates = TRUE` adds nothing, with a warning.
#'
#' `selected`, a [ScmSelectedOptions()], picks how the table shows the
#' candidate each round selected, the fit tagged `"selected"`. By default a
#' Round Result column reads "added" or "dropped" on its row; `column =
#' FALSE` leaves the column out. `bold = TRUE` renders its row in bold, and
#' `selected_only = TRUE` shows it alone, with the other candidates left
#' out and the rounds grouped by direction. [set_spec_scm_selected()] sets
#' the same on a spec already made.
#'
#' `rounds` picks the rounds the table shows, by name (`"forward_round1"`)
#' or by direction (`"forward"`, `"backward"`); the default shows them all.
#' The milestones are not rounds, so they always show, and every row keeps
#' the dOFV and p-value it has in the full table. `rounds = NA` shows no
#' round at all, only the milestones. [set_spec_scm_rounds()] sets the same
#' on a spec already made.
#'
#' Section rules set on the spec with [set_spec_sections()] replace the ones
#' it would work out.
#'
#' @param title Table title. `NULL` (the default) gives a table with no
#'   title; pass a string to set one.
#' @param show_all_covariates Logical. If TRUE, the Description column
#'   also lists the covariates each fit's model has, after what the fit
#'   added or dropped. Default `FALSE`.
#' @param selected A [ScmSelectedOptions()] object: how the table shows the
#'   candidate each round selected: marked with a Round Result column or a
#'   bold row, or alone. The default has the Round Result column.
#' @param rounds The rounds the table shows, as a character vector of round
#'   names (`"forward_round1"`) and directions (`"forward"`, `"backward"`);
#'   `NULL` (the default) for every round; or `NA` for none, which leaves
#'   only the milestones.
#' @param other_section_label Section label for a model matching no round.
#'
#' @return A [ScmSummarySpec] object.
#' @seealso [ScmSummarySpec], [set_spec_scm_selected()], [set_spec_scm_rounds()],
#'   [ScmSelectedOptions()], [apply_summary_spec()], [make_summary_table()].
#' @export
#' @examples \dontrun{
#' # From the process record: descriptions and each model's covariates
#' scm <- hyperion::read_scm("model/nonmem/scm/run001scm.toml")
#' scm |>
#'   hyperion::get_scm_lineage() |>
#'   apply_summary_spec(scm_summary_spec(title = "Covariate search")) |>
#'   make_summary_table()
#'
#' # From the fits' metadata, as any other lineage
#' hyperion::get_model_lineage() |>
#'   apply_summary_spec(scm_summary_spec()) |>
#'   make_summary_table()
#'
#' # Also list the covariates each fit's model has
#' scm_summary_spec(show_all_covariates = TRUE)
#'
#' # The candidate each round selected: also in bold, or without the Round
#' # Result column
#' scm_summary_spec(selected = ScmSelectedOptions(bold = TRUE))
#' scm_summary_spec(selected = ScmSelectedOptions(column = FALSE))
#'
#' # Show only some rounds, or only the milestones
#' scm_summary_spec(rounds = c("forward_round1", "forward_round2"))
#' scm_summary_spec(rounds = "backward")
#' scm_summary_spec(rounds = NA)
#'
#' # Show only the selected candidates: the models that mattered
#' scm_summary_spec(selected = ScmSelectedOptions(selected_only = TRUE))
#'
#' # The same on a spec already made
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
  rounds = NULL,
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
  if (
    !is.null(rounds) && !is.character(rounds) && !scm_milestones_only(rounds)
  ) {
    rlang::abort(
      "`rounds` must be a character vector of round names, NA, or NULL."
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

#' Set how an SCM summary table shows the selected candidates
#'
#' `set_spec_scm_selected()` sets how the table of an SCM spec shows the
#' candidate each round selected: marked with a Round Result column (on
#' unless turned off), with its row in bold, or alone, without the other
#' candidates. It builds the spec's [ScmSelectedOptions()]; a setting not
#' given keeps its value.
#' Operates on [ScmSummarySpec] only.
#'
#' @param spec A ScmSummarySpec object, from [scm_summary_spec()].
#' @param ... Not used.
#' @param column Logical. If TRUE, the table has a Round Result column,
#'   which reads "added" or "dropped" on the selected candidate's row.
#' @param bold Logical. If TRUE, the selected candidate's row is rendered in
#'   bold.
#' @param selected_only Logical. If TRUE, the table shows only the
#'   selected candidates, and no others, with the rounds grouped by
#'   direction.
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
#' @param spec A ScmSummarySpec object, from [scm_summary_spec()].
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
#' `set_spec_scm_rounds()` picks the rounds the table of an SCM spec shows,
#' by name or by direction. The milestones (the base or full model, the
#' working full model and the reduced covariate model) are not rounds, so
#' they always show; `NA` shows them alone. A round is left out after dOFV
#' and the p-value are computed, so every row keeps the values it has in
#' the full table. A round the lineage does not have is a warning. Operates
#' on [ScmSummarySpec] only.
#'
#' @param spec A ScmSummarySpec object, from [scm_summary_spec()].
#' @param ... Round names as pharos tags the fits, such as
#'   `"forward_round1"`, and directions, `"forward"` or `"backward"`, for
#'   every round of one; as strings or a character vector. With none, or
#'   `NULL`, the table shows every round. With `NA`, it shows no round, only
#'   the milestones.
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
#'   set_spec_scm_rounds(NA)
#' # Every round again
#' spec <- set_spec_scm_rounds(spec)
set_spec_scm_rounds <- function(spec, ...) {
  if (!is_scm_summary_spec(spec)) {
    rlang::abort("`spec` must be a <ScmSummarySpec> object.")
  }
  rounds <- unlist(list(...), use.names = FALSE)
  if (scm_milestones_only(rounds)) {
    spec@rounds <- NA_character_
    return(spec)
  }
  if (!is.null(rounds) && !is.character(rounds)) {
    rlang::abort("Rounds must be given as strings, such as \"forward_round1\" or \"forward\".")
  }
  spec@rounds <- if (length(rounds) == 0) NULL else unique(rounds)
  spec
}

#' Get the rounds an SCM summary table shows
#'
#' `get_spec_scm_rounds()` returns the rounds the table shows: NULL when
#' it shows every round, NA when it shows only the milestones. Operates on
#' [ScmSummarySpec] only.
#'
#' @param spec A ScmSummarySpec object, from [scm_summary_spec()].
#' @param ... Not used.
#' @return Character vector of round names and directions, NULL, or NA.
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

# ------------------------------------------------------------------------------
# What an SCM spec changes in apply_summary_spec() and make_summary_table()
# ------------------------------------------------------------------------------

# The spec with a section rule per round of this lineage, in the order the
# process ran them, unless the user set rules of their own. With only the
# selected candidates shown, the rounds of a direction share a section,
# which comes where its first round ran.
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
  # The rules evaluate against each row with `tags` unwrapped; they need
  # base R for `%in%`, nothing else.
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

# The summary rows as an SCM spec shows them: the Description column, the
# Round Result column, the bold flag, and only the selected rows. `df` still
# has each model's `tags` and its key, `.name`.
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
    # Without a backward step that dropped anything, the working full model
    # is the reduced covariate model less its cov step, so only the latter
    # stays.
    keep <- is_selected | is_stage
    if (!any(dropped)) {
      keep <- keep & !has_tag("working_full")
    }
    if (!any(keep)) {
      rlang::abort("The lineage has no selected SCM fits to show.")
    }
  }
  if (!is.null(spec@rounds)) {
    keep <- keep & scm_rows_in_rounds(df, spec@rounds)
  }

  # The Round Result is a round's: a table left with the milestones alone
  # has no round to report one for.
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

# TRUE for the rows the spec's rounds keep: a stage fit, which is in no
# round, and a candidate of a round that is named or whose direction is.
# With `rounds = NA`, only the stage fits. `df` already has every row's
# dOFV, so leaving a round out changes no other row. A round the lineage
# does not have is a warning.
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
  if (scm_milestones_only(rounds)) {
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

# What the Description column shows. A fit's description is its step, what
# it added or dropped; a stage fit (base, working full, reduced covariate)
# has no step, since its section names it. `show_all_covariates` adds the
# covariates the fit's model has, which the lineage's nodes carry when
# hyperion::get_scm_lineage() built it.
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

# The covariates each model of the lineage has, keyed as the summary keys
# its models: the path without the extension. NULL for a node without them.
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

# The column labels an SCM table adds or changes: the Round Result column's,
# and a plain Delta-OFV in place of the LaTeX one, which needs KaTeX or
# markdown rendering in the heading cell and can show a scroll bar in Quarto.
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

# The round tags of the fits that are not round candidates: the reference
# fit (`base` or `full` from pharos, `reference` from hyperion's record
# lineage), the working full model (tagged `working_full`) and the reduced
# covariate model (tagged `reduced_covariate`).
scm_stage_tags <- c("base", "full", "reference", "working_full", "reduced_covariate")

# The rounds in the lineage, in the order their fits appear: each SCM fit's
# tag other than "scm", "selected" and "retry".
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

# "forward_round2" -> "Forward round 2"; the stages get their own names.
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

# "forward_round2" -> "Forward": the rounds of a direction share a section.
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

# c(round, direction, number) for a round tag, or character() for a stage.
scm_round_parts <- function(round) {
  regmatches(round, regexec("^(forward|backward)_round([0-9]+)$", round))[[1]]
}
