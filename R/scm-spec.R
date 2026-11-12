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
#' backward round dropped. The table can mark it with a column, with a bold
#' row, or with both; it can show the selected candidates alone, the path
#' the process took; or it can show every candidate alike (the default).
#' The settings combine freely.
#'
#' With `path = TRUE` the table keeps the models that mattered: the base
#' (or full) model, the working full and reduced covariate models, and in
#' each round the candidate it selected, so a round that selected none
#' shows no row. Since each round then has at most one row, the rounds are
#' grouped by direction instead of sectioned one by one: the reference
#' model, then the forward steps under "Forward", the working full model,
#' the backward steps under "Backward", and the reduced covariate model.
#' When no backward round dropped anything, the working full model says
#' nothing the reduced covariate model does not, so it is left out too.
#'
#' @param column Logical. If TRUE, the table has a Round Result column,
#'   which reads "added" or "dropped" on the selected candidate's row and is
#'   empty elsewhere. Default `FALSE`.
#' @param bold Logical. If TRUE, the selected candidate's row is rendered in
#'   bold. Default `FALSE`.
#' @param path Logical. If TRUE, the table shows only the path the process
#'   took: the selected candidates and no others, with the rounds grouped
#'   by direction. Default `FALSE` shows every candidate.
#'
#' @section Properties:
#' \itemize{
#'   \item `column` - Whether the table has a Round Result column.
#'   \item `bold` - Whether the selected candidate's row is bold.
#'   \item `path` - Whether the table shows only the selected candidates.
#' }
#'
#' @seealso [set_spec_scm_selected()], [scm_summary_spec()].
#' @export
#' @examples
#' ScmSelectedOptions(column = TRUE)
#' ScmSelectedOptions(column = TRUE, bold = TRUE)
#' ScmSelectedOptions(path = TRUE)
ScmSelectedOptions <- S7::new_class(
  "ScmSelectedOptions",
  package = "hyperion.tables",
  properties = list(
    column = S7::new_property(class = S7::class_logical, default = FALSE),
    bold = S7::new_property(class = S7::class_logical, default = FALSE),
    path = S7::new_property(class = S7::class_logical, default = FALSE)
  ),
  validator = function(self) {
    for (flag in c("column", "bold", "path")) {
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
#' behavior (a section per round, Round Result column, bold rows, the path,
#' p-values for backward steps), and the SCM functions,
#' [set_spec_scm_selected()] and [get_spec_scm_selected()], accept nothing
#' else, so an ordinary run summary cannot pick it up by accident. Applied
#' to a lineage with no SCM fits, it is an error.
#'
#' [scm_summary_spec()] is the usual way to make one and describes what the
#' table shows.
#'
#' @param description What the Description column says about each fit:
#'   `"sparse"` (the default), only what it added or dropped; or
#'   `"verbose"`, what it added or dropped and the covariates its model has.
#' @param selected A [ScmSelectedOptions()] object: how the table shows the
#'   candidate each round selected. Prefer [set_spec_scm_selected()] in a
#'   pipe for user-facing code.
#' @param other_section_label Section label for a model matching no round.
#' @param ... Arguments of [SummarySpec()]. Those not given take the SCM
#'   table's defaults: no title, the fits tagged `"scm"`, the columns
#'   Reference, Description, OFV, dOFV and p-value, and no footnotes.
#'
#' @seealso [scm_summary_spec()], [SummarySpec].
#' @export
#' @examples
#' spec <- ScmSummarySpec(description = "verbose")
#' S7::S7_inherits(spec, SummarySpec)
ScmSummarySpec <- S7::new_class(
  "ScmSummarySpec",
  parent = SummarySpec,
  properties = list(
    description = S7::new_property(
      class = S7::class_character,
      default = "sparse"
    ),
    selected = S7::new_property(
      class = ScmSelectedOptions,
      default = ScmSelectedOptions()
    ),
    other_section_label = S7::new_property(
      class = S7::class_character,
      default = "Other"
    )
  ),
  validator = function(self) {
    if (
      length(self@description) != 1 ||
        !self@description %in% c("sparse", "verbose")
    ) {
      return("@description must be \"sparse\" or \"verbose\".")
    }
    if (
      length(self@other_section_label) != 1 ||
        is.na(self@other_section_label)
    ) {
      return("@other_section_label must be a single character string.")
    }
  },
  constructor = function(
    description = "sparse",
    selected = ScmSelectedOptions(),
    other_section_label = "Other",
    ...
  ) {
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
      description = description,
      selected = selected,
      other_section_label = other_section_label
    )
  }
)

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
#' `description` picks how much the Description column says about each
#' fit:
#' \itemize{
#'   \item `"sparse"` (the default) shows the fit's description, which for
#'     an SCM fit is what it added or dropped: "add CRCL_CL".
#'   \item `"verbose"` adds the covariates its model has: "add CRCL_CL;
#'     covariates: WT_CL, CRCL_CL".
#' }
#' The base (or full), working full and reduced covariate models add or
#' drop nothing and their section already names them, so their description
#' never repeats the name: it is empty when sparse and "covariates: WT_CL,
#' CRCL_CL" when verbose.
#'
#' The covariates a model has are not in its description or its metadata.
#' They come from the SCM process record, which
#' `hyperion::get_scm_lineage()` puts on each fit of the lineage it builds.
#' A lineage from `hyperion::get_model_lineage()` has none, so `"verbose"`
#' shows what `"sparse"` does for it, with a warning.
#'
#' `selected`, a [ScmSelectedOptions()], picks how the table shows the
#' candidate each round selected, the fit tagged `"selected"`: marked with
#' a Round Result column reading "added" or "dropped" (`column = TRUE`),
#' with its row in bold (`bold = TRUE`), or alone, as the path the process
#' took, with the other candidates left out and the rounds grouped by
#' direction (`path = TRUE`). The default shows every candidate alike.
#' [set_spec_scm_selected()] sets the same on a spec already made.
#'
#' Section rules set on the spec with [set_spec_sections()] replace the ones
#' it would work out.
#'
#' @param title Table title. `NULL` (the default) gives a table with no
#'   title; pass a string to set one.
#' @param description How much the Description column says about each fit:
#'   `"sparse"` (the default), only what it added or dropped; or
#'   `"verbose"`, what it added or dropped and the covariates its model has.
#' @param selected A [ScmSelectedOptions()] object: how the table shows the
#'   candidate each round selected: marked with a Round Result column or a
#'   bold row, or alone, as the path the process took. The default shows
#'   every candidate alike.
#' @param other_section_label Section label for a model matching no round.
#'
#' @return A [ScmSummarySpec] object.
#' @seealso [ScmSummarySpec], [set_spec_scm_selected()],
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
#' # Describe each fit by what it added or dropped and its model's
#' # covariates
#' scm_summary_spec(description = "verbose")
#'
#' # Mark the candidate each round selected: a Round Result column, its row
#' # in bold, or both
#' scm_summary_spec() |> set_spec_scm_selected(column = TRUE)
#' scm_summary_spec() |> set_spec_scm_selected(bold = TRUE)
#' scm_summary_spec() |> set_spec_scm_selected(column = TRUE, bold = TRUE)
#'
#' # Show the path the process took: only the models that mattered
#' scm_summary_spec() |> set_spec_scm_selected(path = TRUE)
#'
#' # The same in one call
#' scm_summary_spec(selected = ScmSelectedOptions(path = TRUE))
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
  description = c("sparse", "verbose"),
  selected = ScmSelectedOptions(),
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
  description <- match.arg(description)
  if (!S7::S7_inherits(selected, ScmSelectedOptions)) {
    rlang::abort("`selected` must be a <ScmSelectedOptions> object.")
  }
  if (!rlang::is_string(other_section_label)) {
    rlang::abort("`other_section_label` must be a single character string.")
  }

  ScmSummarySpec(
    description = description,
    selected = selected,
    other_section_label = other_section_label,
    title = title %||% ""
  )
}

#' Set how an SCM summary table shows the selected candidates
#'
#' `set_spec_scm_selected()` sets how the table of an SCM spec shows the
#' candidate each round selected: marked with a Round Result column, with
#' its row in bold, or alone, as the path the process took. It builds the
#' spec's [ScmSelectedOptions()]; a setting not given keeps its value.
#' Operates on [ScmSummarySpec] only.
#'
#' @param spec A ScmSummarySpec object, from [scm_summary_spec()].
#' @param ... Not used.
#' @param column Logical. If TRUE, the table has a Round Result column,
#'   which reads "added" or "dropped" on the selected candidate's row.
#' @param bold Logical. If TRUE, the selected candidate's row is rendered in
#'   bold.
#' @param path Logical. If TRUE, the table shows only the path the process
#'   took: the selected candidates and no others, with the rounds grouped
#'   by direction.
#' @return Modified spec.
#' @seealso [get_spec_scm_selected()], [ScmSelectedOptions()],
#'   [scm_summary_spec()].
#' @export
#' @examples
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_selected(column = TRUE, bold = TRUE)
#' # Only the path the process took
#' spec <- scm_summary_spec() |>
#'   set_spec_scm_selected(path = TRUE)
set_spec_scm_selected <- function(
  spec,
  ...,
  column = NULL,
  bold = NULL,
  path = NULL
) {
  if (!is_scm_summary_spec(spec)) {
    rlang::abort("`spec` must be a <ScmSummarySpec> object.")
  }
  rlang::check_dots_empty()
  selected_args <- list(
    column = spec@selected@column,
    bold = spec@selected@bold,
    path = spec@selected@path
  )
  if (!is.null(column)) {
    selected_args$column <- column
  }
  if (!is.null(bold)) {
    selected_args$bold <- bold
  }
  if (!is.null(path)) {
    selected_args$path <- path
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
#'   set_spec_scm_selected(column = TRUE)
#' get_spec_scm_selected(spec)
get_spec_scm_selected <- function(spec, ...) {
  if (!is_scm_summary_spec(spec)) {
    rlang::abort("`spec` must be a <ScmSummarySpec> object.")
  }
  spec@selected
}

# ------------------------------------------------------------------------------
# What an SCM spec changes in apply_summary_spec() and make_summary_table()
# ------------------------------------------------------------------------------

# The spec with a section rule per round of this lineage, in the order the
# process ran them, unless the user set rules of their own. With only the
# path shown, the rounds of a direction share a section, which comes where
# its first round ran.
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
  label <- if (spec@selected@path) scm_direction_label else scm_round_label
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
# Round Result column, the bold flag, and only the path's rows. `df` still
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
  if (spec@selected@column) {
    df$round_result <- NA_character_
    df$round_result[added] <- "added"
    df$round_result[dropped] <- "dropped"
  }
  if (spec@selected@bold) {
    df$.bold <- is_selected
  }
  if (spec@selected@path) {
    # Without a backward step that dropped anything, the working full model
    # is the reduced covariate model less its cov step, so only the latter
    # stays.
    keep <- is_selected | is_stage
    if (!any(dropped)) {
      keep <- keep & !has_tag("working_full")
    }
    df <- df[keep, , drop = FALSE]
    if (nrow(df) == 0) {
      rlang::abort("The lineage has no SCM fits on the path the process took.")
    }
  }
  df
}

# What the Description column shows. A fit's description is its step, what
# it added or dropped; a stage fit (base, working full, reduced covariate)
# has no step, since its section names it. Verbose adds the covariates the
# fit's model has, which the lineage's nodes carry when
# hyperion::get_scm_lineage() built it.
scm_descriptions <- function(df, spec, tree, is_stage) {
  step <- df$description
  step[is_stage] <- ""
  if (spec@description != "verbose") {
    return(step)
  }
  covariates <- scm_lineage_covariates(tree)[df$.name]
  known <- !vapply(covariates, is.null, logical(1))
  if (!any(known)) {
    rlang::warn(c(
      "The lineage does not say which covariates each model has, so the descriptions are sparse.",
      i = "`description = \"verbose\"` needs the lineage from `hyperion::get_scm_lineage()`."
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
