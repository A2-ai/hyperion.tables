#' @include spec-classes.R
NULL

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
      class = BaseSpec | NULL,
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
