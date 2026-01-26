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
