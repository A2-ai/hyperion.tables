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

#' @noRd
sections_property <- function() {
  S7::new_property(
    class = S7::class_list,
    default = list(),
    setter = function(self, value) {
      if (
        length(value) > 0 &&
          all(vapply(value, rlang::is_formula, logical(1)))
      ) {
        # validation only — no dedup; overwrite=TRUE in set_spec_sections
        # handles intentional replacement
      }
      self@sections <- value
      self
    }
  )
}
