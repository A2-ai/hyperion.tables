# =============================================================================
# User-facing DSL functions
# ==============================================================================

#' Create section assignment rules
#'
#' Creates rules for assigning parameters to named sections in the output table.
#' Rules are evaluated after name transformation, so you can match on the final
#' display name or use the preserved `nonmem_name` and `user_name` columns.
#'
#' @param ... Formula expressions following [dplyr::case_when()] syntax, `kind == "THETA" ~ "Structural Parameters"`
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
