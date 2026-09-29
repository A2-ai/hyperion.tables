#' Pre-configured TableSpec for a standard parameter table
#'
#' `parameter_table_spec()` returns a [TableSpec] configured for the common
#' parameter table layout: omegas are shown as CV, parameter names use their
#' display names, and parameters are grouped into sections by kind
#' (THETA/OMEGA/SIGMA). The returned spec can be customized further with the
#' `set_spec_*()` modifiers.
#'
#' Section rules are matched in order and the preset ends with a catch-all
#' rule, so rules appended with [set_spec_sections()] never apply. To change
#' the sections, pass `overwrite = TRUE` to replace the preset's rules, or
#' assign specific parameters with `parameters =`.
#'
#' @param theta_section_label Section label for THETA parameters.
#' @param omega_section_label Section label for OMEGA parameters.
#' @param sigma_section_label Section label for SIGMA parameters.
#' @param other_section_label Section label for parameters matching no other rule.
#' @param title Table title. `NULL` (the default) gives a table with no
#'   title; pass a string to set one.
#'
#' @return A [TableSpec] object.
#' @seealso [TableSpec], [make_parameter_table()].
#' @export
#' @examples
#' spec <- parameter_table_spec(title = "Parameter Estimates")
#'
#' # Customize further with modifiers
#' spec <- parameter_table_spec() |>
#'   set_spec_sigfig(4)
parameter_table_spec <- function(
  theta_section_label = "Structural model parameters",
  omega_section_label = "Interindividual variability",
  sigma_section_label = "Residual error",
  other_section_label = "Other",
  title = NULL
) {
  labels <- list(
    theta_section_label = theta_section_label,
    omega_section_label = omega_section_label,
    sigma_section_label = sigma_section_label,
    other_section_label = other_section_label
  )
  for (nm in names(labels)) {
    if (!rlang::is_string(labels[[nm]])) {
      rlang::abort(sprintf("`%s` must be a single character string.", nm))
    }
  }
  if (!is.null(title) && !rlang::is_string(title)) {
    rlang::abort("`title` must be a single character string or NULL.")
  }

  spec <- TableSpec() |>
    set_spec_transforms(omega = "cv") |>
    set_spec_sections(
      kind == "THETA" ~ !!theta_section_label,
      kind == "OMEGA" ~ !!omega_section_label,
      kind == "SIGMA" ~ !!sigma_section_label,
      TRUE ~ !!other_section_label
    ) |>
    set_spec_parameter_names(source = "display")

  set_spec_title(spec, title %||% "")
}
