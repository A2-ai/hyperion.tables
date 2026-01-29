# ==============================================================================
# Parameter Name Options
# ==============================================================================

#' Parameter name display options
#'
#' Controls how parameter names are displayed in tables.
#'
#' @param source Which name field to use: "name" (default), "display", or "nonmem"
#' @param append_omega_with_theta Logical. If TRUE (default), append associated theta
#'   names to omega parameters (e.g., "OM1 TVCL" or "OMEGA(1,1)-THETA1")
#'
#' @section Properties:
#' The following properties are available on a `ParameterNameOptions` object:
#' \itemize{
#'   \item `source` - Which name field to use ("name", "display", or "nonmem").
#'   \item `append_omega_with_theta` - Whether to append theta info to omega names.
#' }
#'
#' @export
ParameterNameOptions <- S7::new_class(
  "ParameterNameOptions",
  properties = list(
    source = S7::new_property(
      class = S7::class_character,
      default = "name"
    ),
    append_omega_with_theta = S7::new_property(
      class = S7::class_logical,
      default = TRUE
    )
  ),
  validator = function(self) {
    valid_sources <- c("name", "display", "nonmem")
    if (!self@source %in% valid_sources) {
      return(sprintf(
        "@source must be 'name', 'display', or 'nonmem'. Got: '%s'",
        self@source
      ))
    }
    if (
      length(self@append_omega_with_theta) != 1 ||
        is.na(self@append_omega_with_theta)
    ) {
      return(sprintf(
        "@append_omega_with_theta must be TRUE or FALSE. Got: %s",
        self@append_omega_with_theta
      ))
    }
  }
)
