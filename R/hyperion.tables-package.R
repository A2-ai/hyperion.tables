#' hyperion.tables: Publication-Ready Tables for Pharmacometric Models
#'
#' @description
#' hyperion.tables provides tools for creating publication-ready parameter
#' tables, model comparison tables, and run summary tables from NONMEM models
#' managed by hyperion/pharos.
#'
#' @section Spec Classes:
#' S7 classes for configuring table output:
#' \itemize{
#'   \item [TableSpec] - Configuration for parameter tables
#'   \item [SummarySpec] - Configuration for run summary tables
#'   \item [CIOptions] - Confidence interval display options
#'   \item [ParameterNameOptions] - Parameter name display options
#' }
#'
#' @section Table Creation:
#' High-level functions for creating tables:
#' \itemize{
#'   \item [make_parameter_table()] - Build a formatted parameter table
#'   \item [make_summary_table()] - Build a run summary table
#'   \item [make_comparison_table()] - Build a model comparison table
#' }
#'
#' @section Spec Application:
#' Functions for applying specs to data:
#' \itemize{
#'   \item [apply_table_spec()] - Enrich parameter data with transforms, CIs, sections
#'   \item [apply_summary_spec()] - Filter models and prepare summary data
#'   \item [apply_formatting()] - Apply CI merging, numeric formatting, missing text
#' }
#'
#' @section Spec Extraction:
#' Functions for retrieving specs attached to data frames:
#' \itemize{
#'   \item [get_table_spec()] - Extract TableSpec from parameter data
#'   \item [get_summary_spec()] - Extract SummarySpec from summary data
#' }
#'
#' @section Spec Getters:
#' Functions for reading spec properties:
#' \itemize{
#'   \item [get_spec_columns()] - Get columns from a spec
#'   \item [get_spec_title()] - Get title from a spec
#'   \item [get_spec_sigfig()] - Get significant figures setting
#'   \item [get_spec_ci()] - Get CI options from a TableSpec
#'   \item [get_spec_parameter_names()] - Get parameter name options
#'   \item [get_spec_sections()] - Get section rules
#'   \item [get_spec_filter()] - Get row filter rules
#'   \item [get_spec_transforms()] - Get display transforms
#'   \item [get_spec_variability()] - Get variability rules
#'   \item [get_spec_time_format()] - Get time format from a SummarySpec
#' }
#'
#' @section Spec Modifiers - Columns:
#' Functions for modifying spec column settings:
#' \itemize{
#'   \item [add_spec_columns()] - Append columns to output
#'   \item [drop_spec_columns()] - Exclude columns from output
#'   \item [set_spec_columns()] - Replace column list entirely
#' }
#'
#' @section Spec Modifiers - Common:
#' Functions that work on both TableSpec and SummarySpec:
#' \itemize{
#'   \item [set_spec_title()] - Set table title
#'   \item [set_spec_sigfig()] - Set significant figures
#'   \item [set_spec_ofv_decimals()] - Set OFV decimal places
#'   \item [set_spec_hide_empty()] - Control empty column hiding
#'   \item [set_spec_pvalue()] - Set p-value formatting
#'   \item [set_spec_footnotes()] - Set footnote order
#' }
#'
#' @section Spec Modifiers - TableSpec:
#' Functions specific to TableSpec:
#' \itemize{
#'   \item [set_spec_parameter_names()] - Set parameter name display options
#'   \item [set_spec_ci()] - Set confidence interval options
#'   \item [set_spec_missing()] - Set missing value handling
#'   \item [set_spec_transforms()] - Set display transforms by parameter kind
#'   \item [set_spec_sections()] - Set section grouping rules
#'   \item [set_spec_filter()] - Set row filter rules
#'   \item [set_spec_variability()] - Set variability display rules
#' }
#'
#' @section Spec Modifiers - SummarySpec:
#' Functions specific to SummarySpec:
#' \itemize{
#'   \item [set_spec_time_format()] - Set time column format
#'   \item [set_spec_models()] - Filter by model names
#'   \item [set_spec_tag_filter()] - Filter by model tags
#'   \item [set_spec_remove_unrun()] - Control unrun model filtering
#'   \item [set_spec_summary_filter()] - Set summary filter rules
#' }
#'
#' @section Rule Builders:
#' Functions for creating rule expressions:
#' \itemize{
#'   \item [section_rules()] - Create section assignment rules
#'   \item [filter_rules()] - Create row filter rules
#'   \item [variability_rules()] - Create variability display rules
#'   \item [summary_filter_rules()] - Create summary filter rules
#' }
#'
#' @section Rendering:
#' Functions for rendering tables to output formats:
#' \itemize{
#'   \item [render_to_gt()] - Convert HyperionTable to gt
#'   \item [render_to_flextable()] - Convert HyperionTable to flextable
#' }
#'
#' @section Model Comparison:
#' Functions for comparing models:
#' \itemize{
#'   \item [compare_with()] - Join parameter data for side-by-side comparison
#'   \item [add_model_lineage()] - Attach lineage for conditional LRT display
#'   \item [add_summary_info()] - Attach summary info for table footnotes
#' }
#'
## usethis namespace: start
#' @importFrom hyperion ModelComments
#' @importFrom hyperion OmegaComment
#' @importFrom hyperion ThetaComment
#' @importFrom hyperion SigmaComment
#' @importFrom hyperion are_models_in_lineage
#' @importFrom hyperion compute_ci
#' @importFrom hyperion compute_cv
#' @importFrom hyperion compute_rse
#' @importFrom hyperion format_omega_display_name
#' @importFrom hyperion read_model
#' @importFrom hyperion get_parameter_names
#' @importFrom hyperion get_parameter_transform
#' @importFrom hyperion get_parameter_unit
#' @importFrom hyperion transform_value
#' @importFrom rlang .data
## usethis namespace: end
#'
#' @keywords internal
"_PACKAGE"
