# hyperion.tables 0.4.0

## New Features

* Added PNG image export support for both `gt` and `flextable` table outputs via `render_to_image()`.
* Added `render_to_image()` S3 methods for `gt_tbl` and `flextable`, with optional `path` for writing files.

## Improvements

* Fixed description column ordering behavior when explicit column selections are used.
* Improved flextable image snapshot trimming/rendering consistency.

# hyperion.tables 0.3.0

## Breaking Changes

* `apply_summary_spec()` no longer has a default for `spec`. Pass `SummarySpec()` explicitly.

## New Features

* `set_spec_tag_filter()` gains an `exclude` parameter to remove models by tag
  (e.g., `set_spec_tag_filter(exclude = "failed")`).
* **Section rules for summary tables.** `section_rules()` / `set_spec_sections()` now work with `SummarySpec`, enabling model grouping in summary tables (e.g., `"base" %in% tags ~ "Base Models"`). Rules are evaluated row-by-row to support list columns like `tags`.
* **Section filtering.** `set_spec_section_filter()` excludes entire sections from both parameter and summary tables. Pass `NA` to also remove rows that didn't match any section rule.
* `set_spec_sections()` promoted from TableSpec-only to a common modifier that works on both `TableSpec` and `SummarySpec`.
* `get_spec_section_filter()` getter for reading the current section filter.

## Bug Fixes

* Fixed crash when multiple section rules mapped to the same label (e.g., two rules both producing `"Base Models"`). Duplicate factor levels are now deduplicated before ordering.
* Multi-match section warning now only fires when a row matches rules with genuinely different labels. Same-label multi-match (intentional overlap) no longer warns.
* `katex` moved back to Suggests (from Imports) with a one-time warning when missing, instead of aborting `render_to_gt()`.

# hyperion.tables 0.2.1
* Updated katex dep from suggests to imports

# hyperion.tables 0.2.0

## Breaking Changes

* `apply_summary_spec()` now aborts when no models remain after filtering or the tree is empty, instead of silently returning an empty data frame.
* `compare_with()` now aborts when `reference_model` does not match any model in the comparison, instead of silently falling through to the default.

## Migration Notes (0.2.0)

* `compare_with(reference_model = ...)`: use an existing model `run_name` or label already present in the comparison chain; unmatched values now error.
* `apply_summary_spec()`: if your filters or input tree can produce zero models, handle that case before calling `apply_summary_spec()` instead of relying on an empty-table return.
* `compare_with(labels = ...)` in chained comparisons (`params1` is already a comparison):
  * length 1: append the new label to existing labels.
  * length 2: rename the previous/latest existing label to `labels[1]`, then append `labels[2]` for the newly added model.

## New Diagnostics

* `compare_with()` warns when models share no parameters after joining.
* Summary table dOFV calculation warns when a model has multiple `based_on` parents, indicating which parent is used.
* Footnote about number of observations not matching reference model is now an `rlang::inform` instead of footnote.
* LRT suppression now emits an informational message with the reason (e.g., "models not in direct lineage", "degrees of freedom is zero").
* `make_comparison_table()` validates the comparison is renderable before building, aborting early with actionable hints if no rows or model columns remain.
* `SummarySpec` now validates `n_decimals_ofv` at construction time.

## Bug Fixes

* Fixed incorrect LRT p-values in model comparison output.
* Fixed p-value rendering when `df` column is dropped.
* Fixed `format_time_columns` crash when all time values are NA in auto mode.
* Fixed missing parameter-count values in rows used for dOFV/p-value calculations.
* Fixed summary table generation so one unreadable or unsummarizable model no longer aborts the entire table.
* Fixed cases where model files with extensions were not resolved correctly.
* Fixed `source_dir` resolution to use config-relative path resolution, so relative paths in `pharos.toml` are handled correctly.

# hyperion.tables 0.1.0

* Initial release.
