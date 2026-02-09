# hyperion.tables 0.2.0

## Breaking Changes

* `apply_summary_spec()` now aborts when no models remain after filtering or the tree is empty, instead of silently returning an empty data frame.
* `compare_with()` now aborts when `reference_model` does not match any model in the comparison, instead of silently falling through to the default.

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
