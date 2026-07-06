# hyperion.tables (development version)

## Bug Fixes

* Numeric formatting no longer switches to scientific notation for values at or
  above `10^n_sigfig` (e.g. `1000`, `4090`), matching the significant-figure
  style used elsewhere; non-finite values are handled instead of padded.
* Variability rule conditions (e.g. `cv > 30`) are evaluated against raw numeric
  values instead of formatted strings, so numeric comparisons are no longer
  lexicographic. The built-in default rule now correctly blanks `cv == 0`.
* Parameter name resolution matches each row independently and on
  `(name, kind)`, so an uncommented parameter no longer corrupts the
  `nonmem_name`/`user_name` of the others, and a THETA sharing a display name
  with an OMEGA/SIGMA no longer collides.
* `display_transforms` no longer transforms columns that were not requested when
  the same column is listed for every kind. `ci_low`/`ci_high` are no longer
  accepted as (non-functional) transform targets.
* A missing (`NA`) parameter transform now degrades to an untransformed display
  with a warning instead of crashing with a low-level index error.
* Chaining `compare_with()` after `add_model_lineage()` preserves the lineage, so
  LRT footnotes are retained.
* Section rule labels may be variables or expressions, not only string literals.
* `n_decimals_ofv = NA` keeps significant-figure formatting instead of crashing
  when rendering summary tables; delta-OFV values no longer render as `-0.000`.
* Duplicate comparison model labels now abort with an actionable error instead of
  a gt error / silent flextable spanner merge.
* `apply_table_spec()` validates its `params` argument up front with an
  actionable message.
* flextable equation footnotes no longer crash when `equatags` is unavailable.
* flextable Word export of sectioned tables no longer collapses to unreadable
  (1–2 pt) fonts; column widths are scaled proportionally instead.
* gt Word export writes atomically (a failed sanitization no longer leaves a
  broken file at the target path) and no longer mangles literal dollar amounts
  into equations.
* `set_spec_sections()` warns when given the other spec class's assignment
  argument, and preserves inline items across repeated calls. The deprecated
  `set_spec_section_filter()` again accepts positional section labels.

# hyperion.tables 0.5.0

## Breaking Changes

* `ParameterNameOptions()` no longer accepts `append_omega_with_theta`. Omega/theta name joining is handled upstream by `hyperion`; remove the argument from any `ParameterNameOptions()` calls.
* `TableSpec()` and `SummarySpec()` no longer have a `section_filter` property. Filtering is folded into the new `SectionOptions` object stored on `@sections`. Configure filters with `set_spec_sections(keep = ...)` or `set_spec_sections(exclude = ...)`. `NA` in either vector still targets rows that didn't match any section rule.
* The `sections =` constructor argument now takes a `SectionOptions` object, not a list of formulas. Existing code that passes `sections = section_rules(...)` directly will fail to validate — build the spec via the pipe instead:

  ```r
  spec <- TableSpec() |>
    set_spec_sections(
      kind == "THETA" ~ "Structural",
      kind == "OMEGA" ~ "IIV"
    )
  ```
* `set_spec_section_filter()` is deprecated and will be removed in 0.6.0. Replace with `set_spec_sections(keep = ...)` / `set_spec_sections(exclude = ...)`.

## New Features

### Word output

* `render_to_word(table, path, landscape = FALSE)` saves a rendered `gt` or `flextable` object to a `.docx` file. For `gt` output, inline LaTeX (`$...$`) in column labels and footnotes is converted to native Word OMML equations via `equatags`, so Word renders them as equations rather than literal text. For `flextable` output, the table is fitted to page width before saving. Pass `landscape = TRUE` to write US Letter landscape (11 × 8.5 in).
* For comparison tables, the per-model left border that separates model column groups in the `gt` output is preserved in the Word export.

### Unified section configuration

* New exported `SectionOptions` S7 class bundles section `rules`, per-item `assignments`, display `order`, and `filter` into one property on `TableSpec` / `SummarySpec`. Build it through `set_spec_sections()`; direct construction is rarely needed.
* `set_spec_sections()` is now the single entry point for everything section-related: rules (via `...` or `sections =`), display ordering (`order =`), filtering (`keep =`, `exclude =`), and per-item assignments. Defaults of `NULL` mean "leave alone"; pass `character(0)` to clear.
* `set_spec_sections(<TableSpec>)` gains two arguments for assigning specific parameters to sections:
  * `parameters` — a named list keyed by section label, e.g. `parameters = list("Covariate Parameters" = c("CAP-D1", "WT-V2/F"))`.
  * `file` — path to a TOML lookup where each `[parameter]` entry can carry a `section = "..."` field. Inline `parameters =` values win on conflict with a warning.
* `set_spec_sections(<SummarySpec>)` gains a `models` argument for assigning specific runs to sections, e.g. `models = list("Selected Models" = c("run001", "run002"))`.
* Section filtering now warns when labels in `keep` / `exclude` don't match any section in the data, and when the filter removes every row.

### Markdown and LaTeX in gt output

* `gt` column labels and footnotes containing `$...$` are now rendered as markdown / equations rather than literal text.

### Additional getters

New exports round out the spec getter API: `get_spec_footnotes()`, `get_spec_hide_empty()`, `get_spec_missing()`, `get_spec_models()`, `get_spec_ofv_decimals()`, `get_spec_parameter_sections()`, `get_spec_pvalue()`, `get_spec_remove_unrun()`, `get_spec_summary_filter()`, `get_spec_tag_filter()`.

## Bug Fixes

* `make_comparison_table()` no longer drops columns when an added model lacks columns present on the reference model.
* Section-filter warnings now correctly identify unmatched labels and report when filtering removed every row, instead of silently returning an empty table.

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
