# hyperion.tables (development version)

## Breaking Changes

* Column precedence is now resolved in one place with the documented rule
  `(columns %||% default_columns) ∪ add_columns − drop_columns` (drop beats
  add). `get_spec_columns()` now subtracts `drop_columns` as its
  documentation always promised, and every internal consumer (section
  ordering, hidden-column resolution, footnote statistic detection,
  variability planning, comparison column selection, description
  enrichment) uses the same resolver. Code relying on the old
  inconsistencies (e.g. a column listed in both `add_columns` and
  `drop_columns` still displaying) will see the documented behavior instead.
* `add_columns` now works for comparison tables: added columns are carried
  through the join with model suffixes instead of silently no-oping.
* Dropping a per-model CI alias (`ci_1`, `ci_left`, ...) no longer removes
  the *other* models' CI columns from the effective column set; only the
  plain `"ci"` alias means "drop CI everywhere".
* `compare_with()` joins parameters by name AND kind when both frames carry
  `kind`, and aborts on duplicated (name, kind) identities. Previously a
  THETA named "CL" silently paired with an OMEGA commented "CL", fanning
  out duplicated rows and cross-kind percent changes.
* `apply_summary_spec()` aborts when a lineage tree contains models whose
  basename stems collide (e.g. `base/run001.mod` and `covariate/run001.mod`).
  The summary pipeline identifies models by stem, so colliding stems
  silently loaded the wrong model and computed dOFV against the wrong
  parent.
* `set_spec_sections()` now aborts (with a pointer to the right argument)
  when given the other spec type's arguments (`models =` on a TableSpec;
  `parameters =` / `file =` on a SummarySpec) instead of silently dropping
  them.
* dOFV/LRT are suppressed (with an explanatory message) when the two models'
  estimation methods are known and differ — OFVs from different likelihood
  approximations are not comparable. Applies to both summary and comparison
  tables.
* Requires R >= 4.2 (matching the tested matrix) and dplyr >= 1.0.0 (the
  attribute-preservation semantics the pipeline relies on).

## Bug Fixes

* Comparison-table LRT degrees of freedom are now computed from the models'
  free-parameter counts (captured by `add_summary_info()`), not from the rows
  that survived display filtering. Row/section filters and column selections
  (e.g. specs without CI or variability columns) no longer silently change or
  suppress the printed p-value, and the comparison table now always agrees
  with the run-summary table for the same model pair.
* The comparison LRT is oriented by free-parameter count: the model with fewer
  free parameters is treated as the reduced model, so `compare_with()` called
  in reversed (child, parent) order now prints the correct p-value instead of
  a value near 1.
* When the LRT is suppressed despite both OFVs and matching observation counts
  being present (e.g. zero degrees of freedom or missing free-parameter
  counts), an explanatory message is now emitted instead of the footnote
  silently vanishing.
* Large-magnitude values no longer flip to scientific notation mid-table:
  the default formatter renders fixed notation up to the band
  [1e-4, 1e6) (`1000` now renders as `"1000"`, not `"1e+03"`), and values
  beyond it use scientific notation with the full number of significant
  figures (`1e-05` renders as `"1.00e-05"`, not `"1e-05"`).
* Comparison-table footnote statistic detection is suffix-aware: CV/SD/Corr
  abbreviations and CV% formula footnotes now appear for comparison tables
  (the `transforms` column is carried through the join), RSE/SE/shrinkage
  detection covers any model count (not just `_1`/`_2`), and abbreviations
  are no longer emitted for statistics the table doesn't display.
* Word export (`render_to_word()`): summary/comparison headers no longer
  show literal `**` around math-bearing labels (the LaTeX rewrite now runs
  before header bolding); the sanitized document is zipped to a tempfile
  and swapped in so a zip failure can't destroy the file; a sanitization
  failure now raises an error naming the unsanitized file instead of
  silently shipping it; `\$` escapes a literal dollar sign so text like
  "costs \$5,000" is not rewritten as math; landscape export warns when no
  section properties exist to modify; malformed `gridSpan` values no longer
  crash the table-grid pass.
* Flextable output matches gt: `bold_locations` from the table IR is
  honored (titles/spanners/row groups are no longer unconditionally bold),
  IR borders apply to the body only (as in gt), comparison tables get the
  same black model-boundary border gt draws, and a left border on the first
  column no longer produces an invalid `vline(j = 0)` call.
* The no-equatags flextable footnote fallback transliterates LaTeX
  generically (innermost-first reduction with protected subscripts) instead
  of byte-matching whole formulas — editing a formula can no longer produce
  a mathematically different plain-text form (e.g. the `- 1` migrating
  outside a square root). Column labels share the same transliteration, so
  LaTeX labels degrade to the same plain text in both renderers.
* Dynamic section rule labels (`kind == "THETA" ~ label_var`) no longer
  crash with an opaque vapply type error: labels are resolved the same way
  the validator resolves them, with a clear error when a label cannot be
  resolved to a single string.
* `set_spec_ofv_decimals(NA)` no longer crashes summary rendering; NA/unset
  OFV decimals route through the same hyperion formatter the comparison
  path uses.
* With `pvalue_scientific = FALSE`, small p-values now honor the setting
  instead of falling back to scientific notation.
* A computed p-value with a missing df now displays without the df suffix
  instead of rendering as an empty cell.
* dOFV now explains itself when the reference model is missing from the
  summary table, and summary section rules that fail to evaluate (e.g. a
  typo'd column name) warn instead of silently matching nothing.
* `time_format = "auto"` no longer guesses the time unit from already-
  divided values when the unit attribute was stripped (the guess was wrong
  in every decidable case); it warns and omits the unit suffix instead.
* Comparison tables built from metadata-stripped inputs are loud:
  `make_comparison_table()` aborts naming base-R subsetting as the cause,
  and chaining `compare_with()` onto a stripped comparison warns that the
  earlier labels/summaries/references are lost.
* The release workflow now runs `R CMD check` (including the test suite) on
  the tagged commit before building or publishing anything.

## Internal

* Removed dead code: the unused second footnote pipeline
  (`add_footnotes()`, `add_conditional_footnotes()`,
  `apply_comparison_footnotes()`, `apply_model_spanners()`,
  `convert_footnote_to_text()`), `render_gt_summary_table()`,
  `get_comparison_last_two()`, and the unused `include_associated_theta`
  argument of `comment_keys_for()`.

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
