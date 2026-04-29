# BaseSpec Refactor Plan

## Motivation

`TableSpec` and `SummarySpec` share ~11 properties with parallel-but-not-quite-equal handling. The agent review's Theme 1 ("dual-class drift") flagged this as the highest-leverage architectural fix in the package. Today, common properties are duplicated in both class definitions; setters/getters either dispatch on `AnySpec` (a union) or have near-identical methods per class.

A `BaseSpec` parent class would:

- Centralize common property declarations and their defaults.
- Replace the `AnySpec` union with a real parent type.
- Give shared validators one place to live.
- Set up future shared method dispatches (`set_spec_title`, `set_spec_sigfig`, etc.) on `BaseSpec` instead of `AnySpec`.

Public API stays identical. Users continue to call `TableSpec()` / `SummarySpec()` and the same setters/getters.

## Property inventory

### Common (move to BaseSpec)

| Property              | Same default? | Notes                                                                |
| --------------------- | ------------- | -------------------------------------------------------------------- |
| `title`               | No            | TableSpec: `"Model Parameters"`; SummarySpec: `"Run Summary"`        |
| `sections`            | Yes           | `SectionOptions()`                                                   |
| `columns`             | No            | TableSpec defaults to parameter columns; SummarySpec to summary cols |
| `add_columns`         | Yes           | `NULL`                                                               |
| `drop_columns`        | Yes           | `NULL`                                                               |
| `hide_empty_columns`  | Yes           | `TRUE`                                                               |
| `n_sigfig`            | Yes           | `3`                                                                  |
| `n_decimals_ofv`      | Yes           | `3`                                                                  |
| `pvalue_scientific`   | Yes           | `FALSE`                                                              |
| `pvalue_threshold`    | Yes           | `NULL`                                                               |
| `footnote_order`      | No            | TableSpec: 3 entries; SummarySpec: 1                                 |

Properties with diverging defaults can be handled either by re-declaring in the child to override only the default, or by passing the value through the child constructor into `S7::new_object()`. Verify which idiom S7 prefers in Phase 1.

### Stays on `TableSpec`

`parameter_names`, `row_filter`, `display_transforms`, `variability_rules`, `ci`, `missing_text`, `missing_apply_to`, `.columns_provided`

### Stays on `SummarySpec`

`models_to_include`, `tag_filter`, `tag_exclude`, `summary_filter`, `remove_unrun_models`, `time_format`

## Open design questions

These should be confirmed in Phase 1 before committing to the broader migration.

1. **Abstract base or instantiable?** Users shouldn't construct a bare `BaseSpec()`. S7 supports `abstract = TRUE`; verify it gives a useful error if a user attempts construction.

2. **Validator chaining.** S7 validators do *not* auto-chain across inheritance. Two viable patterns:
   - Child validator explicitly invokes `S7::validate(self, parent_class)` first, then adds class-specific checks.
   - Common checks live as a free helper function `validate_base_spec(self)` that both child validators call.

   The free-helper pattern is simpler and more explicit. Recommended unless S7 has a stronger idiom.

3. **`AnySpec` → `BaseSpec`.** Once `BaseSpec` is the real parent, the `AnySpec <- S7::new_union(...)` declaration is redundant. Method dispatches on `AnySpec` become dispatches on `BaseSpec`. No public API change.

## Migration sequencing

Each phase is its own commit; tests stay green at every phase boundary.

### Phase 1 — Skeleton (low risk, ~30 min)

- Define an empty `BaseSpec` (abstract).
- Change `TableSpec` and `SummarySpec` to declare `parent = BaseSpec`.
- Verify everything still loads, all tests pass.
- Validates that the inheritance pattern works before any property migration.

### Phase 2 — Migrate uncontroversial props (medium)

In small groups, with tests between each:

1. Single-default scalars: `n_sigfig`, `n_decimals_ofv`, `hide_empty_columns`, `pvalue_scientific`, `pvalue_threshold`.
2. Section-related: `sections`.
3. Column family: `add_columns`, `drop_columns`. (Hold `columns` for next phase since it has differing defaults.)

### Phase 3 — Migrate diverging-default props

`title`, `columns`, `footnote_order`. Decide on the override pattern in Phase 1.

### Phase 4 — Validator consolidation

Move common-property validation logic into a shared `validate_base_spec()` helper. Child validators call it first, then run their class-specific checks.

### Phase 5 — Retire `AnySpec`

Replace all `AnySpec` method dispatches with `BaseSpec`. Remove the `AnySpec` union declaration. Update any call sites that used the union explicitly.

### Phase 6 (optional) — Method consolidation

Move shared method bodies (e.g., `set_spec_title`, `get_spec_title`, `set_spec_sigfig`) to dispatch on `BaseSpec` instead of having two near-identical methods.

## Risks & mitigations

- **S7 inheritance with overridden defaults may force re-declaration of the property** in the child class. Net benefit becomes "unified type" rather than "deduplicated declaration." Still worth doing — the type/dispatch wins remain.
- **Constructors stay class-specific.** They have class-specific logic (`expand_ci_alias`, `merge_summary_columns`, etc.). The plan does not attempt to dedupe constructors.
- **`AnySpec` references in user code.** If users typed against `AnySpec` (unlikely but possible), they'd need to update. We can keep `AnySpec <- BaseSpec` as a deprecation alias for a release cycle.

## Effort estimate

- Phase 1: ~30 min, low risk.
- Phases 2–5: ~2 hours total, several small commits.
- Phase 6 (optional): another ~30–60 min, mostly deletion.

## What success looks like

- `TableSpec` and `SummarySpec` declare only their class-specific properties.
- Common-property defaults live in one place (modulo class-specific overrides).
- `AnySpec` is gone; method dispatches are on the real parent.
- All 460+ tests still pass with no snapshot changes.
- Public API and behavior are unchanged.
