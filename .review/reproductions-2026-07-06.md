# hyperion.tables — Reproductions for reviewed issues (2026-07-06)

Runnable examples for every finding in `findings-2026-07-06.md` that was
reproduced by execution. Each snippet is self-contained and was verified against
the freshly built package (hyperion.tables 0.5.0.9000, hyperion 0.5.0, dplyr
1.2.0, gt 1.3.0, flextable 0.9.11, equatags 0.2.2, pandoc 3.1.3). The output
shown under each block is the actual observed output.

Run everything from the repo root (its `.Rprofile` activates the rv library).
Snippets marked *(internal)* reach unexported functions via
`ns <- getNamespace("hyperion.tables")`; those functions are the exact ones on
the production call path.

Not reproduced here: **F8** (`sanitize_gt_docx` drift) is a latent risk — the
current output is sound, so there is nothing to trigger without artificially
downgrading gt/pandoc; and the flextable bold/border divergences are structural
(verified by reading both renderers) rather than a single observable value.

## Shared preamble

```r
options(hyperion.config_dir = system.file(package = "hyperion.tables"))
suppressMessages(library(hyperion.tables))
ns <- getNamespace("hyperion.tables")
model_dir <- system.file("extdata/models/onecmt", package = "hyperion.tables")

# Standard enrichment: get_parameters -> apply_table_spec -> add_summary_info
enrich <- function(name, spec = TableSpec(display_transforms = list(omega = "cv"))) {
  m <- hyperion::read_model(file.path(model_dir, paste0(name, ".mod")))
  hyperion::get_parameters(m) |>
    apply_table_spec(spec, hyperion::get_model_parameter_info(m)) |>
    add_summary_info(summary(m))
}
```

---

## F1 (Critical) — `format_sigfig_pad` emits scientific notation, dropping significant figures

`R/variability.R:43-46`. `formatC(x, format="g")` goes scientific at exponent ≥
digits and the function returns it unpadded, so the sig-fig count silently varies
and raw `e+03` notation lands in cells. The sibling hyperion formatter never does
this — cells and footnotes of the same table disagree.

```r
f <- ns$format_sigfig_pad                       # (internal)
for (x in c(999.4, 999.5, 1234, 3497.66, 4085.65)) {
  cat(sprintf("format_sigfig_pad(%g, 3) = %-9s | hyperion = %s\n",
              x, sQuote(f(x, 3)),
              sQuote(hyperion::format_hyperion_sigfig_string(x, 3))))
}
```

```
format_sigfig_pad(999.4, 3) = ‘999’      | hyperion = ‘999’
format_sigfig_pad(999.5, 3) = ‘1e+03’    | hyperion = ‘1000’     <- 1 sig fig
format_sigfig_pad(1234, 3) = ‘1.23e+03’  | hyperion = ‘1230’
format_sigfig_pad(3497.66, 3) = ‘3.5e+03’| hyperion = ‘3500’     <- 2 sig figs
format_sigfig_pad(4085.65, 3) = ‘4.09e+03’| hyperion = ‘4090’
```

`999.4` → `999` but `999.5` → `1e+03`: adjacent values in a sorted column render
in different notation with different implied precision.

---

## F2 (High) — Variability rule predicates compared lexicographically on stringified numbers

`R/variability.R:110-113`. `format_numeric_for_rules` stringifies every numeric
column *before* the rule quosures run, so `cv > 30` becomes a string comparison.

```r
spec <- TableSpec(n_sigfig = 3) |>
  set_spec_variability(
    cv > 30 ~ "(HIGH)",
    !is.na(cv) ~ "(normal)",
    TRUE ~ NA_character_,
    overwrite = TRUE
  )
d <- data.frame(kind = "OMEGA", cv = c(9.5, 40, 120, 200))
data.frame(cv = d$cv, label = ns$build_variability_parameter(d, spec))   # (internal)
```

```
     cv    label
1   9.5   (HIGH)     <- wrong: "9.50" > "30" lexicographically
2  40.0   (HIGH)
3 120.0 (normal)     <- wrong: "120"  < "30" lexicographically
4 200.0 (normal)     <- wrong
```

Even the built-in default rule is defeated at `cv == 0` (its `cv != 0` guard
becomes `"0.00" != 0`):

```r
ns$build_variability_parameter(data.frame(kind = "OMEGA", cv = c(0, 25)),
                               TableSpec(n_sigfig = 3))
#> [1] "(CV = 0.00%)" "(CV = 25.0%)"      # cv==0 should render blank
```

---

## F3 (High) — One uncommented parameter corrupts `nonmem_name` for every row

`R/parameter-apply.R:125-138`. The NONMEM→display name fallback is all-or-nothing;
if any parameter matches by NONMEM name, the fallback never runs and every other
row loses its NONMEM name.

The `.mod` file must live under the config root, so replicate the tree in a temp
dir first:

```r
suppressMessages(library(hyperion.tables))
pkg_root <- system.file(package = "hyperion.tables")
root <- tempfile("cfg"); dir.create(root)
file.copy(file.path(pkg_root, "extdata"), root, recursive = TRUE)
for (f in c("lookup.toml", "pharos.toml"))
  if (file.exists(file.path(pkg_root, f))) file.copy(file.path(pkg_root, f), root)
options(hyperion.config_dir = root)
mp <- file.path(root, "extdata/models/onecmt/run001.mod")
orig <- readLines(mp)

read_enrich <- function() {
  m <- hyperion::read_model(mp)
  hyperion::get_parameters(m) |>
    apply_table_spec(
      TableSpec(parameter_names = ParameterNameOptions(source = "display")),
      hyperion::get_model_parameter_info(m))
}

# Baseline: all params commented
e0 <- read_enrich()
print(data.frame(name = e0$name, kind = e0$kind, nonmem_name = e0$nonmem_name)[1:6, ])

# Strip the "; 2. TVV (L)" comment from the $THETA block
writeLines(sub("^\\(0, 30\\).*$", "(0, 30)", orig), mp)
e1 <- read_enrich()
print(data.frame(name = e1$name, kind = e1$kind, nonmem_name = e1$nonmem_name)[1:6, ])
writeLines(orig, mp)   # restore
```

```
# BASELINE (correct): every nonmem_name is a NONMEM name
        name  kind nonmem_name
1       TVCL THETA      THETA1
2        TVV THETA      THETA2
3       TVKA THETA      THETA3
4 OM1 (TVCL) OMEGA  OMEGA(1,1)
...

# TVV uncommented (corrupted): only TVV resolves; all others degrade to display names
        name  kind nonmem_name
1       TVCL THETA        TVCL      <- should be THETA1
2        TVV THETA      THETA2
3       TVKA THETA        TVKA      <- should be THETA3
4 OM1 (TVCL) OMEGA  OM1 (TVCL)      <- should be OMEGA(1,1)
```

Sections/filters/lookup TOMLs keyed on `nonmem_name` now silently match nothing.

---

## F4 (High) — Per-column `display_transforms` silently transform unlisted columns

`R/parameter-apply.R:331-334`. Restricting transforms to the `estimate` column
still transforms cv/rse/ci, because the `dt_all` fallback maps unlisted columns to
every kind.

```r
spec_est <- TableSpec(display_transforms =
  list(theta = "estimate", omega = "estimate", sigma = "estimate"))
m <- hyperion::read_model(file.path(model_dir, "run001.mod"))
e <- suppressMessages(apply_table_spec(hyperion::get_parameters(m), spec_est,
                                       hyperion::get_model_parameter_info(m)))
om <- e$kind == "OMEGA"
data.frame(name = e$name[om], estimate = round(e$estimate[om], 4),
           cv = round(e$cv[om], 4), ci_low = round(e$ci_low[om], 4))
```

```
        name estimate      cv ci_low
1 OM1 (TVCL)   1.1399 37.4018 1.0238
2  OM2 (TVV)   1.1453 38.1213 1.0612
3 OM3 (TVKA)   1.1052 32.4301     NA
```

`cv` is populated and `ci_low ≈ 1.0` (exp/CV scale) even though the spec asked to
transform only `estimate` — the derived stats are silently on the wrong scale.

---

## F5a (Medium) — A suffix-shaped user column fabricates a phantom model

`R/compare.R:33`. Model identity is inferred purely by regex over column names, so
any `<base>_<n>` column becomes a "model".

```r
e1 <- suppressMessages(enrich("run001")); e2 <- suppressMessages(enrich("run002"))
cmp <- suppressMessages(compare_with(e1, e2, labels = c("run001", "run002")))
idx <- function(df) ns$get_comparison_model_indices(names(df), ns$comparison_suffix_columns())
cat("real indices:", idx(cmp), "\n")
cmp$estimate_9 <- 42            # user post-processes the comparison data frame
cat("after adding estimate_9:", idx(cmp), "\n")
```

```
real indices: 1 2
after adding estimate_9: 1 2 9      <- 9 is a fabricated model
```

Rendering this table produces a "Model 3" spanner with `N/A (no summary)`
footnotes.

---

## F5b (Medium) — Two CI-alias vocabularies disagree

`R/spec-validation.R:94-105`. `expand_ci_drop_columns` hardcodes aliases only up to
`ci_2` and expands them to *global unsuffixed* `ci_low/ci_high`; the layout scopes
per-model. So `drop_columns="ci_2"` and `="ci_3"` behave differently.

```r
ecd <- ns$expand_ci_drop_columns          # (internal)
ecd("ci_2")   #> "ci_2"  "ci_low" "ci_high"   -> expands to GLOBAL ci bounds
ecd("ci_3")   #> "ci_3"                        -> not in alias list, no expansion
```

Consequence (from the T-suite probes): on a 3-model chain, `drop_columns="ci_2"`
drops the CI equation footnote for *all* models while leaving CI columns visible;
`drop_columns="ci_3"` keeps the footnote. On a single-model table,
`drop_columns="ci_1"` deletes the entire CI.

---

## F5c (Medium) — Comparison footnote/abbreviation detection is blind to suffixed columns

`R/format.R:186-234`. `detect_table_statistics` checks unsuffixed `cv`/`stderr`/…,
so a comparison carrying `cv_1`/`cv_2`/`stderr_*` shows the columns but omits their
footnotes.

```r
single  <- data.frame(kind = "OMEGA", cv = 0.3, stderr = 0.1, transforms = "LogNormal")
compare <- data.frame(kind = "OMEGA", cv_1 = 0.3, cv_2 = 0.4,
                      stderr_1 = 0.1, stderr_2 = 0.2)
s1 <- ns$detect_table_statistics(single)     # (internal)
s2 <- ns$detect_table_statistics(compare)
cat("single-model: has_cv =", s1$has_cv, " has_stderr =", s1$has_stderr, "\n")
cat("comparison  : has_cv =", s2$has_cv, " has_stderr =", s2$has_stderr, "\n")
```

```
single-model: has_cv = TRUE  has_stderr = TRUE
comparison  : has_cv = FALSE has_stderr = FALSE     <- footnotes silently omitted
```

---

## F6 (High) — flextable equation-footnote fallback crashes on every input without equatags

`R/render-flextable.R:553`. `as_paragraph(!!!chunks)` — flextable's `as_paragraph`
is not rlang-aware, so `!!!` parses as triple negation of a list.

```r
ns$build_footnote_paragraph("95% CI: $\\mathrm{Estimate} \\pm z \\cdot \\mathrm{SE}$")
#> Error in !chunks : invalid argument type
```

This is the no-equatags path, taken on any install with flextable but without the
(Suggests-only) equatags. Regression from commit `db78d87`, which replaced a
working `do.call(flextable::as_paragraph, chunks)`.

---

## F7 (High) — flextable Word export of sectioned tables collapses to 1–2 pt

`R/render-flextable.R:59,114,718`. `autofit` re-inflates the 0.01″ section column;
`fit_to_width(7)` then fits by shrinking fonts.

```r
m <- hyperion::read_model(file.path(model_dir, "run001.mod"))
info <- hyperion::get_model_parameter_info(m)
font_sizes <- function(spec) {
  ft <- hyperion::get_parameters(m) |>
    apply_table_spec(spec, info) |> add_summary_info(summary(m)) |>
    make_parameter_table(output = "flextable")
  out <- tempfile(fileext = ".docx"); suppressWarnings(render_to_word(ft, out))
  ex <- tempfile(); unzip(out, exdir = ex)
  xml <- paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE), collapse = "")
  vals <- as.integer(sub('.*"([0-9]+)".*', "\\1",
            regmatches(xml, gregexpr('w:sz w:val="[0-9]+"', xml))[[1]])) / 2
  sprintf("min = %g pt, max = %g pt", min(vals), max(vals))
}
base <- TableSpec(display_transforms = list(omega = "cv"), n_sigfig = 3)
cat("no sections:", font_sizes(base), "\n")
cat("sectioned  :", font_sizes(base |> set_spec_sections(
  kind == "THETA" ~ "Structural model parameters",
  kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
  kind == "SIGMA" ~ "Residual error")), "\n")
```

```
no sections: min = 8 pt, max = 8 pt
sectioned  : min = 2 pt, max = 11 pt      + warning "results in negative font sizes"
```

---

## F9 (Medium) — An `NA` transform crosses the API as a cryptic Rust panic

`R/parameter-apply.R:54-57`. A THETA commented with the same display name as a
SIGMA yields `NA` from `get_parameter_transform`, which is passed unguarded into
the Rust compute functions.

Reuse the temp-config-root and `orig` from F3, then:

```r
writeLines(sub("^\\(0, 30\\).*$", "(0, 30)   ; 2. Additive", orig), mp)  # theta named like the SIGMA
m <- hyperion::read_model(mp)
p <- hyperion::get_parameters(m); info <- hyperion::get_model_parameter_info(m)
print(hyperion::get_parameter_transform(info, p$name, p$kind))
apply_table_spec(p, TableSpec(), info)
writeLines(orig, mp)   # restore
```

```
[1] "Identity"  NA  "Identity" "LogNormal" ...      <- NA for the collided theta
Error in `compute_cv()`: ! NA transform at index 2
  (raw index error; no parameter name, whole table lost)
```

---

## F10 (Medium) — Chaining `compare_with()` after `add_model_lineage()` drops lineage

`R/compare.R:599-635`. `finalize_comparison` rebuilds `hyperion_meta` with only
four of the five fields, dropping `lineage` — so a chained comparison loses all
LRT footnotes.

```r
e1 <- suppressMessages(enrich("run001")); e2 <- suppressMessages(enrich("run002"))
e3 <- suppressMessages(enrich("run003"))
base <- suppressMessages(compare_with(e1, e2, labels = c("run001", "run002")))
lin  <- suppressMessages(add_model_lineage(base, hyperion::get_model_lineage()))
cat("after add_model_lineage:", paste(names(ns$get_comparison_meta(lin)), collapse = ", "), "\n")
chained <- suppressMessages(compare_with(lin, e3, labels = "run003"))
cat("after chaining        :", paste(names(ns$get_comparison_meta(chained)), collapse = ", "), "\n")
```

```
after add_model_lineage: summaries, labels, table_spec, pct_change_refs, lineage
after chaining         : summaries, labels, table_spec, pct_change_refs
                                                                    ^ lineage gone
```

The rendered chained table has zero LRT footnote lines and prints the misleading
message "LRT suppressed … no lineage attached".

---

## Additional reproduced issues

### Dynamic (variable) section-rule labels crash `apply_table_spec`
`R/parameter-apply.R:365,424-432`. The `SectionOptions` validator supports a
non-literal RHS, but the apply stage `vapply`s `f_rhs` as character.

```r
lbl <- "Structural model parameters"
spec <- TableSpec() |> set_spec_sections(kind == "THETA" ~ lbl, TRUE ~ "Other")
m <- hyperion::read_model(file.path(model_dir, "run001.mod"))
apply_table_spec(hyperion::get_parameters(m), spec, hyperion::get_model_parameter_info(m))
#> Error in vapply(): values must be type 'character', but FUN(X[[1]]) result is type 'symbol'
```

### `n_decimals_ofv = NA` validates as legal but crashes summary rendering
`R/spec-validation.R:223-242` allows NA; `R/render-common.R:147` calls
`formatC(digits = NA)`.

```r
SummarySpec(n_decimals_ofv = NA_real_)          # accepted, no error
st  <- suppressMessages(apply_summary_spec(hyperion::get_model_lineage(),
                                           SummarySpec(n_decimals_ofv = NA_real_)))
tbl <- suppressMessages(make_summary_table(st, output = "data"))
apply_formatting(tbl)
#> Error: missing value where TRUE/FALSE needed
```

### dOFV / ΔOFV can render as signed zero `-0.000`
`R/render-common.R:147`, `R/compare-table.R:264-271`.

```r
formatC(-0.0004, digits = 3, format = "f")                       #> "-0.000"
hyperion::format_hyperion_decimal_string(-0.0004, 3)            #> "-0.000"
```

### `apply_table_spec` does no schema check on `params`
`R/parameter-apply.R:19-33`. A wrong data frame surfaces as a dplyr internal.

```r
apply_table_spec(data.frame(x = 1:3), TableSpec(), NULL)
#> Error in `.data$kind`: Column `kind` not found in `.data`.
```

### `set_spec_sections` silently discards the sibling class's assignment argument
`R/spec-setters.R:544,577`. `models=` on a TableSpec (or `parameters=`/`file=` on
a SummarySpec) is dropped before the "ignoring named argument" warning fires.

```r
TableSpec() |> set_spec_sections(kind == "THETA" ~ "A", models = list(Sel = "run001"))
#> returns a TableSpec with no warning about the ignored `models` argument
```

### Deprecated `set_spec_section_filter` breaks a valid 0.4.0 pattern
`R/spec-setters.R:275-296`. Multi-label positional calls now misbind to `keep`.

```r
sp <- TableSpec() |> set_spec_sections(kind == "THETA" ~ "A", kind == "OMEGA" ~ "B")
set_spec_section_filter(sp, "A", "B")
#> Error: `keep` and `exclude` are mutually exclusive; pass at most one.
```

### Duplicate model labels: gt errors, flextable silently merges
`R/render-gt.R:177` vs `R/render-flextable.R:298`.

```r
e1 <- suppressMessages(enrich("run001", TableSpec()))
e2 <- suppressMessages(enrich("run002", TableSpec()))
cmp <- suppressMessages(compare_with(e1, e2, labels = c("run001", "run001")))
make_comparison_table(cmp, output = "gt")
#> Error: The spanner `id` provided ("run001") is not unique.
make_comparison_table(cmp, output = "flextable")
#> renders OK — the two models are silently merged under one spanner
```

### flextable renders `$…$` LaTeX only in symbol columns; strips `$` elsewhere
`R/render-flextable.R:249-257`.

```r
ns$convert_md_label("$\\mu$g/L")     #> "\\mug/L"   (the '$' is deleted, mu lost)
ns$convert_md_label("$\\Delta$OFV")  #> "ΔOFV"      (only Delta/delta are handled)
```

### gt `$…$`→OMML rewrite corrupts literal dollar text
`R/render-gt.R:883-920`. The paragraph-level regex `\$[^$]+\$` captures across
prose.

```r
x <- "costs $100 per dose while B is $200 per dose"
regmatches(x, regexpr("\\$[^$]+\\$", x))
#> "$100 per dose while B is $"   -> becomes an equation; dollars & spacing lost
```

### `apply_gt_hide_cols` is dead code
`R/render-gt.R:120-132`. `apply_formatting` already drops hidden columns, so the gt
helper never has anything to hide.

```r
params <- suppressMessages(enrich("run001"))
spec   <- get_table_spec(params)
layout <- ns$prepare_parameter_table_data(params, spec)
ht     <- ns$hyperion_parameter_table(layout$params, layout, spec)
length(ht@hide_cols)                                        #> 11
length(intersect(ht@hide_cols, names(ns$apply_formatting(ht))))  #> 0
```

### `format_sigfig_pad` pads non-finite values
`R/variability.R:51-64`.

```r
ns$format_sigfig_pad(Inf, 4)    #> "Inf.0"
ns$format_sigfig_pad(-Inf, 4)   #> "-Inf.0"
ns$format_sigfig_pad(Inf, 5)    #> "Inf.00"
```
