
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hyperion.tables

<!-- badges: start -->

<!-- badges: end -->

Flexible tables for hyperion models.

## Installation

You can install the development version of hyperion.tables from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("A2-ai/hyperion.tables")
```

``` r
library(gt)
library(hyperion)
#> 
#> 
#> ── pharos configuration ────────────────────────────────────────────────────────
#> ✖ No pharos.toml config file found. Please call hyperion::init() to create one
#> ── hyperion options ────────────────────────────────────────────────────────────
#> ✔ hyperion.significant_number_display : 4
#> ── hyperion nonmem object options ──────────────────────────────────────────────
#> ✔ hyperion.nonmem_model.show_included_columns : FALSE
#> ✔ hyperion.nonmem_summary.rse_threshold : 50
#> ✔ hyperion.nonmem_summary.shrinkage_threshold : 30
library(hyperion.tables)
#> 
#> Attaching package: 'hyperion.tables'
#> The following object is masked from 'package:gt':
#> 
#>     render_gt
```

## Example

### Parameter table

    #> Warning in file(con, "wb"): cannot open file
    #> '/Users/mattsmith/Documents/hyperion.tables/man/figures/README-param-table.png':
    #> No such file or directory
    #> Warning in callback(...): An error occurred: Error in file(con, "wb"): cannot open the connection
    #> file:////var/folders/bx/l0m1kftd7m93lvrc4m3_309c0000gn/T//RtmpMsrrcD/file184af7ab1943c.html screenshot completed
    #> Warning in normalizePath(value): path[1]="An error occurred: Error in file(con, "wb"): cannot open the connection
    #> ": No such file or directory

![](man/figures/README-param-table.png)

### Comparison table

    #> Warning in file(con, "wb"): cannot open file
    #> '/Users/mattsmith/Documents/hyperion.tables/man/figures/README-comparison-table.png':
    #> No such file or directory
    #> Warning in callback(...): An error occurred: Error in file(con, "wb"): cannot open the connection
    #> file:////var/folders/bx/l0m1kftd7m93lvrc4m3_309c0000gn/T//RtmpMsrrcD/file184aff5e0a68.html screenshot completed
    #> Warning in normalizePath(value): path[1]="An error occurred: Error in file(con, "wb"): cannot open the connection
    #> ": No such file or directory

![](man/figures/README-comparison-table.png)

### Summary table

    #> Warning in file(con, "wb"): cannot open file
    #> '/Users/mattsmith/Documents/hyperion.tables/man/figures/README-summary-table.png':
    #> No such file or directory
    #> Warning in callback(...): An error occurred: Error in file(con, "wb"): cannot open the connection
    #> file:////var/folders/bx/l0m1kftd7m93lvrc4m3_309c0000gn/T//RtmpMsrrcD/file184af31a94729.html screenshot completed
    #> Warning in normalizePath(value): path[1]="An error occurred: Error in file(con, "wb"): cannot open the connection
    #> ": No such file or directory

![](man/figures/README-summary-table.png)

### Custom renderers

`make_parameter_table(output = "data")` returns a `HyperionTable`. If
you need custom output, call `apply_formatting()` and render the
returned data frame with your preferred table package.

``` r
htable <- params |>
  apply_table_spec(spec, info) |>
  add_summary_info(sum) |>
  make_parameter_table(output = "data")

data <- apply_formatting(htable)
# render data with your custom table tool
```
