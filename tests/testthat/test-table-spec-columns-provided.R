test_that("TableSpec @columns is NULL until user sets columns", {
  spec <- TableSpec()
  expect_null(spec@columns)

  spec <- set_spec_columns(spec, "symbol", "fixed")
  expect_false(is.null(spec@columns))
})

test_that("TableSpec @columns is non-NULL when columns passed to constructor", {
  spec <- TableSpec(columns = c("name", "estimate"))
  expect_false(is.null(spec@columns))
})
