test_that("TableSpec columns_provided tracks explicit columns", {
  spec <- TableSpec()
  expect_false(isTRUE(spec@.columns_provided))

  spec <- set_spec_columns(spec, "symbol", "fixed")
  expect_true(isTRUE(spec@.columns_provided))
})

test_that("TableSpec columns_provided is TRUE when columns passed to constructor", {
  spec <- TableSpec(columns = c("name", "estimate"))
  expect_true(isTRUE(spec@.columns_provided))
})
