# Regression / structural tests for the Word (.docx) export path (F7, F8).
# These run wherever gt/flextable/pandoc are available (unlike the PNG snapshots,
# which need webshot2 + Chrome and are skipped on CI/Linux).

we_model_dir <- function() {
  system.file("extdata", "models", "onecmt", package = "hyperion.tables")
}

we_param_table <- function(spec, output) {
  mod <- hyperion::read_model(file.path(we_model_dir(), "run001.mod"))
  hyperion::get_parameters(mod) |>
    apply_table_spec(spec, hyperion::get_model_parameter_info(mod)) |>
    add_summary_info(summary(mod)) |>
    make_parameter_table(output = output)
}

docx_font_sizes_pt <- function(docx) {
  ex <- withr::local_tempdir()
  utils::unzip(docx, exdir = ex)
  xml <- paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE), collapse = "")
  matches <- regmatches(xml, gregexpr('w:sz w:val="[0-9]+"', xml))[[1]]
  as.integer(sub('.*"([0-9]+)".*', "\\1", matches)) / 2
}

test_that("F7: sectioned flextable Word export stays at a readable font size", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  local_fixture_dir()

  spec <- TableSpec(display_transforms = list(omega = "cv"), n_sigfig = 3) |>
    set_spec_sections(
      kind == "THETA" ~ "Structural model parameters",
      kind == "OMEGA" & diagonal ~ "Interindividual variance parameters",
      kind == "SIGMA" ~ "Residual error"
    )
  ft <- we_param_table(spec, "flextable")

  out <- withr::local_tempfile(fileext = ".docx")
  suppressWarnings(render_to_word(ft, out))
  expect_true(file.exists(out))

  sizes <- docx_font_sizes_pt(out)
  # Before the fix, sectioned tables collapsed to 1-2 pt.
  expect_gte(min(sizes), 6)
})

test_that("F8: gt Word export is structurally sound", {
  skip_if_not_installed("gt")
  skip_if_not_installed("xml2")
  skip_if_not_installed("equatags")
  skip_if_not_installed("zip")
  skip_if_not(nzchar(Sys.which("pandoc")) || rmarkdown::pandoc_available(), "pandoc not available")
  local_fixture_dir()

  gt_tbl <- we_param_table(TableSpec(display_transforms = list(omega = "cv")), "gt")
  out <- withr::local_tempfile(fileext = ".docx")
  render_to_word(gt_tbl, out)
  expect_true(file.exists(out))

  ex <- withr::local_tempdir()
  utils::unzip(out, exdir = ex)
  doc_path <- file.path(ex, "word", "document.xml")
  expect_true(file.exists(doc_path))

  # Parses as XML (would error on malformed output)
  doc <- xml2::read_xml(doc_path)
  raw <- paste(readLines(doc_path, warn = FALSE), collapse = "")

  # A table grid must be present (Word repairs the file otherwise)
  expect_match(raw, "tblGrid")
  # No leftover SEQ-Table field ("contains fields" prompt)
  expect_false(grepl("fldChar", raw))
  # Exactly one w namespace binding on the document element
  expect_equal(lengths(regmatches(raw, gregexpr("xmlns:w=", raw)))[1], 1L)
})
