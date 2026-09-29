test_that("gt Word exports survive cleanup when using relative paths", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("equatags")
  skip_if_not_installed("zip")
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is required")

  withr::local_tempdir(.local_envir = environment()) |>
    withr::local_dir(.local_envir = environment())
  table <- gt::gt(data.frame(Parameter = "CL", Estimate = 1.2))

  for (path in c("table.docx", "nested output/table.docx")) {
    expect_identical(render_to_word(table, path), path)
    expect_true(file.exists(path))
    entries <- utils::unzip(path, list = TRUE)$Name
    expect_true(all(c("[Content_Types].xml", "word/document.xml") %in% entries))
    contents <- tempfile("word-contents-")
    dir.create(contents)
    withr::defer(unlink(contents, recursive = TRUE))
    utils::unzip(path, files = "word/document.xml", exdir = contents)
    doc <- xml2::read_xml(file.path(contents, "word/document.xml"))
    expect_match(xml2::xml_text(doc), "CL", fixed = TRUE)
  }
})
