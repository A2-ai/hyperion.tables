snapshot_gt <- function(table, name) {
  testthat::skip_if_not_installed("gt")

  html_path <- file.path(tempdir(), paste0(name, ".html"))
  html_snapshot_path <- file.path(tempdir(), paste0(name, "-snapshot.html"))
  html <- gt::as_raw_html(table)
  write_snapshot_html(html, html_snapshot_path)
  testthat::expect_snapshot_file(html_snapshot_path)

  testthat::skip_on_os("windows")
  testthat::skip_on_os("linux")

  testthat::skip_if_not_installed("webshot2")
  png_path <- file.path(tempdir(), paste0(name, ".png"))
  gt::gtsave(table, filename = html_path)

  webshot2::webshot(
    url = html_path,
    file = png_path,
    selector = "table.gt_table",
    delay = 1,
    vwidth = 4000,
    vheight = 3000,
    zoom = 1
  )

  testthat::expect_snapshot_file(png_path)
}

snapshot_flextable <- function(table, name) {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("htmltools")

  html_path <- file.path(tempdir(), paste0(name, ".html"))
  html_snapshot_path <- file.path(tempdir(), paste0(name, "-snapshot.html"))
  html_tag <- flextable::htmltools_value(table)
  htmltools::save_html(html_tag, file = html_path)
  normalize_snapshot_html_file(html_path, html_snapshot_path)
  testthat::expect_snapshot_file(html_snapshot_path)

  testthat::skip_on_os("windows")
  testthat::skip_on_os("linux")

  testthat::skip_if_not_installed("webshot2")
  png_path <- file.path(tempdir(), paste0(name, ".png"))

  webshot2::webshot(
    url = html_path,
    file = png_path,
    selector = ".flextable-shadow-host",
    delay = 1, # Allow time for KaTeX CSS to load from CDN
    vwidth = 4000,
    vheight = 3000,
    zoom = 1
  )

  testthat::expect_snapshot_file(png_path)
}

normalize_snapshot_html <- function(html) {
  html <- gsub("cl-[0-9a-f]+", "cl-XXXX", html)
  html <- gsub(
    "<div id=\"[A-Za-z0-9]{10}\"",
    "<div id=\"id-XXXX\"",
    html
  )
  html
}

write_snapshot_html <- function(html, path) {
  html <- normalize_snapshot_html(html)
  writeLines(html, path)
}

normalize_snapshot_html_file <- function(path, snapshot_path) {
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  write_snapshot_html(html, snapshot_path)
}
