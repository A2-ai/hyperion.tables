# ==============================================================================
# GT Rendering for HyperionTable
# ==============================================================================

#' Render HyperionTable as gt table
#'
#' Converts a HyperionTable intermediate representation to a gt table object.
#'
#' @param table A HyperionTable object
#' @return A gt table object
#' @export
render_to_gt <- function(table) {
  if (!S7::S7_inherits(table, HyperionTable)) {
    rlang::abort("table must be a HyperionTable object")
  }
  check_suggested(
    "katex",
    reason = "to render LaTeX symbols in gt tables.",
    severity = "warn"
  )
  data <- apply_formatting(table)
  visible_cols <- names(data)
  groupname_col <- normalize_groupname_col(table@groupname_col, names(data))

  # Create base gt table
  gt_table <- gt::gt(data, groupname_col = groupname_col)

  # Hide columns that remain in the prepared data
  gt_table <- apply_gt_hide_cols(gt_table, table, names(data))

  # Apply column labels
  gt_table <- apply_gt_labels(gt_table, table, visible_cols)

  # Apply spanners
  gt_table <- apply_gt_spanners(gt_table, table, visible_cols)

  # Format markdown
  gt_table <- gt_table |>
    gt::fmt_markdown()

  # Add title
  gt_table <- apply_gt_title(gt_table, table)

  # Apply bold styling
  gt_table <- apply_gt_bold(gt_table, table)

  # Apply borders
  gt_table <- apply_gt_borders(gt_table, table, visible_cols)

  # Right-align numeric display columns (including merged CI)
  numeric_cols <- intersect(table@numeric_cols, names(data))
  if (length(numeric_cols) > 0) {
    gt_table <- gt::cols_align(
      gt_table,
      align = "right",
      columns = dplyr::all_of(numeric_cols)
    )
  }

  # Add footnotes
  gt_table <- apply_gt_footnotes(gt_table, table)

  # Add nowrap CSS
  gt_table <- gt_table |>
    gt::opt_css(css = "td, th { white-space: nowrap; }")

  gt_table
}

#' Hide columns in gt table
#' @noRd
apply_gt_hide_cols <- function(gt_table, table, data_cols) {
  if (length(table@hide_cols) == 0) {
    return(gt_table)
  }

  hide_cols <- intersect(table@hide_cols, data_cols)
  if (length(hide_cols) == 0) {
    return(gt_table)
  }

  gt_table |>
    gt::cols_hide(dplyr::all_of(hide_cols))
}

#' Apply column labels to gt table
#' @noRd
apply_gt_labels <- function(gt_table, table, visible_cols) {
  if (length(table@col_labels) == 0) {
    return(gt_table)
  }

  labels_to_apply <- table@col_labels[
    intersect(names(table@col_labels), visible_cols)
  ]

  if (length(labels_to_apply) == 0) {
    return(gt_table)
  }

  gt_table |>
    gt::cols_label(!!!labels_to_apply)
}

#' Format numeric columns in gt table
#' @noRd
apply_gt_numeric_format <- function(gt_table, table) {
  numeric_cols <- intersect(table@numeric_cols, names(table@data))

  if (length(numeric_cols) == 0) {
    return(gt_table)
  }

  gt_table |>
    gt::fmt_number(
      columns = dplyr::any_of(numeric_cols),
      n_sigfig = table@n_sigfig
    )
}

#' Add title to gt table
#' @noRd
apply_gt_title <- function(gt_table, table) {
  if (!is_scalar_nonempty_char(table@title)) {
    return(gt_table)
  }

  gt_table |>
    gt::tab_header(title = table@title)
}

#' Apply spanners to gt table
#' @noRd
apply_gt_spanners <- function(gt_table, table, visible_cols) {
  for (spanner in table@spanners) {
    cols <- intersect(spanner$columns, visible_cols)
    if (length(cols) > 0) {
      gt_table <- gt_table |>
        gt::tab_spanner(label = spanner$label, columns = dplyr::all_of(cols))
    }
  }
  gt_table
}

#' Apply CI missing text to gt table
#' @noRd
apply_gt_ci_missing <- function(gt_table, table) {
  if (length(table@ci_missing_rows) == 0) {
    return(gt_table)
  }

  # Find CI columns
  ci_cols <- character(0)
  for (merge in table@ci_merges) {
    ci_cols <- c(ci_cols, merge$ci_low, merge$ci_high)
  }
  ci_cols <- intersect(ci_cols, names(table@data))

  if (length(ci_cols) > 0) {
    gt_table <- gt_table |>
      gt::sub_missing(
        columns = dplyr::all_of(ci_cols),
        rows = table@ci_missing_rows,
        missing_text = table@ci@missing_text
      )
  }

  gt_table
}

#' Apply bold styling to gt table
#' @noRd
apply_gt_bold <- function(gt_table, table) {
  locations <- list()

  if ("column_labels" %in% table@bold_locations) {
    locations <- c(
      locations,
      list(gt::cells_column_labels(dplyr::everything()))
    )
  }
  if ("title" %in% table@bold_locations) {
    locations <- c(locations, list(gt::cells_title(groups = "title")))
  }
  if ("row_groups" %in% table@bold_locations) {
    locations <- c(locations, list(gt::cells_row_groups()))
  }
  if ("spanners" %in% table@bold_locations) {
    locations <- c(
      locations,
      list(gt::cells_column_spanners(dplyr::everything()))
    )
  }

  if (length(locations) > 0) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = locations
      )
  }

  gt_table
}

#' Apply borders to gt table
#' @noRd
apply_gt_borders <- function(gt_table, table, visible_cols) {
  for (border in table@borders) {
    cols <- intersect(border$columns, visible_cols)
    if (length(cols) > 0) {
      gt_table <- gt_table |>
        gt::tab_style(
          style = gt::cell_borders(sides = border$sides, color = border$color),
          locations = gt::cells_body(columns = dplyr::all_of(cols))
        )
    }
  }
  gt_table
}

#' Apply footnotes to gt table
#' @noRd
apply_gt_footnotes <- function(gt_table, table) {
  for (fn in table@footnotes) {
    content <- if (fn$is_markdown) gt::md(fn$content) else fn$content
    gt_table <- gt_table |>
      gt::tab_footnote(content)
  }
  gt_table
}

#' @export
render_to_image.gt_tbl <- function(table, path = NULL) {
  check_suggested("webshot2", reason = "for image output.")
  check_suggested(
    "katex",
    reason = "to render LaTeX symbols in gt tables.",
    severity = "warn"
  )

  # Intermediate HTML is always temp.
  html_path <- tempfile("hyperion-table-", fileext = ".html")
  on.exit(unlink(html_path), add = TRUE)

  # PNG for display (knitr-relative or temp).
  if (isTRUE(getOption("knitr.in.progress"))) {
    png_path <- knitr::fig_path(suffix = ".png")
    dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
  } else {
    png_path <- tempfile("hyperion-table-", fileext = ".png")
  }

  # Temporarily hide Quarto env so gt uses katex (not data-qmd-base64)
  old_quarto <- Sys.getenv("QUARTO_BIN_PATH", unset = NA)
  Sys.unsetenv("QUARTO_BIN_PATH")
  on.exit(
    {
      if (!is.na(old_quarto)) Sys.setenv(QUARTO_BIN_PATH = old_quarto)
    },
    add = TRUE
  )

  table |>
    gt::gtsave(filename = html_path)

  webshot2::webshot(
    url = html_path,
    file = png_path,
    selector = "table.gt_table",
    vwidth = 4000,
    vheight = 3000,
    zoom = 1,
    delay = 1,
    quiet = TRUE
  )

  if (!file.exists(png_path)) {
    rlang::abort(
      paste0("Failed to create PNG output at: ", png_path)
    )
  }

  if (!is.null(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    copied <- file.copy(png_path, path, overwrite = TRUE)
    if (!isTRUE(copied)) {
      rlang::abort(
        paste0("Failed to copy PNG output to: ", path)
      )
    }
  }

  knitr::include_graphics(png_path)
}

#' @export
render_to_word.gt_tbl <- function(table, path) {
  if (!grepl("\\.docx$", path, ignore.case = TRUE)) {
    rlang::abort("`path` must end in `.docx`.")
  }
  rlang::check_installed(c("xml2", "equatags", "zip"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  gt::gtsave(table, path)
  rewrite_latex_math_in_docx(path)
  invisible(path)
}

#' @noRd
rewrite_latex_math_in_docx <- function(path) {
  stage <- tempfile("gt-docx-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = stage)

  doc_path <- file.path(stage, "word", "document.xml")
  doc <- xml2::read_xml(doc_path)
  ns <- c(
    w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    m = "http://schemas.openxmlformats.org/officeDocument/2006/math"
  )

  # Ensure the math namespace is declared on <w:document>
  root <- xml2::xml_root(doc)
  if (is.na(xml2::xml_attr(root, "xmlns:m"))) {
    xml2::xml_set_attr(root, "xmlns:m", ns[["m"]])
  }

  t_nodes <- xml2::xml_find_all(doc, ".//w:t", ns = ns)
  for (t_node in t_nodes) {
    txt <- xml2::xml_text(t_node)
    if (!grepl("\\$[^$]+\\$", txt)) {
      next
    }

    run <- xml2::xml_parent(t_node)
    if (xml2::xml_name(run) != "r") {
      next
    }
    rpr <- xml2::xml_find_first(run, "./w:rPr", ns = ns)
    rpr_xml <- if (!inherits(rpr, "xml_missing")) as.character(rpr) else ""

    parts <- split_on_dollar_math(txt)
    frag <- vapply(parts, render_latex_part, character(1), rpr_xml = rpr_xml)

    wrapper <- xml2::read_xml(paste0(
      "<root xmlns:w=\"",
      ns[["w"]],
      "\" xmlns:m=\"",
      ns[["m"]],
      "\">",
      paste(frag, collapse = ""),
      "</root>"
    ))
    for (child in xml2::xml_children(wrapper)) {
      xml2::xml_add_sibling(run, child, .where = "before")
    }
    xml2::xml_remove(run)
  }

  xml2::write_xml(doc, doc_path)

  files <- list.files(stage, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  unlink(path)
  old <- setwd(stage)
  on.exit(setwd(old), add = TRUE)
  zip::zipr(path, files = files)
}

#' @noRd
split_on_dollar_math <- function(txt) {
  m <- gregexpr("\\$[^$]+\\$", txt, perl = TRUE)[[1]]
  if (m[1] == -1) {
    return(list(list(type = "text", value = txt)))
  }
  starts <- as.integer(m)
  ends <- starts + attr(m, "match.length") - 1L
  out <- list()
  cur <- 1L
  for (i in seq_along(starts)) {
    if (starts[i] > cur) {
      out[[length(out) + 1L]] <- list(
        type = "text",
        value = substr(txt, cur, starts[i] - 1L)
      )
    }
    out[[length(out) + 1L]] <- list(
      type = "eq",
      value = substr(txt, starts[i] + 1L, ends[i] - 1L)
    )
    cur <- ends[i] + 1L
  }
  if (cur <= nchar(txt)) {
    out[[length(out) + 1L]] <- list(
      type = "text",
      value = substr(txt, cur, nchar(txt))
    )
  }
  out
}

#' @noRd
render_latex_part <- function(part, rpr_xml) {
  if (part$type == "text") {
    if (!nzchar(part$value)) {
      return("")
    }
    paste0(
      "<w:r>",
      rpr_xml,
      "<w:t xml:space=\"preserve\">",
      xml_escape(part$value),
      "</w:t>",
      "</w:r>"
    )
  } else {
    mml <- tryCatch(
      equatags::transform_mathjax(part$value, to = "mml"),
      error = function(e) NA_character_
    )
    if (is.na(mml) || !nzchar(mml)) {
      paste0(
        "<w:r>",
        rpr_xml,
        "<w:t xml:space=\"preserve\">$",
        xml_escape(part$value),
        "$</w:t>",
        "</w:r>"
      )
    } else {
      mml
    }
  }
}

#' @noRd
xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
