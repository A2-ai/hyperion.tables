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

  # Comparison tables: left border between models on body rows only (skip
  # column labels so the border doesn't bisect column names). Stash the
  # per-model column segment lengths so the docx post-processor can apply
  # the same border. Segment math excludes the groupname_col (consumed for
  # row grouping, not rendered as a column).
  rendered_cols <- setdiff(visible_cols, groupname_col)
  segments <- comparison_column_segments(table, rendered_cols)
  if (!is.null(segments)) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_borders(
          sides = "left",
          color = "black",
          weight = gt::px(1)
        ),
        locations = gt::cells_body(
          columns = dplyr::all_of(segments$boundary_cols)
        )
      )
    attr(gt_table, "hyperion_segment_lengths") <- segments$segment_lengths
  }

  # Add nowrap CSS
  gt_table <- gt_table |>
    gt::opt_css(css = "td, th { white-space: nowrap; }")

  gt_table
}

#' Compute per-model column segments for comparison tables.
#' Returns NULL for non-comparison tables or ones with <2 model spanners.
#' @noRd
comparison_column_segments <- function(table, visible_cols) {
  if (table@table_type != "comparison" || length(table@spanners) < 2) {
    return(NULL)
  }
  boundary_cols <- vapply(
    table@spanners[-1],
    function(s) s$columns[1],
    character(1)
  )
  boundary_cols <- intersect(boundary_cols, visible_cols)
  if (length(boundary_cols) == 0) {
    return(NULL)
  }
  boundary_idx <- sort(match(boundary_cols, visible_cols))
  n_total <- length(visible_cols)
  starts <- c(1L, boundary_idx)
  ends <- c(boundary_idx - 1L, n_total)
  list(
    boundary_cols = boundary_cols,
    segment_lengths = ends - starts + 1L
  )
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

  # Wrap markdown-flagged labels (raw strings containing LaTeX `$` delimiters)
  # via `gt::md()` so gt renders them as markdown.
  labels_to_apply <- lapply(labels_to_apply, function(x) {
    if (is.character(x) && length(x) == 1 && grepl("\\$", x)) gt::md(x) else x
  })

  gt_table |>
    gt::cols_label(!!!labels_to_apply)
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

#' Add footnotes to a gt table in specified order
#'
#' Coordinator function that applies footnotes from builders in the order
#' specified by spec@footnote_order.
#'
#' @param table A gt table object
#' @param spec TableSpec or SummarySpec object (can be NULL)
#' @param summary_note Character string for summary info, or NULL
#' @param equations List of footnote content for equations, or NULL
#' @param abbreviations Character vector for abbreviations, or NULL
#' @return gt table with footnotes added
#' @noRd
add_footnotes <- function(
  table,
  spec,
  summary_note,
  equations,
  abbreviations
) {
  # Get footnote order from spec - return early if NULL (disabled)
  footnote_order <- if (
    !is.null(spec) && "footnote_order" %in% names(S7::props(spec))
  ) {
    spec@footnote_order
  }
  if (is.null(footnote_order)) {
    return(table)
  }

  footnotes <- list(
    summary_info = summary_note,
    equations = equations,
    abbreviations = abbreviations
  )

  for (section in footnote_order) {
    content <- footnotes[[section]]
    if (!is.null(content)) {
      for (line in content) {
        # equations are markdown strings; other sections are plain text
        line <- if (section == "equations") gt::md(line) else line
        table <- table |> gt::tab_footnote(line)
      }
    }
  }

  table
}

#' Add conditional footnotes based on table contents
#'
#' @param table A gt table object
#' @param params Parameter data frame (or comparison data frame or summary data frame)
#' @param spec TableSpec or SummarySpec object
#' @param comparison_stats Optional list with has_ofv and has_lrt for comparison tables
#' @param summary_stats Optional list with has_ofv, has_dofv, has_cond_num for summary tables
#' @param summary_note Optional character string for summary info footnote
#' @return gt table with appropriate footnotes added
#' @noRd
add_conditional_footnotes <- function(
  table,
  params,
  spec,
  comparison_stats = NULL,
  summary_stats = NULL,
  summary_note = NULL
) {
  stats <- detect_table_statistics(params, spec)

  ci_pct <- if (!is.null(spec) && "ci" %in% names(S7::props(spec))) {
    round(spec@ci@level * 100)
  } else {
    95
  }

  # Build footnote content using builder functions
  abbreviations <- build_abbreviations_footnote(
    stats,
    comparison_stats,
    summary_stats
  )
  equations <- build_equations_footnote(
    stats,
    ci_pct,
    comparison_stats,
    summary_stats
  )

  # Add footnotes in specified order
  add_footnotes(table, spec, summary_note, equations, abbreviations)
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
render_to_word.gt_tbl <- function(table, path, landscape = FALSE) {
  if (!grepl("\\.docx$", path, ignore.case = TRUE)) {
    rlang::abort("`path` must end in `.docx`.")
  }
  rlang::check_installed(c("xml2", "equatags", "zip"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  segment_lengths <- attr(table, "hyperion_segment_lengths")
  gt::gtsave(table, path)
  sanitize_gt_docx(
    path,
    segment_lengths = segment_lengths,
    landscape = landscape
  )
  invisible(path)
}

#' Post-process a gt-generated `.docx` so Word opens it cleanly.
#'
#' Each helper addresses a specific problem: schema gaps pandoc leaves behind
#' (missing tblGrid, empty cells, out-of-order children), a pandoc emission
#' bug (column-label bold), the "contains fields" SEQ-Table warning,
#' aesthetic decisions (cell borders), and our LaTeX → OMML rewrite.
#' Ordering notes: grids are injected before the reorder pass; math is
#' rewritten last so it sees the final structure; xmlns dedup runs on the
#' serialized bytes since xml2 can't remove a binding without breaking it.
#' @noRd
sanitize_gt_docx <- function(path, segment_lengths = NULL, landscape = FALSE) {
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

  fix_caption_style(doc, ns)
  strip_tc_borders(doc, ns)
  add_vertical_borders(doc, ns, segment_lengths)
  strip_seq_table_field(doc, ns)
  bold_header_rows(doc, ns)
  fill_empty_cells(doc, ns)
  inject_table_grids(doc, ns)
  reorder_ooxml_sequences(doc, ns)
  rewrite_latex_to_omml(doc, ns)
  if (isTRUE(landscape)) {
    set_landscape_orientation(doc, ns)
  }

  xml2::write_xml(doc, doc_path)
  dedupe_xmlns_w(doc_path, ns)
  set_word_compat_mode(stage, ns)

  unlink(path)
  zip_dir_contents(stage, path)
}

# --- sanitize_gt_docx helpers --------------------------------------------

# Word opens the document in "Compatibility Mode" unless word/settings.xml
# declares compatibilityMode=15 in a <w:compat> block, AND the <w:settings>
# root declares the modern Microsoft namespaces with mc:Ignorable so they
# can be referenced safely.
#' @noRd
set_word_compat_mode <- function(stage, ns) {
  settings_path <- file.path(stage, "word", "settings.xml")
  if (!file.exists(settings_path)) {
    return(invisible())
  }
  settings <- xml2::read_xml(settings_path)
  root <- xml2::xml_root(settings)
  modern_ns <- c(
    mc = "http://schemas.openxmlformats.org/markup-compatibility/2006",
    w14 = "http://schemas.microsoft.com/office/word/2010/wordml",
    w15 = "http://schemas.microsoft.com/office/word/2012/wordml",
    w16se = "http://schemas.microsoft.com/office/word/2015/wordml/symex",
    w16cid = "http://schemas.microsoft.com/office/word/2016/wordml/cid",
    w16 = "http://schemas.microsoft.com/office/word/2018/wordml",
    w16cex = "http://schemas.microsoft.com/office/word/2018/wordml/cex"
  )
  for (prefix in names(modern_ns)) {
    attr_name <- paste0("xmlns:", prefix)
    if (is.na(xml2::xml_attr(root, attr_name))) {
      xml2::xml_set_attr(root, attr_name, modern_ns[[prefix]])
    }
  }
  xml2::xml_set_attr(
    root,
    "mc:Ignorable",
    "w14 w15 w16se w16cid w16 w16cex"
  )
  if (length(xml2::xml_find_all(settings, ".//w:compat", ns = ns)) == 0) {
    compat_xml <- paste0(
      "<w:compat xmlns:w=\"",
      ns[["w"]],
      "\">",
      "<w:compatSetting w:name=\"compatibilityMode\"",
      " w:uri=\"http://schemas.microsoft.com/office/word\" w:val=\"15\"/>",
      "</w:compat>"
    )
    xml2::xml_add_child(root, xml2::xml_root(xml2::read_xml(compat_xml)))
  }
  xml2::write_xml(settings, settings_path)
  dedupe_xmlns_w(settings_path, ns)
}

# Set the body section to landscape US Letter. gt's docx ships a minimal
# <w:sectPr> with no <w:pgSz>; we inject one. Page dimensions are in twips
# (1/20 of a point, 1440 per inch): US Letter is 8.5 × 11 inches, so
# landscape swaps to 11 × 8.5 → 15840 × 12240 twips.
#' @noRd
set_landscape_orientation <- function(doc, ns) {
  inch <- 1440L
  width <- 11L * inch
  height <- as.integer(8.5 * inch)
  sect <- xml2::xml_find_first(doc, ".//w:body/w:sectPr", ns = ns)
  if (inherits(sect, "xml_missing")) {
    return(invisible())
  }
  pg_size <- xml2::xml_find_first(sect, "./w:pgSz", ns = ns)
  if (inherits(pg_size, "xml_missing")) {
    pg_xml <- sprintf(
      paste0(
        "<w:pgSz xmlns:w=\"%s\" w:w=\"%d\" w:h=\"%d\"",
        " w:orient=\"landscape\"/>"
      ),
      ns[["w"]],
      width,
      height
    )
    xml2::xml_add_child(
      sect,
      xml2::xml_root(xml2::read_xml(pg_xml)),
      .where = 0
    )
  } else {
    xml2::xml_set_attr(pg_size, "w:w", as.character(width))
    xml2::xml_set_attr(pg_size, "w:h", as.character(height))
    xml2::xml_set_attr(pg_size, "w:orient", "landscape")
  }
}

# gt emits <w:pStyle w:val="caption"/> but the styles template defines the
# style as "Caption". Style IDs are case-sensitive, so Word rejects the
# reference and prompts "unreadable content".
#' @noRd
fix_caption_style <- function(doc, ns) {
  for (node in xml2::xml_find_all(
    doc,
    ".//w:pStyle[@w:val='caption']",
    ns = ns
  )) {
    xml2::xml_set_attr(node, "w:val", "Caption")
  }
}

# Strip cell borders. gt emits them but in complex tables they look noisy;
# users can add borders in Word after opening if desired. Vertical borders
# between model groups for comparison tables are added back later via
# `add_vertical_borders()`, which runs after this.
#' @noRd
strip_tc_borders <- function(doc, ns) {
  for (tcb in xml2::xml_find_all(doc, ".//w:tcBorders", ns = ns)) {
    xml2::xml_remove(tcb)
  }
}

# For comparison tables, add a left border on each <w:tc> whose starting
# column (computed by walking gridSpans) matches a model-group boundary.
# Runs after strip_tc_borders so these borders survive. Full-span rows
# (group headers from groupname_col, footnotes) are naturally skipped —
# their single cell starts at column 1, never at a boundary. The
# column-label row (marked with <w:tblHeader/>) is explicitly skipped so
# the border doesn't bisect column names.
#' @noRd
add_vertical_borders <- function(doc, ns, segment_lengths) {
  if (is.null(segment_lengths) || length(segment_lengths) < 2) {
    return(invisible())
  }
  boundary_positions <- cumsum(segment_lengths)[
    -length(segment_lengths)
  ] +
    1L
  for (tbl in xml2::xml_find_all(doc, ".//w:tbl", ns = ns)) {
    for (row in xml2::xml_find_all(tbl, "./w:tr", ns = ns)) {
      if (length(xml2::xml_find_all(row, "./w:trPr/w:tblHeader", ns = ns))) {
        next
      }
      col <- 1L
      for (tc in xml2::xml_find_all(row, "./w:tc", ns = ns)) {
        if (col %in% boundary_positions) {
          add_left_border(tc, ns)
        }
        span <- xml2::xml_find_first(tc, "./w:tcPr/w:gridSpan", ns = ns)
        width <- if (inherits(span, "xml_missing")) {
          1L
        } else {
          as.integer(xml2::xml_attr(span, "val"))
        }
        col <- col + width
      }
    }
  }
}

# Ensure <w:tc> has <w:tcPr><w:tcBorders><w:left .../></w:tcBorders></w:tcPr>.
# Does not touch other border sides.
#' @noRd
add_left_border <- function(tc, ns) {
  tcpr <- xml2::xml_find_first(tc, "./w:tcPr", ns = ns)
  if (inherits(tcpr, "xml_missing")) {
    tcpr_xml <- paste0("<w:tcPr xmlns:w=\"", ns[["w"]], "\"/>")
    xml2::xml_add_child(
      tc,
      xml2::xml_root(xml2::read_xml(tcpr_xml)),
      .where = 0
    )
    tcpr <- xml2::xml_find_first(tc, "./w:tcPr", ns = ns)
  }
  tcb <- xml2::xml_find_first(tcpr, "./w:tcBorders", ns = ns)
  if (inherits(tcb, "xml_missing")) {
    tcb_xml <- paste0("<w:tcBorders xmlns:w=\"", ns[["w"]], "\"/>")
    xml2::xml_add_child(tcpr, xml2::xml_root(xml2::read_xml(tcb_xml)))
    tcb <- xml2::xml_find_first(tcpr, "./w:tcBorders", ns = ns)
  }
  if (length(xml2::xml_find_all(tcb, "./w:left", ns = ns)) == 0) {
    left_xml <- paste0(
      "<w:left xmlns:w=\"",
      ns[["w"]],
      "\"",
      " w:val=\"single\" w:sz=\"8\" w:space=\"0\" w:color=\"000000\"/>"
    )
    xml2::xml_add_child(tcb, xml2::xml_root(xml2::read_xml(left_xml)))
  }
}


# Pandoc's docx backend emits column-label bold styling correctly for row
# groups (`<w:b w:val="true"/>`) but not for column labels — every run in the
# header row ships without <w:b>. Add it ourselves to every run inside any
# <w:tr> that carries <w:tblHeader/>.
#' @noRd
bold_header_rows <- function(doc, ns) {
  rows <- xml2::xml_find_all(
    doc,
    ".//w:tr[w:trPr/w:tblHeader]",
    ns = ns
  )
  for (run in xml2::xml_find_all(rows, ".//w:r", ns = ns)) {
    rpr <- xml2::xml_find_first(run, "./w:rPr", ns = ns)
    if (inherits(rpr, "xml_missing")) {
      rpr_xml <- paste0("<w:rPr xmlns:w=\"", ns[["w"]], "\"><w:b/></w:rPr>")
      xml2::xml_add_child(
        run,
        xml2::xml_root(xml2::read_xml(rpr_xml)),
        .where = 0
      )
    } else if (length(xml2::xml_find_all(rpr, "./w:b", ns = ns)) == 0) {
      b_xml <- paste0("<w:b xmlns:w=\"", ns[["w"]], "\"/>")
      xml2::xml_add_child(rpr, xml2::xml_root(xml2::read_xml(b_xml)))
    }
  }
}

# gt wraps the table caption number in a `SEQ Table \* ARABIC` field, which
# makes Word prompt "contains fields that may refer to other files". Strip
# the fldChar/instrText runs; the literal number run (between "separate" and
# "end") is left in place as plain text.
#' @noRd
strip_seq_table_field <- function(doc, ns) {
  for (fld_begin in xml2::xml_find_all(
    doc,
    ".//w:r[w:fldChar[@w:fldCharType='begin']]",
    ns = ns
  )) {
    para <- xml2::xml_parent(fld_begin)
    runs <- xml2::xml_children(para)
    begin_idx <- which(vapply(runs, identical, logical(1), fld_begin))
    end_idx <- NA_integer_
    if (begin_idx < length(runs)) {
      for (i in (begin_idx + 1):length(runs)) {
        if (
          length(xml2::xml_find_all(
            runs[[i]],
            "./w:fldChar[@w:fldCharType='end']",
            ns = ns
          ))
        ) {
          end_idx <- i
          break
        }
      }
    }
    if (is.na(end_idx)) {
      next
    }
    for (i in seq(begin_idx, end_idx)) {
      r <- runs[[i]]
      if (length(xml2::xml_find_all(r, "./w:instrText|./w:fldChar", ns = ns))) {
        xml2::xml_remove(r)
      }
    }
  }
}

# OOXML requires every <w:tc> to contain at least one <w:p>. gt's
# fmt_markdown() strips paragraphs from cells whose content collapses, leaving
# empty cells Word flags as "Table Properties N" repairs. Insert a paragraph
# matching gt's own empty-cell shape (pPr + styled empty run).
#' @noRd
fill_empty_cells <- function(doc, ns) {
  empty_p_xml <- paste0(
    "<w:p xmlns:w=\"",
    ns[["w"]],
    "\">",
    "<w:pPr><w:spacing w:after=\"60\"/><w:keepNext/></w:pPr>",
    "<w:r><w:rPr>",
    "<w:rFonts w:ascii=\"Calibri\" w:hAnsi=\"Calibri\"/>",
    "<w:sz w:val=\"20\"/>",
    "</w:rPr></w:r>",
    "</w:p>"
  )
  for (tc in xml2::xml_find_all(doc, ".//w:tc", ns = ns)) {
    if (length(xml2::xml_find_all(tc, "./w:p", ns = ns)) == 0) {
      xml2::xml_add_child(tc, xml2::xml_root(xml2::read_xml(empty_p_xml)))
    }
  }
}

# gt omits the required <w:tblGrid> element from each <w:tbl>. ECMA-376 marks
# it mandatory; Word logs one "Table Properties" repair per missing grid.
# Insert a grid with one <w:gridCol/> per column in the first row.
#' @noRd
inject_table_grids <- function(doc, ns) {
  for (tbl in xml2::xml_find_all(doc, ".//w:tbl", ns = ns)) {
    if (length(xml2::xml_find_all(tbl, "./w:tblGrid", ns = ns)) > 0) {
      next
    }
    first_tr <- xml2::xml_find_first(tbl, "./w:tr", ns = ns)
    if (inherits(first_tr, "xml_missing")) {
      next
    }
    tcs <- xml2::xml_find_all(first_tr, "./w:tc", ns = ns)
    n_cols <- sum(vapply(
      tcs,
      function(tc) {
        span <- xml2::xml_find_first(tc, "./w:tcPr/w:gridSpan", ns = ns)
        if (inherits(span, "xml_missing")) {
          1L
        } else {
          as.integer(xml2::xml_attr(span, "val"))
        }
      },
      integer(1)
    ))
    if (n_cols == 0) {
      next
    }
    grid_xml <- paste0(
      "<w:tblGrid xmlns:w=\"",
      ns[["w"]],
      "\">",
      strrep("<w:gridCol/>", n_cols),
      "</w:tblGrid>"
    )
    grid_node <- xml2::xml_root(xml2::read_xml(grid_xml))
    tblpr <- xml2::xml_find_first(tbl, "./w:tblPr", ns = ns)
    if (inherits(tblpr, "xml_missing")) {
      xml2::xml_add_child(tbl, grid_node, .where = 0)
    } else {
      xml2::xml_add_sibling(tblpr, grid_node, .where = "after")
    }
  }
}

# gt emits <w:tblPr> and <w:pPr> children in orders Word considers invalid.
# Reorder per the ECMA-376 XSD sequence for CT_TblPrBase and CT_PPrBase.
#' @noRd
reorder_ooxml_sequences <- function(doc, ns) {
  reorder_children <- function(xpath, order_vec) {
    for (parent in xml2::xml_find_all(doc, xpath, ns = ns)) {
      kids <- xml2::xml_children(parent)
      if (length(kids) < 2) {
        next
      }
      idx <- match(
        xml2::xml_name(kids),
        order_vec,
        nomatch = length(order_vec) + 1L
      )
      new_order <- order(idx)
      if (identical(new_order, seq_along(kids))) {
        next
      }
      for (k in kids) {
        xml2::xml_remove(k)
      }
      for (k in kids[new_order]) {
        xml2::xml_add_child(parent, k)
      }
    }
  }
  reorder_children(
    ".//w:tblPr",
    c(
      "tblStyle",
      "tblpPr",
      "tblOverlap",
      "bidiVisual",
      "tblStyleRowBandSize",
      "tblStyleColBandSize",
      "tblW",
      "jc",
      "tblCellSpacing",
      "tblInd",
      "tblBorders",
      "shd",
      "tblLayout",
      "tblCellMar",
      "tblLook",
      "tblCaption",
      "tblDescription",
      "tblPrChange"
    )
  )
  reorder_children(
    ".//w:pPr",
    c(
      "pStyle",
      "keepNext",
      "keepLines",
      "pageBreakBefore",
      "framePr",
      "widowControl",
      "numPr",
      "suppressLineNumbers",
      "pBdr",
      "shd",
      "tabs",
      "suppressAutoHyphens",
      "kinsoku",
      "wordWrap",
      "overflowPunct",
      "topLinePunct",
      "autoSpaceDE",
      "autoSpaceDN",
      "bidi",
      "adjustRightInd",
      "snapToGrid",
      "spacing",
      "ind",
      "contextualSpacing",
      "mirrorIndents",
      "suppressOverlap",
      "jc",
      "textDirection",
      "textAlignment",
      "textboxTightWrap",
      "outlineLvl",
      "divId",
      "cnfStyle",
      "rPr"
    )
  )
}

# Rewrite every `$…$` inline-math span in a paragraph as a sibling
# <m:oMath> element. Operates at paragraph level because pandoc's markdown
# parser can split `$…$` across multiple <w:r> runs when the LaTeX contains
# characters that markdown interprets as emphasis (paired `_` or `*`). When
# that happens, no single <w:t> contains the full span and a per-<w:t>
# approach misses it. We concatenate all <w:t> text in the paragraph, detect
# spans there, and rebuild the paragraph's runs. The first run's <w:rPr>
# is applied to the reconstructed text — any mid-paragraph pandoc-inferred
# formatting is lost, which is intentional: the intent was math, not
# emphasis. Runs last so it sees the final structure.
#' @noRd
rewrite_latex_to_omml <- function(doc, ns) {
  for (p in xml2::xml_find_all(doc, ".//w:p", ns = ns)) {
    runs <- xml2::xml_find_all(p, "./w:r", ns = ns)
    if (length(runs) == 0) {
      next
    }
    full_text <- paste(
      vapply(runs, run_text_with_markdown, character(1), ns = ns),
      collapse = ""
    )
    if (!grepl("\\$[^$]+\\$", full_text)) {
      next
    }
    first_rpr <- xml2::xml_find_first(runs[[1]], "./w:rPr", ns = ns)
    rpr_xml <- if (!inherits(first_rpr, "xml_missing")) {
      as.character(first_rpr)
    } else {
      ""
    }
    parts <- split_on_dollar_math(full_text)
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
      xml2::xml_add_sibling(runs[[1]], child, .where = "before")
    }
    for (r in runs) {
      xml2::xml_remove(r)
    }
  }
}

# Reconstruct a run's original markdown source. Pandoc consumes emphasis
# delimiters (`_`, `*`, `**`) when it emits <w:i/> / <w:b/>, which matters
# when the delimiters were inside a `$…$` math span: the math's subscripts
# get eaten. Wrap italic runs with `_` and bold runs with `**` so the
# concatenated paragraph text matches what the user originally wrote.
#' @noRd
run_text_with_markdown <- function(run, ns) {
  t <- xml2::xml_find_first(run, "./w:t", ns = ns)
  if (inherits(t, "xml_missing")) {
    return("")
  }
  txt <- xml2::xml_text(t)
  rpr <- xml2::xml_find_first(run, "./w:rPr", ns = ns)
  if (inherits(rpr, "xml_missing")) {
    return(txt)
  }
  italic <- length(xml2::xml_find_all(rpr, "./w:i", ns = ns)) > 0
  bold <- length(xml2::xml_find_all(rpr, "./w:b", ns = ns)) > 0
  if (bold) {
    txt <- paste0("**", txt, "**")
  }
  if (italic) {
    txt <- paste0("_", txt, "_")
  }
  txt
}

# Strip redundant `xmlns:w="…"` declarations from descendants. xml2 adds one
# to every spliced fragment; we can't remove it in-tree without breaking the
# namespace binding (xml2 then serializes without the `w:` prefix), so we
# operate on the serialized string. Keeps the first occurrence on the root.
#' @noRd
dedupe_xmlns_w <- function(path, ns) {
  raw <- paste(readLines(path, warn = FALSE), collapse = "\n")
  w_decl <- paste0(" xmlns:w=\"", ns[["w"]], "\"")
  first <- regexpr(w_decl, raw, fixed = TRUE)
  if (first > 0) {
    keep <- substr(raw, 1, first + attr(first, "match.length") - 1L)
    rest <- substr(raw, first + attr(first, "match.length"), nchar(raw))
    rest <- gsub(w_decl, "", rest, fixed = TRUE)
    raw <- paste0(keep, rest)
  }
  writeLines(raw, path)
}

# Zip the contents of `dir` into `zipfile`. Directory entries are excluded:
# zip::zipr emits them with version_needed_to_extract = 0 in the local header,
# which is not a defined PKZIP value; Word rejects the package with
# "unreadable content" and silently drops the dir entries on repair. OPC
# treats packages as a flat collection of parts, so dir entries aren't
# required anyway.
#' @noRd
zip_dir_contents <- function(dir, zipfile) {
  entries <- list.files(dir, all.files = TRUE, no.. = TRUE)
  old <- setwd(dir)
  on.exit(setwd(old))
  zip::zipr(zipfile, files = entries, include_directories = FALSE)
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
