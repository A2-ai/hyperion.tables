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
  sanitize_gt_docx(path)
  invisible(path)
}

#' Post-process a gt-generated `.docx` so Word opens it without repair prompts
#'
#' gt's docx output (produced via pandoc) violates OOXML in ways that trigger
#' Word's "unreadable content" and "contains fields" prompts. Each helper
#' below addresses one specific validation issue observed in Word's repair
#' log. Ordering matters: tcBorders are stripped before start/end renaming
#' (so we don't rename descendants about to be deleted), grids are injected
#' before child-reorder passes, xmlns cleanup runs after all splicing, math
#' rewriting runs last so it sees the final structure.
#' @noRd
sanitize_gt_docx <- function(path) {
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

  ensure_math_namespace(doc, ns)
  declare_modern_word_namespaces(doc)
  fix_caption_style(doc, ns)
  strip_tc_borders(doc, ns)
  strip_seq_table_field(doc, ns)
  normalize_xml_space(doc, ns)
  normalize_jc_values(doc, ns)
  rename_rtl_border_tags(doc, ns)
  strip_zero_cell_margins(doc, ns)
  fill_empty_cells(doc, ns)
  inject_table_grids(doc, ns)
  reorder_ooxml_sequences(doc, ns)
  rewrite_latex_to_omml(doc, ns)

  xml2::write_xml(doc, doc_path)
  post_write_cleanup(doc_path, ns)
  drop_empty_custom_xml(stage)
  set_word_compat_mode(stage)

  unlink(path)
  zip_dir_contents(stage, path)
}

# --- sanitize_gt_docx helpers --------------------------------------------

#' @noRd
ensure_math_namespace <- function(doc, ns) {
  root <- xml2::xml_root(doc)
  if (is.na(xml2::xml_attr(root, "xmlns:m"))) {
    xml2::xml_set_attr(root, "xmlns:m", ns[["m"]])
  }
}

# Word opens the document in "Compatibility Mode" when <w:document> lacks the
# modern Microsoft namespaces and mc:Ignorable. Declare them so Word treats
# the file as a current-format .docx.
#' @noRd
declare_modern_word_namespaces <- function(doc) {
  modern_ns <- c(
    mc = "http://schemas.openxmlformats.org/markup-compatibility/2006",
    w14 = "http://schemas.microsoft.com/office/word/2010/wordml",
    w15 = "http://schemas.microsoft.com/office/word/2012/wordml",
    w16se = "http://schemas.microsoft.com/office/word/2015/wordml/symex",
    w16cid = "http://schemas.microsoft.com/office/word/2016/wordml/cid",
    w16 = "http://schemas.microsoft.com/office/word/2018/wordml",
    w16cex = "http://schemas.microsoft.com/office/word/2018/wordml/cex",
    wp14 = "http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
  )
  root <- xml2::xml_root(doc)
  for (prefix in names(modern_ns)) {
    attr_name <- paste0("xmlns:", prefix)
    if (is.na(xml2::xml_attr(root, attr_name))) {
      xml2::xml_set_attr(root, attr_name, modern_ns[[prefix]])
    }
  }
  xml2::xml_set_attr(
    root,
    "mc:Ignorable",
    "w14 w15 w16se w16cid w16 w16cex wp14"
  )
}

# Word treats a document as legacy (Compatibility Mode) unless
# word/settings.xml has a <w:compat> block declaring compatibilityMode=15,
# AND the <w:settings> root itself declares the modern Microsoft namespaces.
#' @noRd
set_word_compat_mode <- function(stage) {
  settings_path <- file.path(stage, "word", "settings.xml")
  if (!file.exists(settings_path)) {
    return(invisible())
  }
  settings <- xml2::read_xml(settings_path)
  declare_modern_word_namespaces(settings)
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  if (length(xml2::xml_find_all(settings, ".//w:compat", ns = ns)) == 0) {
    compat_xml <- paste0(
      "<w:compat xmlns:w=\"",
      ns[["w"]],
      "\">",
      "<w:compatSetting w:name=\"compatibilityMode\"",
      " w:uri=\"http://schemas.microsoft.com/office/word\" w:val=\"15\"/>",
      "</w:compat>"
    )
    xml2::xml_add_child(
      xml2::xml_root(settings),
      xml2::xml_root(xml2::read_xml(compat_xml))
    )
  }
  xml2::write_xml(settings, settings_path)
  # Strip the redundant xmlns:w that xml2 added on <w:compat>.
  raw <- paste(readLines(settings_path, warn = FALSE), collapse = "\n")
  raw <- gsub(
    paste0(" xmlns:w=\"", ns[["w"]], "\""),
    "",
    raw,
    fixed = TRUE
  )
  # Re-add the root's xmlns:w (we stripped the first occurrence too).
  raw <- sub(
    "<w:settings",
    paste0("<w:settings xmlns:w=\"", ns[["w"]], "\""),
    raw,
    fixed = TRUE
  )
  writeLines(raw, settings_path)
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

# gt emits <w:left>/<w:right> border children without the required `w:sz`
# attribute, so Word strips the whole <w:tcBorders> on open. Drop the element
# ourselves. Users who want cell borders can add them in Word.
#' @noRd
strip_tc_borders <- function(doc, ns) {
  for (tcb in xml2::xml_find_all(doc, ".//w:tcBorders", ns = ns)) {
    xml2::xml_remove(tcb)
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
    end_idx <- begin_idx
    for (i in seq(begin_idx + 1, length(runs))) {
      if (
        length(xml2::xml_find_all(
          runs[[i]],
          "./w:fldChar[@w:fldCharType='end']",
          ns = ns
        )) >
          0
      ) {
        end_idx <- i
        break
      }
    }
    for (i in seq(begin_idx, end_idx)) {
      r <- runs[[i]]
      has_instr <- length(xml2::xml_find_all(r, "./w:instrText", ns = ns)) > 0
      has_fld <- length(xml2::xml_find_all(r, "./w:fldChar", ns = ns)) > 0
      if (has_instr || has_fld) xml2::xml_remove(r)
    }
  }
}

# gt sometimes emits xml:space="default"; Word expects "preserve" for docx
# text content.
#' @noRd
normalize_xml_space <- function(doc, ns) {
  for (node in xml2::xml_find_all(doc, ".//*[@xml:space='default']", ns = ns)) {
    xml2::xml_set_attr(node, "xml:space", "preserve")
  }
}

# <w:jc w:val="start"/> and w:val="end" are the newer RTL-aware justification
# values; some Word versions reject them. Drop "start" (default behaviour)
# and rewrite "end" → "right".
#' @noRd
normalize_jc_values <- function(doc, ns) {
  for (n in xml2::xml_find_all(doc, ".//w:jc[@w:val='start']", ns = ns)) {
    xml2::xml_remove(n)
  }
  for (n in xml2::xml_find_all(doc, ".//w:jc[@w:val='end']", ns = ns)) {
    xml2::xml_set_attr(n, "w:val", "right")
  }
}

# gt emits the new-style RTL tags <w:start> and <w:end> inside <w:tblCellMar>
# and <w:tblBorders> etc. Rename to the classic <w:left>/<w:right> that Word
# accepts unconditionally.
#' @noRd
rename_rtl_border_tags <- function(doc, ns) {
  rename_one <- function(old, new) {
    for (n in xml2::xml_find_all(doc, paste0(".//w:", old), ns = ns)) {
      attrs <- xml2::xml_attrs(n)
      attr_str <- if (length(attrs)) {
        paste(sprintf("w:%s=\"%s\"", names(attrs), attrs), collapse = " ")
      } else {
        ""
      }
      new_xml <- paste0(
        "<w:",
        new,
        " xmlns:w=\"",
        ns[["w"]],
        "\"",
        if (nzchar(attr_str)) paste0(" ", attr_str) else "",
        "/>"
      )
      xml2::xml_add_sibling(
        n,
        xml2::xml_root(xml2::read_xml(new_xml)),
        .where = "before"
      )
      xml2::xml_remove(n)
    }
  }
  rename_one("start", "left")
  rename_one("end", "right")
}

# Word's repair strips <w:top> and <w:bottom> children from <w:tblCellMar>
# when they carry w:w="0". Drop them ourselves. Snapshot targets before
# removing to avoid mutating-while-iterating.
#' @noRd
strip_zero_cell_margins <- function(doc, ns) {
  for (m in xml2::xml_find_all(doc, ".//w:tblCellMar", ns = ns)) {
    kids <- xml2::xml_children(m)
    targets <- kids[
      xml2::xml_name(kids) %in%
        c("top", "bottom") &
        vapply(
          kids,
          function(c) identical(xml2::xml_attr(c, "w"), "0"),
          logical(1)
        )
    ]
    for (t in targets) {
      xml2::xml_remove(t)
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
    n_cols <- length(xml2::xml_find_all(first_tr, "./w:tc", ns = ns))
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

# Rewrite every `$…$` inline-math span inside a <w:t> as a sibling <m:oMath>
# element generated by equatags. Runs last so the text content reflects all
# prior cleanup. Preserves the original run's <w:rPr> for the text fragments
# on either side of the equation.
#' @noRd
rewrite_latex_to_omml <- function(doc, ns) {
  for (t_node in xml2::xml_find_all(doc, ".//w:t", ns = ns)) {
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
}

# Post-write text passes for changes xml2 can't do cleanly:
#
# 1. Strip zero-valued `w:before` on <w:spacing>. xml2 can't remove
#    namespaced regular attributes (libxml2 treats them specially) and Word
#    flags them when repairing.
# 2. Strip redundant `xmlns:w="…"` declarations from descendants of
#    <w:document>. xml2 adds one to every spliced fragment. xml2 can remove
#    the attribute in-tree, but doing so breaks the namespace binding of the
#    node and xml2 then serializes without the `w:` prefix — we must operate
#    on the already-serialized string instead. Keep only the first
#    occurrence (on <w:document>).
#' @noRd
post_write_cleanup <- function(doc_path, ns) {
  raw <- paste(readLines(doc_path, warn = FALSE), collapse = "\n")
  raw <- gsub(" w:before=\"0\"", "", raw, fixed = TRUE)
  w_decl <- paste0(" xmlns:w=\"", ns[["w"]], "\"")
  first <- regexpr(w_decl, raw, fixed = TRUE)
  if (first > 0) {
    keep <- substr(raw, 1, first + attr(first, "match.length") - 1L)
    rest <- substr(raw, first + attr(first, "match.length"), nchar(raw))
    rest <- gsub(w_decl, "", rest, fixed = TRUE)
    raw <- paste0(keep, rest)
  }
  writeLines(raw, doc_path)
}

# gt emits an empty `<Properties/>` in docProps/custom.xml; per OOXML the
# CT_Properties schema requires at least one child. Drop the file and its
# references in [Content_Types].xml and _rels/.rels.
#' @noRd
drop_empty_custom_xml <- function(stage) {
  custom_path <- file.path(stage, "docProps", "custom.xml")
  if (!file.exists(custom_path)) {
    return(invisible())
  }
  unlink(custom_path)
  ct_path <- file.path(stage, "[Content_Types].xml")
  ct <- paste(readLines(ct_path, warn = FALSE), collapse = "")
  ct <- gsub(
    "<Override PartName=\"/docProps/custom\\.xml\"[^/]*/>",
    "",
    ct
  )
  writeLines(ct, ct_path)
  rels_path <- file.path(stage, "_rels", ".rels")
  rels <- paste(readLines(rels_path, warn = FALSE), collapse = "")
  rels <- gsub(
    "<Relationship[^/]*Target=\"docProps/custom\\.xml\"[^/]*/>",
    "",
    rels
  )
  writeLines(rels, rels_path)
}

# Zip the immediate contents of `dir` (files + directories) into `zipfile`,
# relying on zip::zipr's recursion to preserve the nested structure OOXML
# requires. The setwd/on.exit dance is localized here so caller-side errors
# can't leak a changed working directory.
#' @noRd
zip_dir_contents <- function(dir, zipfile) {
  entries <- list.files(dir, all.files = TRUE, no.. = TRUE)
  old <- setwd(dir)
  on.exit(setwd(old))
  zip::zipr(zipfile, files = entries)
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
