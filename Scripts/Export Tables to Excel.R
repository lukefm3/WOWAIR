export_tables_to_excel <- function(
  tables_dir = file.path("Paper", "Tables"),
  output_file = file.path("Paper", "Tables", "WOWAIR Tables Editable.xlsx")
) {
  table_files <- list.files(
    tables_dir,
    pattern = "^Table S?[0-9]+\\.tex$",
    full.names = TRUE
  )

  if (length(table_files) == 0) {
    warning("No Paper/Tables/Table *.tex files found to export.")
    return(invisible(NULL))
  }

  table_ids <- sub("^Table (S?[0-9]+)\\.tex$", "\\1", basename(table_files))
  table_order <- order(grepl("^S", table_ids), as.integer(sub("^S", "", table_ids)))
  table_files <- table_files[table_order]
  table_ids <- table_ids[table_order]

  # Table 2 is exported from the numbered manuscript copy. The rename script
  # selects either the combined or three-row version before this step runs.

  clean_latex_cell <- function(x) {
    x <- trimws(x)
    x <- gsub("\\\\hspace\\{[^}]*\\}", "", x)

    # Strip common formatting wrappers. Repeat for simple nested wrappers.
    for (i in seq_len(6)) {
      x <- gsub("\\\\(textbf|textit|emph)\\{([^{}]*)\\}", "\\2", x, perl = TRUE)
    }

    x <- gsub("\\\\%", "%", x)
    x <- gsub("\\\\&", "&", x)
    x <- gsub("\\\\\\$", "$", x)
    x <- gsub("\\\\_", "_", x)
    x <- gsub("\\$", "", x)
    x <- gsub("[{}]", "", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  split_latex_row <- function(row) {
    placeholder <- "__LATEX_ESCAPED_AMPERSAND__"
    row <- gsub("\\\\&", placeholder, row)
    cells <- strsplit(row, "\\s*&\\s*", perl = TRUE)[[1]]
    cells <- gsub(placeholder, "&", cells, fixed = TRUE)
    vapply(cells, clean_latex_cell, character(1), USE.NAMES = FALSE)
  }

  parse_latex_table <- function(path) {
    lines <- readLines(path, warn = FALSE)
    # Keep escaped percent signs (\%) because they are table content, not comments.
    lines <- lines[!grepl("^\\s*%", lines)]

    start <- grep("\\\\begin\\{tabular\\}", lines)
    end <- grep("\\\\end\\{tabular\\}", lines)

    if (length(start) == 0 || length(end) == 0) {
      warning("No tabular block found in ", path)
      return(data.frame())
    }

    body <- lines[(start[1] + 1):(end[1] - 1)]
    body <- trimws(body)
    body <- body[body != ""]
    body <- body[!grepl("^\\\\(hline|toprule|midrule|bottomrule|addlinespace)", body)]

    rows <- list()

    for (line in body) {
      if (!grepl("\\\\\\\\", line) && !grepl("\\\\multicolumn", line)) next

      line <- sub("\\\\\\\\[[:space:]]*$", "", line)
      line <- trimws(line)
      if (line == "") next

      if (grepl("\\\\multicolumn", line)) {
        label <- sub(
          ".*\\\\multicolumn\\{[0-9]+\\}\\{[^}]*\\}\\{(.*)\\}.*",
          "\\1",
          line
        )
        rows[[length(rows) + 1]] <- clean_latex_cell(label)
      } else {
        rows[[length(rows) + 1]] <- split_latex_row(line)
      }
    }

    if (length(rows) == 0) return(data.frame())

    max_cols <- max(vapply(rows, length, integer(1)))
    rows <- lapply(rows, function(x) c(x, rep("", max_cols - length(x))))

    out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(out) <- paste0("Column", seq_len(ncol(out)))
    out
  }

  tables <- stats::setNames(
    lapply(table_files, parse_latex_table),
    paste0("Table", table_ids)
  )

  tables <- tables[vapply(tables, nrow, integer(1)) > 0]
  if (length(tables) == 0) {
    warning("No table rows were parsed from Paper/Tables.")
    return(invisible(NULL))
  }

  xml_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&apos;", x, fixed = TRUE)
    x
  }

  excel_col <- function(n) {
    out <- character(length(n))
    for (i in seq_along(n)) {
      x <- n[i]
      label <- ""
      while (x > 0) {
        rem <- (x - 1) %% 26
        label <- paste0(intToUtf8(65 + rem), label)
        x <- (x - rem - 1) %/% 26
      }
      out[i] <- label
    }
    out
  }

  write_text <- function(path, text) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, path, useBytes = TRUE)
  }

  cell_xml <- function(row_idx, col_idx, value, style_id) {
    ref <- paste0(excel_col(col_idx), row_idx)
    value <- as.character(value)
    if (is.na(value) || value == "") {
      return(sprintf('<c r="%s" s="%s"/>', ref, style_id))
    }
    sprintf(
      '<c r="%s" s="%s" t="inlineStr"><is><t>%s</t></is></c>',
      ref,
      style_id,
      xml_escape(value)
    )
  }

  sheet_xml <- function(df) {
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    dim_ref <- paste0("A1:", excel_col(n_cols), n_rows)

    section_rows <- which(
      trimws(df[[1]]) != "" &
        apply(df[, -1, drop = FALSE], 1, function(x) all(trimws(x) == ""))
    )

    col_widths <- vapply(seq_len(n_cols), function(col_idx) {
      width <- max(nchar(as.character(df[[col_idx]]), keepNA = FALSE), na.rm = TRUE) + 2
      max(10, min(width, 45))
    }, numeric(1))

    cols <- paste0(
      sprintf(
        '<col min="%s" max="%s" width="%s" customWidth="1"/>',
        seq_len(n_cols),
        seq_len(n_cols),
        sprintf("%.1f", col_widths)
      ),
      collapse = ""
    )

    rows <- vapply(seq_len(n_rows), function(row_idx) {
      style_id <- if (row_idx == 1) {
        1
      } else if (row_idx %in% section_rows) {
        2
      } else {
        0
      }
      cells <- vapply(seq_len(n_cols), function(col_idx) {
        cell_xml(row_idx, col_idx, df[[col_idx]][row_idx], style_id)
      }, character(1))
      sprintf('<row r="%s">%s</row>', row_idx, paste0(cells, collapse = ""))
    }, character(1))

    paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<dimension ref="', dim_ref, '"/>',
      '<sheetViews><sheetView workbookViewId="0"><pane ySplit="1" topLeftCell="A2" ',
      'activePane="bottomLeft" state="frozen"/></sheetView></sheetViews>',
      '<cols>', cols, '</cols>',
      '<sheetData>', paste0(rows, collapse = ""), '</sheetData>',
      '</worksheet>'
    )
  }

  styles_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    '<fonts count="2">',
    '<font><sz val="11"/><name val="Calibri"/></font>',
    '<font><b/><sz val="11"/><name val="Calibri"/></font>',
    '</fonts>',
    '<fills count="4">',
    '<fill><patternFill patternType="none"/></fill>',
    '<fill><patternFill patternType="gray125"/></fill>',
    '<fill><patternFill patternType="solid"><fgColor rgb="FFD9EAF7"/><bgColor indexed="64"/></patternFill></fill>',
    '<fill><patternFill patternType="solid"><fgColor rgb="FFF2F2F2"/><bgColor indexed="64"/></patternFill></fill>',
    '</fills>',
    '<borders count="2">',
    '<border><left/><right/><top/><bottom/><diagonal/></border>',
    '<border><left style="thin"/><right style="thin"/><top style="thin"/><bottom style="thin"/><diagonal/></border>',
    '</borders>',
    '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
    '<cellXfs count="3">',
    '<xf numFmtId="0" fontId="0" fillId="0" borderId="1" xfId="0" applyBorder="1" applyAlignment="1"><alignment vertical="top" wrapText="1"/></xf>',
    '<xf numFmtId="0" fontId="1" fillId="2" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
    '<xf numFmtId="0" fontId="1" fillId="3" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment vertical="center" wrapText="1"/></xf>',
    '</cellXfs>',
    '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
    '</styleSheet>'
  )

  workbook_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets>',
    paste0(
      sprintf(
        '<sheet name="%s" sheetId="%s" r:id="rId%s"/>',
        xml_escape(names(tables)),
        seq_along(tables),
        seq_along(tables)
      ),
      collapse = ""
    ),
    '</sheets></workbook>'
  )

  workbook_rels <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    paste0(
      sprintf(
        '<Relationship Id="rId%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet%s.xml"/>',
        seq_along(tables),
        seq_along(tables)
      ),
      collapse = ""
    ),
    '<Relationship Id="rIdStyles" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>',
    '</Relationships>'
  )

  content_types <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
    '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
    paste0(
      sprintf(
        '<Override PartName="/xl/worksheets/sheet%s.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
        seq_along(tables)
      ),
      collapse = ""
    ),
    '</Types>'
  )

  package_rels <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
    '</Relationships>'
  )

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  output_file <- normalizePath(dirname(output_file), winslash = "/", mustWork = TRUE) |>
    file.path(basename(output_file))

  package_dir <- file.path(tempdir(), paste0("wowair_table_xlsx_", Sys.getpid()))
  if (dir.exists(package_dir)) unlink(package_dir, recursive = TRUE)
  dir.create(package_dir, recursive = TRUE)

  write_text(file.path(package_dir, "[Content_Types].xml"), content_types)
  write_text(file.path(package_dir, "_rels", ".rels"), package_rels)
  write_text(file.path(package_dir, "xl", "workbook.xml"), workbook_xml)
  write_text(file.path(package_dir, "xl", "styles.xml"), styles_xml)
  write_text(file.path(package_dir, "xl", "_rels", "workbook.xml.rels"), workbook_rels)

  for (idx in seq_along(tables)) {
    write_text(
      file.path(package_dir, "xl", "worksheets", paste0("sheet", idx, ".xml")),
      sheet_xml(tables[[idx]])
    )
  }

  if (file.exists(output_file)) unlink(output_file)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(package_dir)
  zip_files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  zip_result <- try(utils::zip(zipfile = output_file, files = zip_files), silent = TRUE)

  if (inherits(zip_result, "try-error") || !file.exists(output_file)) {
    powershell <- Sys.which("powershell")
    if (!nzchar(powershell)) {
      stop("Could not create xlsx zip archive with R utils::zip, and PowerShell was not found.")
    }
    command <- paste0(
      "Compress-Archive -Path ",
      shQuote(file.path(package_dir, "*")),
      " -DestinationPath ",
      shQuote(output_file),
      " -Force"
    )
    status <- system2(powershell, c("-NoProfile", "-Command", command))
    if (!file.exists(output_file) || status != 0) {
      stop("Could not create xlsx zip archive.")
    }
  }

  unlink(package_dir, recursive = TRUE)
  message("Exported editable table workbook: ", normalizePath(output_file, winslash = "/"))

  invisible(output_file)
}

export_tables_to_excel()
