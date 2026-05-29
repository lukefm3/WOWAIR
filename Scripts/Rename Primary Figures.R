# Rename Tables, Markdowns, and Graphics

copy_image_as_jpg <- function(from, to) {
  jpg_to <- sub("\\.[^.]+$", ".jpg", to)
  dir.create(dirname(jpg_to), recursive = TRUE, showWarnings = FALSE)

  from_abs <- normalizePath(from, winslash = "\\", mustWork = TRUE)
  jpg_abs <- normalizePath(jpg_to, winslash = "\\", mustWork = FALSE)

  ps_quote <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

  command <- paste(
    "Add-Type -AssemblyName System.Drawing;",
    "$src =", ps_quote(from_abs), ";",
    "$dst =", ps_quote(jpg_abs), ";",
    "$img = [System.Drawing.Image]::FromFile($src);",
    "$bmp = New-Object System.Drawing.Bitmap $img.Width, $img.Height;",
    "$graphics = [System.Drawing.Graphics]::FromImage($bmp);",
    "$graphics.Clear([System.Drawing.Color]::White);",
    "$graphics.DrawImage($img, 0, 0, $img.Width, $img.Height);",
    "$bmp.Save($dst, [System.Drawing.Imaging.ImageFormat]::Jpeg);",
    "$graphics.Dispose();",
    "$bmp.Dispose();",
    "$img.Dispose();"
  )

  result <- system2(
    "powershell",
    args = c("-NoProfile", "-Command", command),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!file.exists(jpg_to)) {
    stop("Failed to convert image to jpg: ", from, "\n", paste(result, collapse = "\n"))
  }

  invisible(jpg_to)
}

parse_latex_tabular <- function(path) {
  lines <- readLines(path, warn = FALSE)
  rows <- list()
  in_tabular <- FALSE

  clean_cell <- function(x) {
    x <- trimws(x)
    x <- gsub("<<AMP>>", "&", x, fixed = TRUE)
    x <- gsub("\\\\%", "%", x)
    x <- gsub("\\\\&", "&", x)
    x <- gsub("\\\\_", "_", x)
    x <- gsub("\\\\", "", x)
    x <- gsub("[{}]", "", x)
    trimws(x)
  }

  for (line in lines) {
    line <- trimws(line)

    if (grepl("\\\\begin\\{tabular\\}", line)) {
      in_tabular <- TRUE
      next
    }

    if (!in_tabular) next
    if (grepl("\\\\end\\{tabular\\}", line)) break
    if (!nzchar(line) || grepl("^\\\\(midrule|hline|toprule|bottomrule)", line)) next

    if (grepl("\\\\multicolumn", line)) {
      label <- sub(".*\\\\multicolumn\\{[0-9]+\\}\\{[^}]*\\}\\{([^}]*)\\}.*", "\\1", line)
      rows[[length(rows) + 1]] <- c(type = "section", clean_cell(label))
      next
    }

    if (grepl("&", line, fixed = TRUE)) {
      line <- sub("\\\\\\\\s*$", "", line)
      line_for_split <- gsub("\\\\&", "<<AMP>>", line)
      cells <- clean_cell(strsplit(line_for_split, "&", fixed = TRUE)[[1]])
      row_type <- if (identical(cells[[1]], "Indicator")) "header" else "row"
      rows[[length(rows) + 1]] <- c(type = row_type, cells)
    }
  }

  max_cols <- max(lengths(rows) - 1)
  out <- data.frame(type = vapply(rows, `[[`, character(1), 1), stringsAsFactors = FALSE)

  for (i in seq_len(max_cols)) {
    out[[paste0("C", i)]] <- vapply(
      rows,
      function(row) if (length(row) >= i + 1) row[[i + 1]] else "",
      character(1)
    )
  }

  out
}

combine_latex_table_and_image_as_jpg <- function(table_tex,
                                                 image,
                                                 to,
                                                 table_label,
                                                 image_label,
                                                 image_note = "",
                                                 table_note = "",
                                                 table_first = TRUE) {
  png_to <- sub("\\.[^.]+$", ".png", to)
  dir.create(dirname(png_to), recursive = TRUE, showWarnings = FALSE)

  table_rows <- parse_latex_tabular(table_tex)
  table_csv <- tempfile(fileext = ".csv")
  ps_script <- tempfile(fileext = ".ps1")

  write.csv(table_rows, table_csv, row.names = FALSE, na = "")

  ps_lines <- c(
    "param(",
    "  [string]$TableCsv,",
    "  [string]$ImagePath,",
    "  [string]$OutputPng,",
    "  [string]$TableLabel,",
    "  [string]$ImageLabel,",
    "  [string]$ImageNote,",
    "  [string]$TableNote,",
    "  [string]$TableFirst",
    ")",
    "Add-Type -AssemblyName System.Drawing",
    "$rows = @(Import-Csv -LiteralPath $TableCsv)",
    "$img = [System.Drawing.Image]::FromFile($ImagePath)",
    "$width = $img.Width",
    "$margin = [int]($width * 0.035)",
    "$contentWidth = $width - (2 * $margin)",
    "$cols = @($rows[0].PSObject.Properties.Name | Where-Object { $_ -match '^C\\d+$' } | Sort-Object { [int]($_ -replace '\\D', '') })",
    "$colCount = $cols.Count",
    "if ($colCount -eq 8) {",
    "  $fractions = @(0.235, 0.125, 0.135, 0.135, 0.135, 0.12, 0.06, 0.055)",
    "} else {",
    "  $first = 0.24",
    "  $rest = (1 - $first) / [Math]::Max(1, $colCount - 1)",
    "  $fractions = @($first) + (1..($colCount - 1) | ForEach-Object { $rest })",
    "}",
    "$rowHeights = @()",
    "foreach ($row in $rows) {",
    "  if ($row.type -eq 'header') { $rowHeights += 120 }",
    "  elseif ($row.type -eq 'section') { $rowHeights += 58 }",
    "  else { $rowHeights += 56 }",
    "}",
    "$titleHeight = 66",
    "$gap = 46",
    "$tableNoteHeight = if ([string]::IsNullOrWhiteSpace($TableNote)) { 0 } else { 94 }",
    "$tableHeight = ($rowHeights | Measure-Object -Sum).Sum + $titleHeight + $tableNoteHeight",
    "$plotWidth = [int]($contentWidth * 0.92)",
    "$plotHeight = [int]($img.Height * ($plotWidth / $img.Width))",
    "$imageNoteHeight = if ([string]::IsNullOrWhiteSpace($ImageNote)) { 0 } else { 94 }",
    "$plotBlockHeight = $titleHeight + $plotHeight + $imageNoteHeight",
    "$height = $margin + $tableHeight + $gap + $plotBlockHeight + $margin",
    "$bmp = New-Object System.Drawing.Bitmap $width, $height",
    "$g = [System.Drawing.Graphics]::FromImage($bmp)",
    "$g.Clear([System.Drawing.Color]::White)",
    "$g.SmoothingMode = [System.Drawing.Drawing2D.SmoothingMode]::HighQuality",
    "$g.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic",
    "$g.TextRenderingHint = [System.Drawing.Text.TextRenderingHint]::ClearTypeGridFit",
    "$black = [System.Drawing.Brushes]::Black",
    "$grayText = New-Object System.Drawing.SolidBrush ([System.Drawing.Color]::FromArgb(72, 72, 72))",
    "$headerBrush = New-Object System.Drawing.SolidBrush ([System.Drawing.Color]::FromArgb(242, 242, 242))",
    "$sectionBrush = New-Object System.Drawing.SolidBrush ([System.Drawing.Color]::FromArgb(232, 232, 232))",
    "$linePen = New-Object System.Drawing.Pen ([System.Drawing.Color]::FromArgb(165, 165, 165)), 2",
    "$lightPen = New-Object System.Drawing.Pen ([System.Drawing.Color]::FromArgb(210, 210, 210)), 1",
    "$labelFont = New-Object System.Drawing.Font 'Arial', 38, ([System.Drawing.FontStyle]::Bold)",
    "$headerFont = New-Object System.Drawing.Font 'Arial', 27, ([System.Drawing.FontStyle]::Bold)",
    "$cellFont = New-Object System.Drawing.Font 'Arial', 27, ([System.Drawing.FontStyle]::Regular)",
    "$sectionFont = New-Object System.Drawing.Font 'Arial', 29, ([System.Drawing.FontStyle]::Bold)",
    "$noteFont = New-Object System.Drawing.Font 'Arial', 24, ([System.Drawing.FontStyle]::Regular)",
    "function New-Format([string]$align) {",
    "  $sf = New-Object System.Drawing.StringFormat",
    "  $sf.Alignment = [System.Drawing.StringAlignment]::$align",
    "  $sf.LineAlignment = [System.Drawing.StringAlignment]::Center",
    "  $sf.Trimming = [System.Drawing.StringTrimming]::EllipsisWord",
    "  return $sf",
    "}",
    "$fmtNear = New-Format 'Near'",
    "$fmtCenter = New-Format 'Center'",
    "$fmtFar = New-Format 'Far'",
    "function Draw-Table([int]$x, [int]$y) {",
    "  $labelRect = New-Object System.Drawing.RectangleF $x, $y, $contentWidth, 58",
    "  $g.DrawString($TableLabel, $labelFont, $black, $labelRect, $fmtNear)",
    "  $y += $titleHeight",
    "  for ($r = 0; $r -lt $rows.Count; $r++) {",
    "    $row = $rows[$r]",
    "    $rowHeight = $rowHeights[$r]",
    "    if ($row.type -eq 'section') {",
    "      $rect = New-Object System.Drawing.RectangleF $x, $y, $contentWidth, $rowHeight",
    "      $g.FillRectangle($sectionBrush, $rect)",
    "      $g.DrawRectangle($linePen, $x, $y, $contentWidth, $rowHeight)",
    "      $g.DrawString($row.C1, $sectionFont, $black, $rect, $fmtCenter)",
    "    } else {",
    "      if ($row.type -eq 'header') {",
    "        $g.FillRectangle($headerBrush, $x, $y, $contentWidth, $rowHeight)",
    "      }",
    "      $cx = $x",
    "      for ($c = 0; $c -lt $colCount; $c++) {",
    "        $cw = [int]([Math]::Round($contentWidth * $fractions[$c]))",
    "        if ($c -eq $colCount - 1) { $cw = ($x + $contentWidth) - $cx }",
    "        $cellRect = New-Object System.Drawing.RectangleF ($cx + 10), ($y + 4), ($cw - 20), ($rowHeight - 8)",
    "        $font = if ($row.type -eq 'header') { $headerFont } else { $cellFont }",
    "        $fmt = if ($row.type -eq 'header') { $fmtCenter } elseif ($c -eq 0) { $fmtNear } else { $fmtFar }",
    "        $text = [string]$row.($cols[$c])",
    "        $g.DrawString($text, $font, $black, $cellRect, $fmt)",
    "        $g.DrawRectangle($lightPen, $cx, $y, $cw, $rowHeight)",
    "        $cx += $cw",
    "      }",
    "      $g.DrawLine($linePen, $x, $y, $x + $contentWidth, $y)",
    "    }",
    "    $y += $rowHeight",
    "  }",
    "  $g.DrawLine($linePen, $x, $y, $x + $contentWidth, $y)",
    "  if (-not [string]::IsNullOrWhiteSpace($TableNote)) {",
    "    $noteRect = New-Object System.Drawing.RectangleF $x, ($y + 16), $contentWidth, ($tableNoteHeight - 16)",
    "    $g.DrawString($TableNote, $noteFont, $grayText, $noteRect, $fmtNear)",
    "  }",
    "}",
    "function Draw-ImagePanel([int]$x, [int]$y) {",
    "  $labelRect = New-Object System.Drawing.RectangleF $x, $y, $contentWidth, 58",
    "  $g.DrawString($ImageLabel, $labelFont, $black, $labelRect, $fmtNear)",
    "  $y += $titleHeight",
    "  $plotX = [int]($x + (($contentWidth - $plotWidth) / 2))",
    "  $plotRect = New-Object System.Drawing.Rectangle $plotX, $y, $plotWidth, $plotHeight",
    "  $g.DrawImage($img, $plotRect)",
    "  if (-not [string]::IsNullOrWhiteSpace($ImageNote)) {",
    "    $noteRect = New-Object System.Drawing.RectangleF $x, ($y + $plotHeight + 16), $contentWidth, ($imageNoteHeight - 16)",
    "    $g.DrawString($ImageNote, $noteFont, $grayText, $noteRect, $fmtNear)",
    "  }",
    "}",
    "$y = $margin",
    "if ($TableFirst -eq 'true') {",
    "  Draw-Table $margin $y",
    "  $y += $tableHeight + $gap",
    "  Draw-ImagePanel $margin $y",
    "} else {",
    "  Draw-ImagePanel $margin $y",
    "  $y += $plotBlockHeight + $gap",
    "  Draw-Table $margin $y",
    "}",
    "$bmp.Save($OutputPng, [System.Drawing.Imaging.ImageFormat]::Png)",
    "$g.Dispose()",
    "$bmp.Dispose()",
    "$img.Dispose()"
  )

  writeLines(ps_lines, ps_script)

  ps_quote <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  command <- paste(
    "&", ps_quote(normalizePath(ps_script, winslash = "\\", mustWork = TRUE)),
    "-TableCsv", ps_quote(normalizePath(table_csv, winslash = "\\", mustWork = TRUE)),
    "-ImagePath", ps_quote(normalizePath(image, winslash = "\\", mustWork = TRUE)),
    "-OutputPng", ps_quote(normalizePath(png_to, winslash = "\\", mustWork = FALSE)),
    "-TableLabel", ps_quote(table_label),
    "-ImageLabel", ps_quote(image_label),
    "-ImageNote", ps_quote(image_note),
    "-TableNote", ps_quote(table_note),
    "-TableFirst", ps_quote(if (isTRUE(table_first)) "true" else "false")
  )

  result <- system2(
    "powershell",
    args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", command),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!file.exists(png_to)) {
    stop("Failed to combine table and image: ", to, "\n", paste(result, collapse = "\n"))
  }

  copy_image_as_jpg(png_to, to)
}

# Table 1: Summary Table
file.copy(from = "Main Figures and Tables/summary_table.tex", to   = "Paper/Tables/Table 1.tex", overwrite = T)

# Table 2: Varying Information Availability
file.copy(from = "Manually Created Tables/information_asymmetry.tex", to   = "Paper/Tables/Table 2.tex", overwrite = T)

# Table 3: Iceland Sectoral Employment Concentration
file.copy(from = "Main Figures and Tables/specialization.tex", to   = "Paper/Tables/Table 3.tex", overwrite = T)

# Table 4: Placebo Counterfactual Analysis
file.copy(from = "Main Figures and Tables/placebo_impact_table.tex", to   = "Paper/Tables/Table 4.tex", overwrite = T)

# Table 5: Choice of Control Countries
file.copy(from = "Main Figures and Tables/choice_of_control_countries.tex", to   = "Paper/Tables/Table 5.tex", overwrite = T)

# Table 6: Projected Change in Tourism From Different Coefficient Groups
file.copy(from = "Main Figures and Tables/choice_of_variables.tex", to   = "Paper/Tables/Table 6.tex", overwrite = T)

# Table 7: Forecasted Impacts Without Time-Series
file.copy(from = "Main Figures and Tables/drop_time_series.tex", to   = "Paper/Tables/Table 7.tex", overwrite = T)

# Table 8: Two-Way F.E. Treatment Estimate
file.copy(from = "Main Figures and Tables/DID_table.tex", to   = "Paper/Tables/Table 8.tex", overwrite = T)


# Figures

# Figure 1: Recent Information Bias
copy_image_as_jpg(from = "Main Figures and Tables/recent_information_bias.png", to = "Paper/Figures/Figure 1.jpg")

# Figure 2: Posterior Inclusion Probabilities
copy_image_as_jpg(from = "Main Figures and Tables/coefficients_arrivals_plot.png", to = "Paper/Figures/Figure 2.jpg")

# Figure 3: Cumulative effects across pre-periods
copy_image_as_jpg(from = "Main Figures and Tables/time_callibration.png", to = "Paper/Figures/Figure 3.jpg")

# Figure 4: Decline in Airline Passengers
copy_image_as_jpg(from = "Main Figures and Tables/primary_arrivals_plot.png", to = "Paper/Figures/Figure 4.jpg")

# Figure 5: Impacts on Passenger Counts by Nationality
copy_image_as_jpg(from = "Main Figures and Tables/passengers_by_nationality.png", to = "Paper/Figures/Figure 5a.jpg")
file.copy(from = "Main Figures and Tables/passenger_nationality.tex", to   = "Paper/Figures/Figure 5b.tex", overwrite = T)
combine_latex_table_and_image_as_jpg(
  table_tex = "Main Figures and Tables/passenger_nationality.tex",
  image = "Main Figures and Tables/passengers_by_nationality.png",
  to = "Paper/Figures/Figure 5.jpg",
  table_label = "(b) Passenger Summary by Nationality",
  image_label = "(a) Monthly Difference in Observed and Estimated Departures by Nationality",
  image_note = "Note: Difference between observed and estimated counterfactual passenger departures by nationality. Passengers represent departures through international security.",
  table_note = "Note: Data show observed vs. estimated counterfactual passenger departures by nationality.",
  table_first = FALSE
)

# Figure 6: Monthly Arrivals Relative to Counterfactual 
copy_image_as_jpg(from = "Main Figures and Tables/arrivals_by_month.png", to = "Paper/Figures/Figure 6.jpg")

# Figure 7: National Labor Impacts
# Figure 7a:
copy_image_as_jpg(from = "Main Figures and Tables/national_labor_plot.png", to = "Paper/Figures/Figure 7a.jpg")

# Figure 7b:
file.copy(from = "Main Figures and Tables/comprehensive_labor_impact_table.tex", to   = "Paper/Figures/Figure 7b.tex", overwrite = T)

# Combined Figure 7:
combine_latex_table_and_image_as_jpg(
  table_tex = "Main Figures and Tables/comprehensive_labor_impact_table.tex",
  image = "Main Figures and Tables/national_labor_plot.png",
  to = "Paper/Figures/Figure 7.jpg",
  table_label = "(a) Comprehensive Labor Impacts",
  image_label = "(b) Monthly Effect on Unemployed Persons and Unemployment Rate",
  image_note = "Note: Solid line shows observed values and dashed line shows the synthetic counterfactual. Monthly series are plotted as differences relative to the counterfactual.",
  table_note = "Notes: Stars denote p-value thresholds: *** p<0.01, ** p<0.05, and * p<0.10.",
  table_first = TRUE
)

# Figure 8: Monthly Differences in Estimated and Realized Labor Levels
copy_image_as_jpg(from = "Main Figures and Tables/sectoral_labor_impact_plot.png", to = "Paper/Figures/Figure 8.jpg")

# Figure 9: Projected Monthly Differences (No Time-Series Elements)
copy_image_as_jpg(from = "Main Figures and Tables/dropped_time_series_differences.png", to = "Paper/Figures/Figure 9.jpg")


# Figure 10: Projected Cumulative Differences (No Time-Series Elements)
copy_image_as_jpg(from = "Main Figures and Tables/dropped_time_series_cumul_difs.png", to = "Paper/Figures/Figure 10.jpg")


# Move any .png or .tex files from folder a to folder B: 
from <- getwd()   # current working directory
to   <- file.path(getwd(), "Additional Tables and Figures")

# create destination if it doesn't exist
dir.create(to, showWarnings = FALSE)

image_files <- list.files(from, pattern = "\\.(png|jpg|jpeg)$", full.names = TRUE, ignore.case = TRUE)
text_files <- list.files(from, pattern = "\\.(tex|md)$", full.names = TRUE, ignore.case = TRUE)

invisible(lapply(image_files, function(path) {
  out <- file.path(to, paste0(tools::file_path_sans_ext(basename(path)), ".jpg"))
  copy_image_as_jpg(path, out)
}))

file.rename(text_files, file.path(to, basename(text_files)))

