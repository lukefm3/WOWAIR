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

rotate_image_90_clockwise <- function(path) {
  path_abs <- normalizePath(path, winslash = "\\", mustWork = TRUE)
  temp_abs <- paste0(path_abs, ".rotating", tools::file_ext(path_abs))

  ps_quote <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

  image_format <- switch(
    tolower(tools::file_ext(path_abs)),
    "jpg" = "[System.Drawing.Imaging.ImageFormat]::Jpeg",
    "jpeg" = "[System.Drawing.Imaging.ImageFormat]::Jpeg",
    "png" = "[System.Drawing.Imaging.ImageFormat]::Png",
    "[System.Drawing.Imaging.ImageFormat]::Jpeg"
  )

  command <- paste(
    "Add-Type -AssemblyName System.Drawing;",
    "$src =", ps_quote(path_abs), ";",
    "$tmp =", ps_quote(temp_abs), ";",
    "$img = [System.Drawing.Image]::FromFile($src);",
    "$img.RotateFlip([System.Drawing.RotateFlipType]::Rotate90FlipNone);",
    "$img.Save($tmp,", image_format, ");",
    "$img.Dispose();",
    "Move-Item -LiteralPath $tmp -Destination $src -Force;"
  )

  result <- system2(
    "powershell",
    args = c("-NoProfile", "-Command", command),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!file.exists(path)) {
    stop("Failed to rotate image: ", path, "\n", paste(result, collapse = "\n"))
  }

  invisible(path)
}

parse_latex_tabular <- function(path) {
  lines <- readLines(path, warn = FALSE)
  rows <- list()
  in_tabular <- FALSE

  clean_cell <- function(x) {
    x <- trimws(x)
    x <- gsub("\\\\textbf\\{([^}]*)\\}", "\\1", x)
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
      label <- sub(".*\\\\multicolumn\\{[0-9]+\\}\\{[^}]*\\}\\{(.*)\\}.*", "\\1", line)
      rows[[length(rows) + 1]] <- c(type = "section", clean_cell(label))
      next
    }

    if (grepl("&", line, fixed = TRUE)) {
      line <- sub("\\\\\\\\s*$", "", line)
      line_for_split <- gsub("\\\\&", "<<AMP>>", line)
      cells <- clean_cell(strsplit(line_for_split, "&", fixed = TRUE)[[1]])
      row_type <- if (cells[[1]] %in% c("Indicator", "Outcome", "Passenger Series")) "header" else "row"
      rows[[length(rows) + 1]] <- c(type = row_type, cells)
    }
  }

  format_numeric_cell <- function(x, style) {
    x <- trimws(x)
    if (!nzchar(x)) return(x)
    if (grepl("%", x, fixed = TRUE)) return(x)

    match <- regexec("^(-?[0-9]+(?:\\.[0-9]+)?(?:e[+-]?[0-9]+)?)(.*)$", x, ignore.case = TRUE)
    parts <- regmatches(x, match)[[1]]

    if (length(parts) == 0) return(x)

    value <- suppressWarnings(as.numeric(parts[2]))
    suffix <- parts[3]

    if (is.na(value)) return(x)

    if (identical(style, "percent")) {
      return(paste0(formatC(value, format = "f", digits = 2), "%", suffix))
    }

    if (identical(style, "comma")) {
      return(paste0(formatC(round(value), format = "d", big.mark = ","), suffix))
    }

    x
  }

  format_rows_for_display <- function(out) {
    header_index <- which(out$type == "header")[1]
    if (is.na(header_index)) return(out)

    value_cols <- grep("^C[0-9]+$", names(out), value = TRUE)
    headers <- out[header_index, value_cols]

    for (col in value_cols) {
      header <- headers[[col]]

      style <- if (grepl("\\(\\%\\)|\\(\\s*%\\s*\\)|Relative Impact|% Change|YoY Actual Change", header)) {
        "percent"
      } else if (grepl("p-value", header, ignore.case = TRUE)) {
        "plain"
      } else if (grepl("Impact|Predicted|Observed|Upper|Lower|RMSE", header, ignore.case = TRUE)) {
        "comma"
      } else {
        "plain"
      }

      if (!identical(style, "plain")) {
        data_rows <- out$type == "row"
        out[data_rows, col] <- vapply(out[data_rows, col], format_numeric_cell, character(1), style = style)
      }
    }

    out
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

  format_rows_for_display(out)
}

combine_latex_table_and_image_as_jpg <- function(table_tex,
                                                 image,
                                                 to,
                                                 table_label,
                                                 image_label,
                                                 image_note = "",
                                                 table_note = "",
                                                 image_scale = 0.92,
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
    "  [double]$ImageScale,",
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
    "$plotWidth = [int]($contentWidth * $ImageScale)",
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
    "$headerBrush = [System.Drawing.Brushes]::White",
    "$sectionBrush = [System.Drawing.Brushes]::White",
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
    "-ImageScale", ps_quote(as.character(image_scale)),
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

# Candidate comparison units table
file.copy(from = "Main Figures and Tables/summary_table.tex", to   = "Paper/Tables/ComparisonUnits.tex", overwrite = T)

# Table 1: candidate comparison units
file.copy(from = "Main Figures and Tables/summary_table.tex", to = "Paper/Tables/Table 1.tex", overwrite = TRUE)

# Table 2: combined passenger and nationality effects by default.
# Uncomment the next line to use only Total, Icelandic, and Foreign Passengers.
passenger_table <- "Main Figures and Tables/passenger_nationality_combined.tex"
# passenger_table <- "Main Figures and Tables/passenger_nationality.tex"
if (!file.exists(passenger_table)) stop("Missing Table 2 source: ", passenger_table)
file.copy(from = passenger_table, to = "Paper/Tables/Table 2.tex", overwrite = TRUE)
writeLines(c(
  "\\begin{table}[htbp]", "\\centering", "\\scriptsize",
  "\\caption{Passenger Effects of the WOW Air Bankruptcy}\\label{tab:passenger_impact}",
  "\\resizebox{\\textwidth}{!}{\\input{Paper/Tables/Table 2.tex}}",
  "\\begin{minipage}{\\textwidth}",
  "\\raggedright\\scriptsize \\textit{Note:} Estimates compare April--December 2019 observed passenger departures with the no-bankruptcy counterfactual. The 95\\% intervals and nine-month impacts are in passenger counts. Stars denote statistical significance: *** $p<0.01$, ** $p<0.05$, * $p<0.10$.",
  "\\end{minipage}", "\\end{table}"
), "Paper/Tables/PassengerImpact.tex")

# Table S1 in the supplemental manuscript: Varying Data & Information Availability
file.copy(from = "Manually Created Tables/information_asymmetry.tex", to = "Paper/Tables/InformationAsymmetry.tex", overwrite = TRUE)
file.copy(from = "Manually Created Tables/information_asymmetry.tex", to = "Paper/Tables/Table S1.tex", overwrite = TRUE)

# Tourism employment concentration tabular
file.copy(from = "Main Figures and Tables/specialization.tex", to   = "Paper/Tables/Specialization.tex", overwrite = T)

# Main Table 4: tourism employment exposure (tabular only)
file.copy(from = "Main Figures and Tables/specialization.tex", to = "Paper/Tables/Table 4.tex", overwrite = TRUE)

# Supplemental placebo counterfactual analysis
file.copy(from = "Main Figures and Tables/placebo_impact_table.tex", to = "Paper/Tables/PlaceboChecks.tex", overwrite = TRUE)
file.copy(from = "Main Figures and Tables/placebo_impact_table.tex", to = "Paper/Tables/Table S2.tex", overwrite = TRUE)

# Table 5: Choice of Control Countries
file.copy(from = "Main Figures and Tables/choice_of_control_countries.tex", to   = "Paper/Tables/Table 5.tex", overwrite = T)
file.copy(from = "Main Figures and Tables/choice_of_control_countries.tex", to = "Paper/Tables/Table S3.tex", overwrite = TRUE)

# Table 6: Projected Change in Tourism From Different Coefficient Groups
file.copy(from = "Main Figures and Tables/choice_of_variables.tex", to   = "Paper/Tables/Table 6.tex", overwrite = T)
file.copy(from = "Main Figures and Tables/choice_of_variables.tex", to = "Paper/Tables/Table S4.tex", overwrite = TRUE)

# Table 7: Forecasted Impacts Without Time-Series
file.copy(from = "Main Figures and Tables/drop_time_series.tex", to   = "Paper/Tables/Table 7.tex", overwrite = T)
file.copy(from = "Main Figures and Tables/drop_time_series.tex", to = "Paper/Tables/Table S5.tex", overwrite = TRUE)

# Table 8: Two-Way F.E. Treatment Estimate
file.copy(from = "Main Figures and Tables/DID_table.tex", to   = "Paper/Tables/Table 8.tex", overwrite = T)
file.copy(from = "Main Figures and Tables/DID_table.tex", to = "Paper/Tables/Table S6.tex", overwrite = TRUE)

# Table 3: labor-market and wage effects
file.copy(from = "Main Figures and Tables/comprehensive_labor_impact_table.tex", to = "Paper/Tables/LaborImpact.tex", overwrite = T)
file.copy(from = "Main Figures and Tables/comprehensive_labor_impact_table.tex", to = "Paper/Tables/Table 3.tex", overwrite = TRUE)

# Additional appendix tables
file.copy(from = "Main Figures and Tables/tourism_activity_impact_table.tex", to = "Paper/Tables/TourismActivityImpact.tex", overwrite = T)
file.copy(from = "Main Figures and Tables/tourism_activity_impact_table.tex", to = "Paper/Tables/Table S7.tex", overwrite = TRUE)
file.copy(from = "Main Figures and Tables/demographic_labor_impact_table.tex", to = "Paper/Tables/DemographicLaborImpact.tex", overwrite = T)


# Figures

# Main Figure 2: nationality impacts, 95% intervals, and WOW Air direct routes
copy_image_as_jpg(from = "Additional Tables and Figures/Passengers by Nationality/nationality_route_effect_comparison.png", to = "Paper/Figures/Figure 2.jpg")

# Main Figure 3: monthly passenger departures relative to the counterfactual
copy_image_as_jpg(from = "Main Figures and Tables/arrivals_by_month.png", to = "Paper/Figures/Figure 3.jpg")

# Main Figure 4: stacked monthly flights by airline, 2017--2019
copy_image_as_jpg(from = "Additional Tables and Figures/BTS_T100_TO_ICELAND_monthly_flights_stacked_2017_2019.png", to = "Paper/Figures/Figure 4.jpg")


# Main Figure 5: nine-month relative labor impacts and 95% intervals
copy_image_as_jpg(from = "Main Figures and Tables/labor_relative_impact_95ci.png", to = "Paper/Figures/Figure 5.jpg")

# Supplemental Figures S1--S3: data coverage and model diagnostics
copy_image_as_jpg(from = "Main Figures and Tables/recent_information_bias.png", to = "Paper/Figures/Figure S1.jpg")
copy_image_as_jpg(from = "Main Figures and Tables/time_callibration.png", to = "Paper/Figures/Figure S2.jpg")
copy_image_as_jpg(from = "Main Figures and Tables/coefficients_arrivals_plot.png", to = "Paper/Figures/Figure S3.jpg")

# Supplemental Figures S4--S5: no-time-series sensitivity plots
copy_image_as_jpg(from = "Main Figures and Tables/dropped_time_series_differences.png", to = "Paper/Figures/Figure S4.jpg")
copy_image_as_jpg(from = "Main Figures and Tables/dropped_time_series_cumul_difs.png", to = "Paper/Figures/Figure S5.jpg")

# Supplemental Figures S6--S7: additional unemployment and labor outcomes
copy_image_as_jpg(from = "Main Figures and Tables/national_labor_plot.png", to = "Paper/Figures/Figure S6.jpg")
copy_image_as_jpg(from = "Main Figures and Tables/sectoral_labor_impact_plot.png", to = "Paper/Figures/Figure S7.jpg")

# Main manuscript figure exports with semantic names
copy_image_as_jpg(from = "Main Figures and Tables/primary_arrivals_plot.png", to = "Paper/Figures/PassengerDeparturesCounterfactual.jpg")
copy_image_as_jpg(from = "Main Figures and Tables/arrivals_by_month.png", to = "Paper/Figures/MonthlyPassengerDepartures.jpg")
# copy_image_as_jpg(from = "Main Figures and Tables/sectoral_labor_impact_plot.png", to = "Paper/Figures/MonthlyLaborDifferences.jpg")

# Main Figure 1: observed and counterfactual international passenger departures
copy_image_as_jpg(from = "Main Figures and Tables/primary_arrivals_plot.png", to = "Paper/Figures/Figure 1.jpg")

# Former Figure 5 composite is no longer used; its passenger breakdown is in Table 2.
# copy_image_as_jpg(from = "Main Figures and Tables/passengers_by_nationality.png", to = "Paper/Figures/Figure 5a.jpg")
# file.copy(from = "Main Figures and Tables/passenger_nationality.tex", to = "Paper/Figures/Figure 5b.tex", overwrite = TRUE)
# combine_latex_table_and_image_as_jpg(
#   table_tex = "Main Figures and Tables/passenger_nationality.tex",
#   image = "Main Figures and Tables/passengers_by_nationality.png",
#   to = "Paper/Figures/Figure 5.jpg",
#   table_label = "(b) Passenger Summary by Nationality",
#   image_label = "(a) Monthly Difference in Observed and Estimated Departures by Nationality",
#   image_note = "Note: Difference between observed and estimated counterfactual passenger departures by nationality. Passengers represent departures through international security.",
#   table_note = "Note: Data show observed vs. estimated counterfactual passenger departures by nationality.",
#   table_first = FALSE
# )

# Former main labor-level figure is now Supplemental Figure S7 above.

# Move any .png or .tex files from folder a to folder B: 
from <- getwd()   # current working directory
to   <- file.path(getwd(), "Additional Tables and Figures")

# create destination if it doesn't exist
dir.create(to, showWarnings = FALSE)

image_files <- list.files(from, pattern = "\\.(png|jpg|jpeg)$", full.names = TRUE, ignore.case = TRUE)
text_files <- list.files(from, pattern = "\\.(tex|md)$", full.names = TRUE, ignore.case = TRUE)
text_files <- text_files[basename(text_files) != "WOWAIR_organized_manuscript.tex"]

invisible(lapply(image_files, function(path) {
  out <- file.path(to, paste0(tools::file_path_sans_ext(basename(path)), ".jpg"))
  copy_image_as_jpg(path, out)
}))

file.rename(text_files, file.path(to, basename(text_files)))

