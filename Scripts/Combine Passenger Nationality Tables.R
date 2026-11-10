# Combine the overall passenger rows with the individual-nationality results.
# Source this after Passengers by Nationality.R has written its impact table.
summary_path <- file.path("Main Figures and Tables", "passenger_nationality.tex")
nationality_path <- file.path(
  "Additional Tables and Figures", "Passengers by Nationality",
  "nationality_impact_table.tex"
)
output_path <- file.path("Main Figures and Tables", "passenger_nationality_combined.tex")

read_table_rows <- function(path, expected_columns) {
  lines <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
  lines <- lines[grepl(" & ", lines, fixed = TRUE)]
  rows <- lapply(lines, function(line) {
    line <- sub("[[:space:]]*\\\\\\\\[[:space:]]*$", "", line)
    trimws(strsplit(line, " & ", fixed = TRUE)[[1L]])
  })
  rows <- Filter(function(row) length(row) == expected_columns, rows)
  if (length(rows) < 2L) stop("Could not read table rows from: ", path)
  rows[-1L]
}

summary_rows <- read_table_rows(summary_path, 7L)
summary_names <- vapply(summary_rows, `[[`, character(1), 1L)
required <- c("Total Passengers", "Icelandic Passengers", "Foreign Passengers")
if (!all(required %in% summary_names)) stop("The passenger summary is missing a required row.")
summary_rows <- summary_rows[match(required, summary_names)]

nationality_rows <- read_table_rows(nationality_path, 9L)
names <- vapply(nationality_rows, `[[`, character(1), 1L)
exclude <- c("European Union", "North America", "Other (excluding Israel)", "Other Countries")
nationality_rows <- nationality_rows[!(names %in% exclude)]
if (!length(nationality_rows)) stop("No individual-nationality rows were found.")

# Order by the magnitude of the relative impact: United States, Canada, Sweden, ...
impact <- vapply(nationality_rows, function(row) {
  as.numeric(gsub("[^0-9.+-]", "", row[3L]))
}, numeric(1))
if (anyNA(impact)) stop("A nationality has an invalid relative impact.")
nationality_rows <- nationality_rows[order(-abs(impact))]

# Keep the columns and their order from passenger_nationality.tex.
nationality_rows <- lapply(nationality_rows, function(row) row[c(1L, 3L, 4L, 6L, 5L, 8L, 9L)])
latex_row <- function(row) paste0(paste(row, collapse = " & "), " \\\\")
output <- c(
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Passenger Series & Relative Impact (\\%) & 9-Month Cumulative Impact & 95\\% CI Upper & 95\\% CI Lower & p-value & RMSE (pre-period) \\\\",
  "\\midrule",
  latex_row(summary_rows[[1L]]),
  "\\midrule",
  latex_row(summary_rows[[2L]]),
  latex_row(summary_rows[[3L]]),
  "\\midrule",
  vapply(nationality_rows, latex_row, character(1)),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(output, output_path, useBytes = TRUE)
cat("Wrote", output_path, "with", length(nationality_rows), "individual-nationality rows.\n")
