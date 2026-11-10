# BTS T-100 monthly airline traffic to and from Iceland (KEF)
#
# This script is designed to be sourced from the WOWAIR project Master.R.
# It creates TO_ICELAND in the calling environment and saves several flight and
# passenger graphics in "Additional Tables and Figures" for the five largest
# airlines by passenger volume.

suppressPackageStartupMessages(library(ggplot2))

# Minimum average seats per performed flight. This excludes cargo, private, and
# small-aircraft operations from the flight count. Change this value if desired.
minimum_seats_per_flight <- 50

# Locate the project whether this file is sourced from the project root or run
# directly from the Scripts directory.
project_candidates <- unique(normalizePath(
  c(getwd(), file.path(getwd(), "..")),
  winslash = "/",
  mustWork = FALSE
))
project_root <- project_candidates[
  dir.exists(file.path(project_candidates, "Data", "Raw Data", "BTS_T100"))
][1]

if (is.na(project_root)) {
  stop("Could not locate Data/Raw Data/BTS_T100 from the current working directory.")
}

input_dir <- file.path(project_root, "Data", "Raw Data", "BTS_T100")
plot_dir <- file.path(project_root, "Additional Tables and Figures")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

annual_files <- list.files(
  input_dir,
  pattern = "^[0-9]{4}\\.csv$",
  full.names = TRUE
)

if (length(annual_files) == 0L) {
  stop("No annual CSV files were found in ", input_dir)
}

required_columns <- c(
  "DEPARTURES_PERFORMED", "SEATS", "PASSENGERS",
  "UNIQUE_CARRIER", "UNIQUE_CARRIER_NAME",
  "ORIGIN", "DEST", "YEAR", "MONTH"
)

read_t100 <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  missing_columns <- setdiff(required_columns, names(x))
  if (length(missing_columns) > 0L) {
    stop(basename(path), " is missing: ", paste(missing_columns, collapse = ", "))
  }
  x[trimws(x$DEST) == "KEF" | trimws(x$ORIGIN) == "KEF", required_columns]
}

t100_kef <- do.call(rbind, lapply(annual_files, read_t100))
rownames(t100_kef) <- NULL

# Standardize fields and ensure the measures are numeric before aggregation.
t100_kef$UNIQUE_CARRIER <- trimws(as.character(t100_kef$UNIQUE_CARRIER))
t100_kef$UNIQUE_CARRIER_NAME <- trimws(as.character(t100_kef$UNIQUE_CARRIER_NAME))
t100_kef$ORIGIN <- trimws(as.character(t100_kef$ORIGIN))
t100_kef$DEST <- trimws(as.character(t100_kef$DEST))
t100_kef$YEAR <- as.integer(t100_kef$YEAR)
t100_kef$MONTH <- as.integer(t100_kef$MONTH)

measure_columns <- c("DEPARTURES_PERFORMED", "SEATS", "PASSENGERS")
t100_kef[measure_columns] <- lapply(t100_kef[measure_columns], as.numeric)
t100_kef$date <- as.Date(sprintf("%04d-%02d-01", t100_kef$YEAR, t100_kef$MONTH))
t100_kef$PASSENGER_FLIGHTS <- ifelse(
  t100_kef$DEPARTURES_PERFORMED > 0 &
    (t100_kef$SEATS / t100_kef$DEPARTURES_PERFORMED) > minimum_seats_per_flight,
  t100_kef$DEPARTURES_PERFORMED,
  0
)

# Map each carrier code to its newest available nonblank carrier name. The final
# source-row order breaks ties if more than one name appears in the newest month.
valid_names <- !is.na(t100_kef$UNIQUE_CARRIER_NAME) & t100_kef$UNIQUE_CARRIER_NAME != ""
name_rows <- t100_kef[valid_names, c("UNIQUE_CARRIER", "UNIQUE_CARRIER_NAME", "date")]
name_rows$source_order <- seq_len(nrow(name_rows))
name_rows <- name_rows[order(name_rows$date, name_rows$source_order), ]
newest_names <- name_rows[!duplicated(name_rows$UNIQUE_CARRIER, fromLast = TRUE), ]
newest_names <- newest_names[c("UNIQUE_CARRIER", "UNIQUE_CARRIER_NAME")]

aggregate_direction <- function(x, direction) {
  if (direction == "TO_ICELAND") {
    x <- x[x$DEST == "KEF", ]
  } else if (direction == "FROM_ICELAND") {
    x <- x[x$ORIGIN == "KEF", ]
  } else {
    stop("Unknown direction: ", direction)
  }

  out <- aggregate(
    x[c("DEPARTURES_PERFORMED", "PASSENGER_FLIGHTS", "SEATS", "PASSENGERS")],
    by = list(date = x$date, UNIQUE_CARRIER = x$UNIQUE_CARRIER),
    FUN = sum,
    na.rm = TRUE
  )
  names(out)[names(out) == "DEPARTURES_PERFORMED"] <- "ALL_FLIGHTS"
  names(out)[names(out) == "PASSENGER_FLIGHTS"] <- "FLIGHTS"
  out <- merge(out, newest_names, by = "UNIQUE_CARRIER", all.x = TRUE, sort = FALSE)
  out <- out[c("date", "UNIQUE_CARRIER", "UNIQUE_CARRIER_NAME",
               "FLIGHTS", "ALL_FLIGHTS", "SEATS", "PASSENGERS")]
  out[order(out$date, out$UNIQUE_CARRIER), ]
}

TO_ICELAND <- aggregate_direction(t100_kef, "TO_ICELAND")

# Plot monthly passenger and commercial-flight levels from January 2017 through
# December 2019 for the five largest airlines by total passenger volume.
plot_start <- as.Date("2017-01-01")
plot_end <- as.Date("2019-12-01")

plot_data <- TO_ICELAND[
  TO_ICELAND$date >= plot_start & TO_ICELAND$date <= plot_end,
]

carrier_size <- aggregate(
  plot_data$PASSENGERS,
  by = list(UNIQUE_CARRIER = plot_data$UNIQUE_CARRIER),
  FUN = sum,
  na.rm = TRUE
)
names(carrier_size)[2] <- "passenger_size"
carrier_size <- carrier_size[order(-carrier_size$passenger_size,
                                   carrier_size$UNIQUE_CARRIER), ]
top_five_carriers <- head(carrier_size$UNIQUE_CARRIER, 5L)
plot_data <- plot_data[plot_data$UNIQUE_CARRIER %in% top_five_carriers, ]

common_airline_names <- c(
  WW = "WOW air",
  FI = "Icelandair",
  DL = "Delta",
  AA = "American",
  UA = "United"
)
plot_data$airline <- unname(common_airline_names[plot_data$UNIQUE_CARRIER])
missing_common_name <- is.na(plot_data$airline)
plot_data$airline[missing_common_name] <- plot_data$UNIQUE_CARRIER_NAME[missing_common_name]

# Show WOW air first, followed by the other carriers in descending size order.
legend_order <- c("WW", top_five_carriers[top_five_carriers != "WW"])
legend_order <- legend_order[legend_order %in% top_five_carriers]
plot_data$airline <- factor(
  plot_data$airline,
  levels = unname(common_airline_names[legend_order])
)

# Disabled earlier quarterly/table alternative. It is retained here only for
# easy reference and does not create an output file.
if (FALSE) {
# Quarterly flight totals for the chart and the table beneath it.
plot_data$year <- as.integer(format(plot_data$date, "%Y"))
plot_data$quarter <- ((as.integer(format(plot_data$date, "%m")) - 1L) %/% 3L) + 1L
plot_data$quarter_date <- as.Date(sprintf(
  "%04d-%02d-01",
  plot_data$year,
  1L + 3L * (plot_data$quarter - 1L)
))

quarterly_flights <- aggregate(
  plot_data$FLIGHTS,
  by = list(
    quarter_date = plot_data$quarter_date,
    UNIQUE_CARRIER = plot_data$UNIQUE_CARRIER,
    airline = plot_data$airline
  ),
  FUN = sum,
  na.rm = TRUE
)
names(quarterly_flights)[4] <- "FLIGHTS"

monthly_plot_above_table <- ggplot(
  plot_data,
  aes(x = date, y = FLIGHTS, color = airline, group = UNIQUE_CARRIER)
) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 1.4, na.rm = TRUE) +
  scale_x_date(
    date_breaks = "3 months",
    labels = function(x) paste0(
      format(x, "%Y"), " Q",
      ((as.integer(format(x, "%m")) - 1L) %/% 3L) + 1L
    ),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    name = "Flights",
    labels = scales::label_comma(),
    breaks = scales::breaks_pretty(n = 8),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(x = NULL, color = "Airline") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

quarterly_flights$quarter_label <- paste0(
  format(quarterly_flights$quarter_date, "%Y"),
  " Q",
  ((as.integer(format(quarterly_flights$quarter_date, "%m")) - 1L) %/% 3L) + 1L
)
quarter_table <- reshape(
  quarterly_flights[c("airline", "quarter_label", "FLIGHTS")],
  idvar = "airline",
  timevar = "quarter_label",
  direction = "wide"
)
names(quarter_table) <- sub("^FLIGHTS\\.", "", names(quarter_table))
quarter_columns <- as.vector(t(outer(2017:2019, paste0("Q", 1:4), paste, sep = " ")))
missing_quarters <- setdiff(quarter_columns, names(quarter_table))
quarter_table[missing_quarters] <- 0
quarter_table <- quarter_table[c("airline", quarter_columns)]
names(quarter_table)[1] <- "Airline"
quarter_table <- quarter_table[match(levels(plot_data$airline), quarter_table$Airline), ]
quarter_table[is.na(quarter_table)] <- 0

quarter_table_grob <- gridExtra::tableGrob(
  quarter_table,
  rows = NULL,
  theme = gridExtra::ttheme_minimal(
    base_size = 8,
    core = list(bg_params = list(fill = rep(c("white", "#F4F4F4"),
                                               length.out = nrow(quarter_table)), col = NA)),
    colhead = list(
      fg_params = list(fontface = "bold"),
      bg_params = list(fill = "#E6E6E6", col = NA)
    )
  )
)
# Force the quarter cells to span the figure rather than collapsing into a
# narrow table. The slightly wider first column accommodates airline names;
# the remaining equal-width cells approximately align with chart quarters.
quarter_table_grob$widths <- grid::unit(c(1.4, rep(1, 12)), "null")
quarterly_with_table <- gridExtra::arrangeGrob(
  monthly_plot_above_table,
  quarter_table_grob,
  ncol = 1,
  heights = c(0.72, 0.28)
)

ggsave(
  file.path(plot_dir, "BTS_T100_TO_ICELAND_quarterly_flights_with_table_2017_2019.png"),
  quarterly_with_table,
  width = 16,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)
}

# Monthly flight-only version for 2018-2019 with denser axes and a value label
# at each positive observation.
label_offsets <- c("WOW air" = -0.9, "Icelandair" = -0.9, "Delta" = -0.9,
                   "American" = 1.5, "United" = 1.5)
plot_data$label_vjust <- unname(label_offsets[as.character(plot_data$airline)])
labeled_plot_start <- as.Date("2018-01-01")
labeled_plot_data <- plot_data[
  plot_data$date >= labeled_plot_start & plot_data$date <= plot_end,
]

monthly_flights_labeled_plot <- ggplot(
  labeled_plot_data,
  aes(x = date, y = FLIGHTS, color = airline, group = UNIQUE_CARRIER)
) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  geom_text(
    aes(label = ifelse(FLIGHTS > 0, format(FLIGHTS, big.mark = ","), ""),
        vjust = label_vjust),
    size = 4.2,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  scale_x_date(
    limits = c(labeled_plot_start, plot_end),
    date_breaks = "2 months",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    name = "Flights",
    labels = scales::label_comma(),
    breaks = scales::breaks_pretty(n = 12),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(x = NULL, color = "Airline") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(plot_dir, "BTS_T100_TO_ICELAND_monthly_flights_labeled_2018_2019.png"),
  monthly_flights_labeled_plot,
  width = 18,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Matching passenger-count chart. Values and axis labels are expressed in
# thousands of passengers.
monthly_passengers_labeled_plot <- ggplot(
  labeled_plot_data,
  aes(x = date, y = PASSENGERS / 1000, color = airline, group = UNIQUE_CARRIER)
) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  geom_text(
    aes(
      label = ifelse(PASSENGERS > 0, sprintf("%.1f", PASSENGERS / 1000), ""),
      vjust = label_vjust
    ),
    size = 4.2,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  scale_x_date(
    limits = c(labeled_plot_start, plot_end),
    date_breaks = "2 months",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    name = "Passengers (thousands)",
    labels = scales::label_number(accuracy = 1),
    breaks = scales::breaks_pretty(n = 12),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(x = NULL, color = "Airline") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(plot_dir, "BTS_T100_TO_ICELAND_monthly_passengers_labeled_thousands_2018_2019.png"),
  monthly_passengers_labeled_plot,
  width = 18,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Monthly stacked bars for commercial passenger flights only. Because
# WOW air is the first airline factor level and ggplot stacks in reverse factor
# order by default, WOW air is drawn at the top of every stack.
stacked_plot_data <- plot_data

monthly_stacked_plot <- ggplot(
  stacked_plot_data,
  aes(x = date, y = FLIGHTS, fill = airline)
) +
  geom_col(width = 25, position = position_stack(reverse = FALSE), na.rm = TRUE) +
  scale_x_date(
    limits = c(plot_start, plot_end),
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_comma(),
    breaks = scales::breaks_pretty(n = 8),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = "Flights", fill = "Airline") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(plot_dir, "BTS_T100_TO_ICELAND_monthly_flights_stacked_2017_2019.png"),
  monthly_stacked_plot,
  width = 14,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

message("Created TO_ICELAND and saved exactly three plots: labeled flights, labeled passengers, and stacked flights to ", plot_dir)
