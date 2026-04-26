# Model Calibration
complete <- month[,c(tourism_list)]

loop <- month[,c(tourism_list)]
colnames(month[,c(tourism_list)])
cumul_dif_plot <- data.frame(a=1:12)
dif_plot <- data.frame(a=1:12)

# Create cumulative dif DF / Straight Differences

# First days of years
year_seq <- seq(as.Date("2000-1-1"), as.Date("2018-1-1"), "years")

### 
# To make visualizing easier and creating a ts plot easier, make df only 2017 to 2020
# that way I can us cbind
###

# OR, work with as a TS, pair by dates
for(i in 1:length(year_seq)){
  year_loop <- year_seq[i]  
  complete <- month[,c(tourism_list)]
  complete2 <- as.data.frame(complete[,3])
  colnames(complete2) <- "arrivals_iceland"
  icelandcolumns <- grep('iceland', colnames(complete))
  
  complete <- complete[,-c(icelandcolumns)]
  
  complete <- cbind(complete,complete2)
  
  
  complete <- complete %>% filter(date >= year_loop) %>% filter(date < '2019-12-31') # to do arrivals 
  
  complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)
  
  
  impact_temp<- ts(data = complete[-c(1)],
                   start = as.numeric(year(year_loop)),
                   end = 2020,
                   frequency = 12) %>% head(-1)
  
  pre.period <- as.yearmon(c(year_loop, "2019-01-01"))
  
  post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))
  
  
  impact_temp <- CausalImpact(impact_temp,pre.period, post.period,model.args=list(niter=5000, nseasons = 12, season.duration = 1))
  
  
  temp <- as.data.frame(impact_temp$series)
  
  
  temp$dif <- temp$response - temp$point.pred
  colnames(temp)[15] <- as.character(year(year_loop))
  temp$cum.dif <- temp$cum.response - temp$cum.pred
  colnames(temp)[16] <- as.character(year(year_loop))
  temp <- temp[c((length(rownames(temp))-11):length(rownames(temp))),]
  
  # Assign DF values
  dif_plot <- cbind(as.data.frame(dif_plot),temp[,15])
  #assign DF Values
  colnames(dif_plot)[i+1] <- as.character(year(year_loop))
  
  cumul_dif_plot <- cbind(as.data.frame(cumul_dif_plot),temp[,16])
  #assign DF Values
  colnames(cumul_dif_plot)[i+1] <- as.character(year(year_loop))
  
}

dif_plot <- dif_plot[c(4:9),-1]
cumul_dif_plot <- cumul_dif_plot[,-1]

dif_plot2 <- ts(data = dif_plot, start=2019.25, end=2020, frequency=12) %>% head(-4)

cumul_dif_plot2 <- ts(data = cumul_dif_plot, start=2019, end=2020, frequency=12) %>% head(-1)


library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(zoo)

# -----------------------------
# POINT EFFECTS PLOT
# -----------------------------
dif_df <- dif_plot2 %>%
  as.data.frame() %>%
  mutate(date = seq(as.Date("2019-04-01"), by = "month", length.out = n())) %>%
  pivot_longer(-date, names_to = "PrePeriodStart", values_to = "value")

plot <- ggplot(dif_df, aes(x = date, y = value, color = PrePeriodStart)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2019-04-01"), linetype = "dashed") +
  annotate(
    "text",
    x = as.Date("2019-05-01"),
    y = min(dif_df$value, na.rm = TRUE) * 0.92,
    label = "WOW Air Closure",
    color = "black",
    hjust = 0
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m/%Y",
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.08, 0.15))
  ) +
  labs(
    x = "Month/Year",
    y = "Departures From Iceland",
    color = "Pre-Treatment Phase Starts In"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    aspect.ratio = 0.6,
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("Figure 2.png", plot = plot, width = 9, height = 5.5, dpi = 500)

# Mean average difference across all 9 months
dif_mean <- mean(dif_plot2[,c(1:18)],na.rm=T)
dif_mean <- mean(dif_mean, na.rm=T)

print(paste("Average Monthly Impact Across ALL Starting Periods was", dif_mean))

dif_mean2 <- mean(cumul_dif_plot2[12,c(1:18)], na.rm=T)
dif_median <- median(cumul_dif_plot2[12,c(1:18)], na.rm=T)

print(paste("Average Cumulative Impact Across All Starting Periods was", round(dif_mean2,0)))

print(paste("Median Cumulative Impact Across All Starting Periods was", round(dif_median,0)))



library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(zoo)

# -----------------------------
# POINT EFFECTS PLOT
# -----------------------------
dif_df <- dif_plot2 %>%
  as.data.frame() %>%
  mutate(date = seq(as.Date("2019-04-01"), by = "month", length.out = n())) %>%
  pivot_longer(-date, names_to = "PrePeriodStart", values_to = "value")

plot <- ggplot(dif_df, aes(x = date, y = value, color = PrePeriodStart)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2019-04-01"), linetype = "dashed") +
  annotate(
    "text",
    x = as.Date("2019-05-01"),
    y = min(dif_df$value, na.rm = TRUE) * 0.92,
    label = "WOW Air Closure",
    color = "black",
    hjust = 0
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m/%Y",
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.08, 0.15))
  ) +
  labs(
    x = "Month/Year",
    y = "Departures From Iceland",
    color = "Pre-Treatment Phase Starts In"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    aspect.ratio = 0.6,
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("Figure 2.png", plot = plot, width = 9, height = 5.5, dpi = 500)


# -----------------------------
# CUMULATIVE EFFECTS PLOT
# -----------------------------
cumul_df <- cumul_dif_plot2 %>%
  as.data.frame() %>%
  mutate(date = seq(as.Date("2019-01-01"), by = "month", length.out = n())) %>%
  pivot_longer(-date, names_to = "PrePeriodStart", values_to = "value")

plot <- ggplot(cumul_df, aes(x = date, y = value, color = PrePeriodStart)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2019-03-01"), linetype = "dashed") +
  annotate(
    "text",
    x = as.Date("2019-04-01"),   # ← move LEFT
    y = min(cumul_df$value, na.rm = TRUE) * 0.85,
    label = "- WOW Air Closure",
    hjust = .3                    # anchor text to the right → pushes text left
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%Y/%m",
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.08, 0.15))
  ) +
  labs(
    x = "Year/Month",
    y = "Airline Passengers",
    color = "Pre-Treatment Phase Starts In"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 12)),
    axis.title.x = element_text(margin = margin(t = 12)),  # ← THIS LINE
    
    legend.position = "right",
    legend.box.margin = margin(t = 40),
    plot.margin = margin(t = 10, r = 5, b = 10, l = 25)
  )+ 
  theme(
    legend.position = "right",
    legend.box.margin = margin(t = 30),  # ← pushes legend DOWN
    legend.title = element_text(hjust = 0.5)
  ) + theme(
    plot.margin = margin(
      t = 10,
      r = 5,     # ← smaller right margin
      b = 10,
      l = 25     # ← larger left margin
    )
  )
ggsave("Main Figures and Tables/time_callibration.png", plot = plot, width = 9, height = 5.5, dpi = 500)



#  Information by Time
info <- month[,-c(grep("iceland", colnames(month)))]
# Intentionally NA'd out a few columns That I need to Omit
#omitted <- sum(colSums(is.na(month)) == nrow(month))
#temp_denom <- (length(colnames(info))-1-omitted)
#info$na_count <- rowSums(is.na(info[,-1]))-omitted
#info <- info %>% select(date, na_count)
#info$percent <- 1-(info$na_count/(temp_denom-1))
#info <- info %>%  filter(date < '2019-12-31') %>% filter(date>='2000-01-01')

#png("recent_information_bias.png", width = 800, height = 600)
#plot(info$date, info$percent, type = "l", xlab = "Date", ylab = "Percent of Covariate Fields without NAs", main = "Recent Information Bias", xaxt = "n")
#ticks <- seq(as.Date("2000-01-01"), as.Date("2019-12-31"), by = "2 years")  # Create a sequence of every other year
#axis(1, at = as.numeric(ticks), labels = format(ticks, "%Y"), cex.axis = 0.8, las = 2)




# Information by Time
info <- month[, -c(grep("iceland", colnames(month)))]

# Intentionally NA'd out a few columns that need to be omitted
omitted <- sum(colSums(is.na(month)) == nrow(month))
temp_denom <- (length(colnames(info)) - 1 - omitted)

info$na_count <- rowSums(is.na(info[, -1])) - omitted
info <- info %>%
  select(date, na_count) %>%
  mutate(percent = 1 - (na_count / (temp_denom - 1))) %>%
  filter(date < as.Date("2019-12-31"),
         date >= as.Date("2000-01-01"))

# keep proportions cleaner
png("Main Figures and Tables/recent_information_bias.png", width = 1800, height = 1200, res = 300)

par(
  mar = c(5, 5, 2, 1),   # bottom, left, top, right
  mgp = c(2.8, 0.8, 0),  # axis title, tick labels, axis line
  bty = "l",
  family = "serif"
)

plot(
  info$date,
  info$percent,
  type = "l",
  lwd = 2,
  col = "black",
  xlab = "Date",
  ylab = "Percent of Covariate Fields\nwithout Missing Values",
  main = "",                         # no title/caption on saved figure
  xaxt = "n",
  yaxt = "n"
)

# x-axis ticks every 2 years
ticks_x <- seq(as.Date("2000-01-01"), as.Date("2019-12-31"), by = "2 years")
axis(
  1,
  at = ticks_x,
  labels = format(ticks_x, "%Y"),
  cex.axis = 0.9,
  las = 2
)

# y-axis as actual percents
ticks_y <- pretty(info$percent, n = 6)
axis(
  2,
  at = ticks_y,
  labels = percent(ticks_y, accuracy = 1),
  cex.axis = 0.9,
  las = 1
)

# light grid
abline(h = ticks_y, col = "grey85", lty = 1)
abline(v = ticks_x, col = "grey92", lty = 1)

# redraw line on top of grid
lines(info$date, info$percent, lwd = 2, col = "black")

dev.off()





library(readxl)
library(dplyr)
library(stringr)

raw <- read_excel("Data/Summary Table.xlsx", sheet = "Table", col_names = TRUE)

country_cols <- c(
  "Iceland", "Faroe Islands", "Greenland", "Norway",
  "Denmark", "Sweden", "Ireland", "UK"
)

# Keep only the rows needed for the final table
final <- raw %>%
  filter(Indicator %in% c(
    "Nordic Countries",
    "Distance to Iceland (mi.)",
    "Seasonality (Difference Between Longest & Shortest Day)",
    "Population 2018 (million)",
    "Annual Tourists per Person (2013)",
    "Annual Tourists per Person (2018)",
    "5 Year Change in Tourism Per Person",
    "Fish, Animals, and Animal Products as Export Product Share (2009)"
  )) %>%
  select(Indicator, all_of(country_cols))

# Rename row labels to exactly match your target
final <- final %>%
  mutate(
    Indicator = recode(
      Indicator,
      "Nordic Countries" = "Nordic Country",
      "Annual Tourists per Person (2013)" = "Annual Tourists per Capita (2013)",
      "Annual Tourists per Person (2018)" = "Annual Tourists per Capita (2018)",
      "5 Year Change in Tourism Per Person" = "5 Year Change in Tourism Per Capita"
    )
  )

# Put rows in the exact order you want
final$Indicator <- factor(
  final$Indicator,
  levels = c(
    "Nordic Country",
    "Distance to Iceland (mi.)",
    "Seasonality (Difference Between Longest & Shortest Day)",
    "Population 2018 (million)",
    "Annual Tourists per Capita (2013)",
    "Annual Tourists per Capita (2018)",
    "5 Year Change in Tourism Per Capita",
    "Fish, Animals, and Animal Products as Export Product Share (2009)"
  )
)

final <- final %>% arrange(Indicator)

# Helper for Excel time fractions -> h:mm
fmt_daylength <- function(x) {
  x <- as.numeric(x)
  mins <- round(x * 24 * 60)
  h <- mins %/% 60
  m <- mins %% 60
  paste0(h, ":", str_pad(m, 2, pad = "0"))
}

# Convert everything to character first
final <- final %>%
  mutate(across(all_of(country_cols), as.character))

# Row-by-row formatting
final[final$Indicator == "Nordic Country", country_cols] <-
  lapply(final[final$Indicator == "Nordic Country", country_cols], function(x) {
    x[is.na(x) | x == "NA"] <- "-"
    x
  })

# Force Ireland and UK to "-"
final[final$Indicator == "Nordic Country", c("Ireland", "UK")] <- list("-", "-")

final[final$Indicator == "Distance to Iceland (mi.)", country_cols] <-
  lapply(final[final$Indicator == "Distance to Iceland (mi.)", country_cols], function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    out <- ifelse(is.na(x_num), "-", as.character(round(x_num, 0)))
    out
  })

final[final$Indicator == "Seasonality (Difference Between Longest & Shortest Day)", country_cols] <-
  lapply(final[final$Indicator == "Seasonality (Difference Between Longest & Shortest Day)", country_cols], fmt_daylength)

for (r in c(
  "Population 2018 (million)",
  "Annual Tourists per Capita (2013)",
  "Annual Tourists per Capita (2018)"
)) {
  final[final$Indicator == r, country_cols] <-
    lapply(final[final$Indicator == r, country_cols], function(x) {
      formatC(as.numeric(x), format = "f", digits = 2)
    })
}

final[final$Indicator == "5 Year Change in Tourism Per Capita", country_cols] <-
  lapply(final[final$Indicator == "5 Year Change in Tourism Per Capita", country_cols], function(x) {
    paste0(round(as.numeric(x) * 100), "\\%")
  })

final[final$Indicator == "Fish, Animals, and Animal Products as Export Product Share (2009)", country_cols] <-
  lapply(final[final$Indicator == "Fish, Animals, and Animal Products as Export Product Share (2009)", country_cols], function(x) {
    paste0(round(as.numeric(x) * 100), "\\%")
  })

# Escape ampersand in the row label for LaTeX
final$Indicator <- gsub("&", "\\\\&", final$Indicator)

# Build latex rows
body_rows <- apply(final, 1, function(x) {
  paste0("    ", paste(x, collapse = " & "), " \\\\")
})

body_text <- paste(body_rows, collapse = "\n")

latex_table <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Forecasted Change in Tourism From Different Coefficient Groups}\\label{tab:summary_table}\n",
  "\\resizebox{\\textwidth}{!}{%\n",
  "\\begin{tabular}{lcccccccc}\n",
  "    \\toprule\n",
  "          & \\textbf{Iceland} & \\textbf{Faroe Islands} & \\textbf{Greenland} & \\textbf{Norway} & \\textbf{Denmark} & \\textbf{Sweden} & \\textbf{Ireland} & \\textbf{UK} \\\\\n",
  "    \\midrule\n",
  body_text, "\n",
  "    \\bottomrule\n",
  "    \\end{tabular}%\n",
  "}\n",
  "\\begin{tablenotes}\\tiny[Source:]\n",
  "1. Globefeed.com, 2. US Naval Observatory Astronomical Applications Department, 3. Nordicstatistics.org, 4. World Bank\n",
  "\\end{tablenotes}\n",
  "\\end{table}\n"
)

cat(latex_table)
writeLines(latex_table, "Main Figures and Tables/summary_table.tex")

