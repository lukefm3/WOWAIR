complete <- month[,c(tourism_list)]

complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)


temp<- ts(data = complete[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))


primary_arrivals_plot <- plot(impact_temp) +
  ggtitle("Icelandic Airline Departures Post WOW Air Bankruptcy") +
  theme(
    plot.title = element_text(size = 12),
    plot.margin = margin(t = 10, r = 60, b = 10, l = 10)
  )
ggsave(
  "Main Figures and Tables/primary_arrivals_plot.png",
  plot = primary_arrivals_plot,
  width = 9,
  height = 6,
  units = "in",
  dpi = 500
)

# Create the plot
png("Main Figures and Tables/coefficients_arrivals_plot.png", width = 2600, height = 900)
par(
  mfrow = c(1, 2),
  oma = c(1, 3, 1, 4)
)
coeff_plot(impact_temp, name_cex = 1.15, title_cex = 1.25)
dev.off()


# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)        # gpar, unit
library(patchwork)
library(scales)

# --- Prepare the data -------------------------------------------------------
temp <- as.data.frame(impact_temp$series)

# Dates in rownames like "Apr 2019" -> first-of-month Date
temp$date <- as.Date(paste0("01 ", rownames(temp)), format = "%d %b %Y")

# Keep only the last 9 rows
temp <- dplyr::slice_tail(temp, n = 9)

temp <- temp %>%
  mutate(
    effect_pct        = (point.effect / point.pred) * 100,
    effect_lower_pct  = (point.effect.lower / point.pred) * 100,
    effect_upper_pct  = (point.effect.upper / point.pred) * 100
  )

# --- Top plot: Point Effect with CI ----------------------------------------
p_top <- ggplot(temp, aes(x = date, y = effect_pct)) +
  geom_ribbon(aes(ymin = effect_lower_pct, ymax = effect_upper_pct), alpha = 0.2) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_date(breaks = temp$date, date_labels = "%b-%y", expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  labs(
    x = NULL,
    y = "Relative Difference",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# --- Bottom table: Actuals, Predicted, Effect -------------------------------
# Helper to format numbers as thousands with 1 decimal (e.g., 12345 -> "12.3k")
format_k <- function(x) sprintf("%.1fk", x / 1000)

temp_tbl <- temp %>%
  mutate(month = format(date, "%b-%y")) %>%
  select(month, response, point.pred, point.effect) %>%
  rename(
    Actual    = response,
    Control = point.pred,
    Difference    = point.effect
  )

# Transpose so rows = Metric and columns = months, formatting as k
tab_transposed <- temp_tbl %>%
  pivot_longer(-month, names_to = "Metric", values_to = "Value") %>%
  mutate(Value = format_k(Value)) %>%
  pivot_wider(names_from = month, values_from = Value) %>%
  arrange(factor(Metric, levels = c("Actual", "Control", "Difference")))

# Compact table styling; same look, just tighter
tt <- gridExtra::ttheme_default(
  base_size = 9,
  core = list(
    fg_params = list(fontsize = 9),
    padding = unit(c(1, 2), "mm")
  ),
  colhead = list(
    fg_params = list(fontsize = 9, fontface = 2),
    padding = unit(c(1, 2), "mm")
  ),
  rowhead = list(
    fg_params = list(fontsize = 9, fontface = 2),
    padding = unit(c(1, 2), "mm")
  )
)

tbl_grob <- tableGrob(tab_transposed, rows = NULL, theme = tt)

# Keep columns evenly sized to avoid width blowout
tbl_grob$widths <- lapply(tbl_grob$widths, function(x) unit(1.3, "null"))

# --- Combine ---------------------------------------------------------------
final_plot <- p_top / patchwork::wrap_elements(tbl_grob) + plot_layout(heights = c(5, 1.6))

# Display
final_plot
ggsave("Main Figures and Tables/arrivals_by_month.png", plot = final_plot, width = 8, height = 6, dpi = 500)
