
##
#Loading in the passenger nationality data (bind later)
##
monthly_passengers_nationality <- read_csv("Data/monthly_passengers_nationality.csv", 
                                           col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                                            passengers_austria_iceland = col_double(), 
                                                            passengers_australianewzealand_iceland = col_double(), 
                                                            passengers_belgium_iceland = col_double(), 
                                                            passengers_estonialatvialithuania_iceland = col_double(), 
                                                            passengers_hongkong_iceland = col_double(), 
                                                            passengers_india_iceland = col_double(), 
                                                            passengers_ireland_iceland = col_double(), 
                                                            passengers_israel_iceland = col_double(), 
                                                            passengers_china_iceland = col_double(), 
                                                            passengers_poland_iceland = col_double(), 
                                                            passengers_russia_iceland = col_double(), 
                                                            passengers_singapore_iceland = col_double(), 
                                                            passengers_southkorea_iceland = col_double(), 
                                                            passengers_taiwan_iceland = col_double()))


month2 <- cbind(
  month,
  monthly_passengers_nationality[, -1]
)

complete <- cbind(
  month[, c(tourism_list)],
  monthly_passengers_nationality[, -1]
)

# Model every nationality series supplied in the nationality file. The first
# three are regional aggregates; the remaining columns are countries/groups.
nationality_columns <- names(monthly_passengers_nationality)[-1]

output_dir <- file.path("Additional Tables and Figures", "Passengers by Nationality")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

post_start <- as.Date("2019-04-01")
post_end <- as.Date("2019-12-01")
latest_allowed_start <- as.Date("2016-12-01")

nationality_results <- list()
nationality_summaries <- list()
nationality_impacts <- list()
nationality_plot_data <- list()

# Number of city-level routes visible in the supplied 2018 route map. Spain's
# count includes Barcelona, Alicante, Tenerife, and Gran Canaria.
wow_route_count_2018 <- c(
  Austria = 1, `United States` = 12, Belgium = 1, `United Kingdom` = 2,
  Denmark = 1, France = 2, Netherlands = 1, Ireland = 1, Israel = 1,
  Italy = 1, Canada = 2, Poland = 1, Spain = 4, Sweden = 1, Germany = 4
)
direct_wow_route_2018 <- wow_route_count_2018 > 0

pretty_nationality <- function(x) {
  x <- sub("^passengers_", "", x)
  x <- sub("_iceland$", "", x)
  labels <- c(
    na = "North America",
    eu = "European Union",
    othnoisrael = "Other (excluding Israel)",
    australianewzealand = "Australia and New Zealand",
    estonialatvialithuania = "Estonia, Latvia, and Lithuania",
    usa = "United States",
    uk = "United Kingdom",
    hongkong = "Hong Kong",
    southkorea = "South Korea",
    othercountries = "Other Countries"
  )
  if (x %in% names(labels)) labels[[x]] else tools::toTitleCase(x)
}

safe_file_name <- function(x) {
  gsub("(^_|_$)", "", gsub("[^a-z0-9]+", "_", tolower(x)))
}

for (outcome_name in nationality_columns) {
  outcome_label <- pretty_nationality(outcome_name)

  # Blank, NA, and zero values do not establish the start of a series. Once
  # the series begins, however, a genuine zero is retained as an observation.
  availability <- complete %>%
    dplyr::transmute(date = as.Date(date), value = .data[[outcome_name]]) %>%
    dplyr::filter(
      date >= as.Date("2014-01-01"),
      date <= post_end,
      !is.na(value),
      value > 0
    )

  if (nrow(availability) == 0) {
    warning("Skipping ", outcome_label, ": no positive observations in 2014-2019.")
    next
  }

  series_start <- min(availability$date)
  if (series_start > latest_allowed_start) {
    warning(
      "Skipping ", outcome_label, ": its first usable observation is ",
      format(series_start, "%B %Y"), ", which is 2017 or later."
    )
    next
  }

  # Remove every Iceland outcome from the predictor pool, and then put only
  # the outcome selected by this iteration back as the first model column.
  predictor_names <- names(complete)[!grepl("iceland", names(complete), ignore.case = TRUE)]
  predictor_names <- setdiff(predictor_names, "date")

  model_data <- complete %>%
    dplyr::transmute(
      date = as.Date(date),
      response = .data[[outcome_name]],
      dplyr::across(dplyr::all_of(predictor_names))
    ) %>%
    dplyr::filter(date >= series_start, date <= post_end)

  if (anyNA(model_data$response)) {
    warning("Skipping ", outcome_label, ": missing outcome values remain after its start date.")
    next
  }

  # CausalImpact needs complete predictors. Constant predictors are also
  # discarded because they provide no information to the model.
  keep_predictor <- vapply(
    model_data[predictor_names],
    function(x) !anyNA(x) && length(unique(x)) > 1,
    logical(1)
  )
  model_data <- model_data[, c("date", "response", predictor_names[keep_predictor])]

  model_ts <- stats::ts(
    model_data[, setdiff(names(model_data), "date")],
    start = c(as.integer(format(series_start, "%Y")), as.integer(format(series_start, "%m"))),
    frequency = 12
  )

  pre.period <- zoo::as.yearmon(c(series_start, as.Date("2019-03-01")))
  post.period <- zoo::as.yearmon(c(post_start, post_end))

  impact_temp <- CausalImpact(
    model_ts,
    pre.period,
    post.period,
    model.args = list(niter = 5000, nseasons = 12, season.duration = 1)
  )

  nationality_impacts[[outcome_name]] <- impact_temp
  series_result <- as.data.frame(impact_temp$series)
  series_result$date <- model_data$date[seq_len(nrow(series_result))]

  post_result <- series_result %>%
    dplyr::filter(date >= post_start, date <= post_end) %>%
    dplyr::transmute(
      Nationality = outcome_label,
      Date = date,
      Actual = response,
      Projected = point.pred,
      `Projected Lower` = point.pred.lower,
      `Projected Upper` = point.pred.upper,
      Difference = point.effect,
      `Difference Percent` = 100 * point.effect / point.pred
    )

  nationality_results[[outcome_name]] <- post_result

  # Treat the full April-December cumulative effect as statistically
  # significant when its posterior interval excludes zero.
  final_post_row <- series_result %>%
    dplyr::filter(date == post_end) %>%
    dplyr::slice_tail(n = 1)
  is_significant <- nrow(final_post_row) == 1 && isTRUE(
    final_post_row$cum.effect.lower > 0 || final_post_row$cum.effect.upper < 0
  )
  has_direct_route <- isTRUE(unname(direct_wow_route_2018[outcome_label]))
  route_count <- ifelse(
    outcome_label %in% names(wow_route_count_2018),
    unname(wow_route_count_2018[outcome_label]),
    0
  )
  impact_p_value <- as.numeric(impact_temp$summary$p[1])
  pre_period_rows <- series_result$date < post_start
  pre_period_rmse <- sqrt(mean(
    (series_result$response[pre_period_rows] - series_result$point.pred[pre_period_rows])^2,
    na.rm = TRUE
  ))
  prior_year_actual <- complete %>%
    dplyr::transmute(date = as.Date(date), value = .data[[outcome_name]]) %>%
    dplyr::filter(date >= as.Date("2018-04-01"), date <= as.Date("2018-12-01")) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::pull(value)

  nationality_summaries[[outcome_name]] <- post_result %>%
    dplyr::summarise(
      Nationality = dplyr::first(Nationality),
      `Data Start` = series_start,
      `Significant Cumulative Effect` = is_significant,
      `Direct WOW Route (Provisional)` = has_direct_route,
      `Number of Direct Routes` = route_count,
      `Actual Total` = sum(Actual),
      `Prior Year Actual Total` = prior_year_actual,
      `Year-over-Year Percent` = 100 * (`Actual Total` - prior_year_actual) / prior_year_actual,
      `Projected Total` = sum(Projected),
      `Difference Total` = sum(Difference),
      `Difference Percent` = 100 * (`Actual Total` - `Projected Total`) / `Projected Total`,
      `Cumulative Effect Lower` = final_post_row$cum.effect.lower,
      `Cumulative Effect Upper` = final_post_row$cum.effect.upper,
      `Difference Lower Percent` = 100 * final_post_row$cum.effect.lower / sum(Projected),
      `Difference Upper Percent` = 100 * final_post_row$cum.effect.upper / sum(Projected),
      `p-value` = impact_p_value,
      `RMSE (pre-period)` = pre_period_rmse
    )

  # Store one actual pre-trend and both the actual and projected post-period.
  # No fitted pre-trend or confidence interval is included in the figure.
  actual_plot_data <- series_result %>%
    dplyr::filter(date >= max(as.Date("2017-01-01"), series_start)) %>%
    dplyr::transmute(Date = date, Passengers = response, Series = "Actual")
  projected_plot_data <- series_result %>%
    dplyr::filter(date >= post_start, date <= post_end) %>%
    dplyr::transmute(Date = date, Passengers = point.pred, Series = "Projected")

  nationality_plot_data[[outcome_name]] <- dplyr::bind_rows(
    actual_plot_data,
    projected_plot_data
  ) %>%
    dplyr::mutate(
      Nationality = outcome_label,
      Significance = ifelse(is_significant, "Significant", "Not significant"),
      `Direct WOW route` = ifelse(has_direct_route, "Direct route", "No direct route")
    )
}

# =============================================================================
# GRAPHICS-ONLY RESTART POINT
# =============================================================================
# The model loop above is slow. After one complete run, the fitted results and
# supporting objects are saved below. When revising only figures or tables in a
# new R session, first run the package/data setup at the top of the project,
# then run these two commented lines and continue from `if (length(...))` below:
#
# nationality_checkpoint <- readRDS(file.path("Additional Tables and Figures", "Passengers by Nationality", "nationality_model_checkpoint.rds"))
# list2env(nationality_checkpoint, envir = .GlobalEnv)
#
# If the objects are still present in the current R session, simply restart at
# the `if (length(nationality_results) == 0)` line below; no reload is needed.
nationality_checkpoint <- list(
  nationality_results = nationality_results,
  nationality_summaries = nationality_summaries,
  nationality_impacts = nationality_impacts,
  nationality_plot_data = nationality_plot_data,
  nationality_columns = nationality_columns,
  pretty_nationality = pretty_nationality,
  direct_wow_route_2018 = direct_wow_route_2018,
  wow_route_count_2018 = wow_route_count_2018,
  post_start = post_start,
  post_end = post_end,
  output_dir = output_dir
)
saveRDS(
  nationality_checkpoint,
  file.path(output_dir, "nationality_model_checkpoint.rds")
)

if (length(nationality_results) == 0) {
  warning("No nationality projections were produced, so no workbook was written.")
} else {
  monthly_results <- dplyr::bind_rows(nationality_results)
  summary_results <- dplyr::bind_rows(nationality_summaries)

  # CHECKPOINT-SAFE INTERVAL RECALCULATION ---------------------------------
  # Older checkpoints stored the point estimate with the Apr-Dec denominator
  # but stored interval percentages with a full-series cumulative denominator.
  # Recalculate both interval endpoints from the fitted models here so the dot
  # and its interval always occupy the same percentage axis, even when the
  # graphics section is rerun from an existing checkpoint.
  recalculated_intervals <- dplyr::bind_rows(lapply(
    names(nationality_impacts),
    function(outcome_name) {
      fitted_series <- as.data.frame(nationality_impacts[[outcome_name]]$series)
      post_series <- utils::tail(fitted_series, 9)
      projected_post_total <- sum(post_series$point.pred, na.rm = TRUE)
      final_post <- utils::tail(post_series, 1)

      data.frame(
        Nationality = pretty_nationality(outcome_name),
        `Difference Lower Percent Recalculated` =
          100 * final_post$cum.effect.lower / projected_post_total,
        `Difference Upper Percent Recalculated` =
          100 * final_post$cum.effect.upper / projected_post_total,
        check.names = FALSE
      )
    }
  ))

  summary_results <- summary_results %>%
    dplyr::select(
      -dplyr::any_of(c("Difference Lower Percent", "Difference Upper Percent"))
    ) %>%
    dplyr::left_join(recalculated_intervals, by = "Nationality") %>%
    dplyr::rename(
      `Difference Lower Percent` = `Difference Lower Percent Recalculated`,
      `Difference Upper Percent` = `Difference Upper Percent Recalculated`
    )

  combined_plot_data <- dplyr::bind_rows(nationality_plot_data) %>%
    dplyr::mutate(
      Nationality = factor(Nationality, levels = unique(Nationality)),
      Series = factor(Series, levels = c("Actual", "Projected")),
      Significance = factor(Significance, levels = c("Significant", "Not significant")),
      `Direct WOW route` = factor(
        `Direct WOW route`,
        levels = c("Direct route", "No direct route")
      )
    )

  nationality_colors <- stats::setNames(
    grDevices::hcl.colors(length(levels(combined_plot_data$Nationality)), "Dark 3"),
    levels(combined_plot_data$Nationality)
  )

  common_line_scales <- list(
    ggplot2::scale_color_manual(values = nationality_colors),
    ggplot2::scale_linetype_manual(values = c(`Direct route` = "solid", `No direct route` = "dashed")),
    ggplot2::scale_alpha_manual(values = c(Significant = 1, `Not significant` = 0.32)),
    ggplot2::scale_linewidth_manual(values = c(Significant = 1.15, `Not significant` = 0.55)),
    ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y"),
    ggplot2::scale_y_continuous(labels = scales::label_comma()),
    ggplot2::guides(
      linewidth = "none",
      alpha = ggplot2::guide_legend(override.aes = list(linewidth = c(1.15, 0.55)))
    ),
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.caption = ggplot2::element_text(color = "grey35")
    )
  )

  # 1. Actuals and projections, all overlapping on one shared x-y plane.
  # Open circles distinguish projected observations after April 2019.
  combined_nationality_plot <- ggplot2::ggplot(
    combined_plot_data,
    ggplot2::aes(
      x = Date, y = Passengers, color = Nationality,
      linetype = `Direct WOW route`, alpha = Significance,
      linewidth = Significance, group = interaction(Nationality, Series)
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = dplyr::filter(combined_plot_data, Series == "Projected"),
      shape = 1, size = 2, stroke = 0.8, show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      xintercept = post_start, linetype = "longdash", linewidth = 0.45,
      color = "grey35", inherit.aes = FALSE
    ) +
    common_line_scales +
    ggplot2::labs(
      title = "Passenger Arrivals by Nationality: Actual and Projected",
      subtitle = "Open circles identify projections after April 2019; bold lines have a significant cumulative effect.",
      x = NULL, y = "Monthly passengers", color = "Nationality",
      linetype = "2018 WOW Air service", alpha = "Cumulative effect",
      linewidth = "Cumulative effect",
      caption = "Vertical line marks April 2019. Route classifications are provisional placeholders."
    )

  ggplot2::ggsave(
    file.path(output_dir, "all_nationalities_actual_projected.png"),
    combined_nationality_plot, width = 14, height = 8, units = "in", dpi = 400
  )

  # 2. Actual passenger counts only, all overlapping on the same axes.
  actuals_only_data <- dplyr::filter(combined_plot_data, Series == "Actual")
  actuals_only_plot <- ggplot2::ggplot(
    actuals_only_data,
    ggplot2::aes(
      x = Date, y = Passengers, color = Nationality,
      linetype = `Direct WOW route`, alpha = Significance,
      linewidth = Significance, group = Nationality
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      xintercept = post_start, linetype = "longdash", linewidth = 0.45,
      color = "grey35", inherit.aes = FALSE
    ) +
    common_line_scales +
    ggplot2::labs(
      title = "Passenger Arrivals by Nationality: Actuals Only",
      subtitle = "Bold lines have a significant cumulative April-December 2019 effect.",
      x = NULL, y = "Monthly passengers", color = "Nationality",
      linetype = "2018 WOW Air service", alpha = "Cumulative effect",
      linewidth = "Cumulative effect",
      caption = "Vertical line marks April 2019. Route classifications are provisional placeholders."
    )

  ggplot2::ggsave(
    file.path(output_dir, "all_nationalities_actuals_only.png"),
    actuals_only_plot, width = 14, height = 8, units = "in", dpi = 400
  )

  # 3. Post-period differences only. Difference is actual minus projected.
  differences_only_data <- monthly_results %>%
    dplyr::left_join(
      summary_results %>%
        dplyr::select(
          Nationality, `Significant Cumulative Effect`,
          `Direct WOW Route (Provisional)`
        ),
      by = "Nationality"
    ) %>%
    dplyr::mutate(
      Nationality = factor(Nationality, levels = levels(combined_plot_data$Nationality)),
      Significance = factor(
        ifelse(`Significant Cumulative Effect`, "Significant", "Not significant"),
        levels = c("Significant", "Not significant")
      ),
      `Direct WOW route` = factor(
        ifelse(`Direct WOW Route (Provisional)`, "Direct route", "No direct route"),
        levels = c("Direct route", "No direct route")
      )
    )

  differences_only_plot <- ggplot2::ggplot(
    differences_only_data,
    ggplot2::aes(
      x = Date, y = Difference, color = Nationality,
      linetype = `Direct WOW route`, alpha = Significance,
      linewidth = Significance, group = Nationality
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey35", linewidth = 0.45) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.7) +
    common_line_scales +
    ggplot2::labs(
      title = "Passenger Arrivals by Nationality: Actual Minus Projected",
      subtitle = "Negative values indicate fewer actual passengers than projected; bold lines are significant.",
      x = NULL, y = "Actual minus projected passengers", color = "Nationality",
      linetype = "2018 WOW Air service", alpha = "Cumulative effect",
      linewidth = "Cumulative effect",
      caption = "April-December 2019. Route classifications are provisional placeholders."
    )

  ggplot2::ggsave(
    file.path(output_dir, "all_nationalities_differences_only.png"),
    differences_only_plot, width = 14, height = 8, units = "in", dpi = 400
  )

  # Manuscript nationality route and confidence-interval figure.
# Regenerate the nationality interval figure from the saved model checkpoint.
suppressPackageStartupMessages(library(dplyr))
if (!all(vapply(
  c("nationality_impacts", "nationality_summaries", "nationality_columns",
    "pretty_nationality", "output_dir"),
  exists, logical(1), inherits = TRUE
))) {
  checkpoint_path <- file.path(
    "Additional Tables and Figures", "Passengers by Nationality",
    "nationality_model_checkpoint.rds"
  )
  nationality_checkpoint <- readRDS(checkpoint_path)
  list2env(nationality_checkpoint, envir = .GlobalEnv)
}
summary_results <- dplyr::bind_rows(nationality_summaries)
recalculated_intervals <- dplyr::bind_rows(lapply(
  names(nationality_impacts),
  function(outcome_name) {
    fitted_series <- as.data.frame(nationality_impacts[[outcome_name]]$series)
    post_series <- utils::tail(fitted_series, 9)
    projected_total <- sum(post_series$point.pred, na.rm = TRUE)
    final_post <- utils::tail(post_series, 1)
    data.frame(
      Nationality = pretty_nationality(outcome_name),
      lower_recalculated = 100 * final_post$cum.effect.lower / projected_total,
      upper_recalculated = 100 * final_post$cum.effect.upper / projected_total
    )
  }
))
summary_results <- summary_results %>%
  dplyr::select(-dplyr::any_of(c("Difference Lower Percent", "Difference Upper Percent"))) %>%
  dplyr::left_join(recalculated_intervals, by = "Nationality") %>%
  dplyr::rename(
    `Difference Lower Percent` = lower_recalculated,
    `Difference Upper Percent` = upper_recalculated
  )

  # 4. Preferred cross-country comparison. Aggregates are excluded so that
  # direct-route countries are compared with other countries/country groups.
  # Effects retain their sign: a decline is negative and an increase positive.
  aggregate_labels <- vapply(nationality_columns[1:3], pretty_nationality, character(1))
  route_comparison_table <- summary_results %>%
    dplyr::filter(!Nationality %in% aggregate_labels) %>%
    dplyr::transmute(
      Nationality,
      `2018 WOW Air Service` = ifelse(
        `Direct WOW Route (Provisional)`, "Direct route", "No direct route"
      ),
      `Number of Direct Routes`,
      `Estimated Impact Percent` = `Difference Percent`,
      `Impact Lower Percent` = `Difference Lower Percent`,
      `Impact Upper Percent` = `Difference Upper Percent`,
      `YoY Difference Percent` = `Year-over-Year Percent`,
      `Significant Decline` = `Significant Cumulative Effect`,
      `Actual Passengers` = `Actual Total`,
      `Projected Passengers` = `Projected Total`
    ) %>%
    dplyr::arrange(`Estimated Impact Percent`)

  route_comparison_plot_data <- route_comparison_table %>%
    dplyr::filter(!(`Estimated Impact Percent` > 0 & `Significant Decline`)) %>%
    dplyr::mutate(
      row_position = rev(dplyr::row_number()),
      Significance = factor(
        ifelse(`Significant Decline`, "Significant decline", "Not significant"),
        levels = c("Significant decline", "Not significant")
      ),
      `Impact Direction` = ifelse(`Estimated Impact Percent` < 0, "Decline", "Increase")
    )

  comparison_row_count <- nrow(route_comparison_plot_data)

  # Left-hand table panel. It shares numeric row positions with the interval
  # plot, which keeps every country and estimate precisely aligned.
  route_comparison_table_panel <- ggplot2::ggplot(route_comparison_plot_data) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = 0.02, y = row_position, label = Nationality,
        fontface = ifelse(`Significant Decline`, "bold", "plain")
      ),
      hjust = 0, size = 3.2, color = "black", show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 0.50, y = row_position, label = `Number of Direct Routes`),
      hjust = 0.5, size = 3.2, color = "black"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = 0.96, y = row_position,
        label = sprintf("%.1f%%", `YoY Difference Percent`)
      ),
      hjust = 1, size = 3.2, color = "black"
    ) +
    ggplot2::annotate(
      "text", x = c(0.02, 0.50, 0.96),
      y = comparison_row_count + 0.85,
      label = c("Nationality", "WOW Air\nDirect Routes", "YoY Change"),
      hjust = c(0, 0.5, 1), fontface = "bold", size = 3.3
    ) +
    ggplot2::annotate(
      "segment", x = 0, xend = 1, y = comparison_row_count + 0.48,
      yend = comparison_row_count + 0.48, linewidth = 0.45, color = "grey55"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      limits = c(0.5, comparison_row_count + 1.25), expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 8, r = 22, b = 38, l = 8))

  route_comparison_plot <- ggplot2::ggplot(
    route_comparison_plot_data,
    ggplot2::aes(
      x = `Estimated Impact Percent`,
      y = row_position
    )
  ) +
    ggplot2::annotate(
      "segment", x = 0, xend = 0, y = 0.5,
      yend = comparison_row_count + 0.45,
      color = "grey60", linewidth = 0.5
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = `Impact Lower Percent`,
        xmax = `Impact Upper Percent`
      ),
      orientation = "y", width = 0.14, linewidth = 0.8, color = "#326987"
    ) +
    ggplot2::geom_point(size = 3.2, color = "#B23A48") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", `Estimated Impact Percent`)),
      vjust = -0.9, size = 2.8, color = "#B23A48", show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(suffix = "%", accuracy = 1),
      breaks = scales::breaks_pretty(n = 6)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0.5, comparison_row_count + 1.25), breaks = NULL, expand = c(0, 0)
    ) +
    ggplot2::annotate(
      "text", x = 0, y = comparison_row_count + 0.85,
      label = "Estimated Impact and 95% Interval",
      fontface = "bold", size = 3.3
    ) +
    ggplot2::annotate(
      "segment", x = -Inf, xend = Inf, y = comparison_row_count + 0.48,
      yend = comparison_row_count + 0.48, linewidth = 0.45, color = "grey55"
    ) +
    ggplot2::labs(
      x = "Nine-month cumulative relative impact (%)",
      y = NULL
    ) +
    ggplot2::guides(
      color = "none",
      alpha = "none",
      linewidth = "none",
      size = "none"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 8, r = 8, b = 8, l = 12)
    )

  route_comparison_hybrid <- gridExtra::arrangeGrob(
    route_comparison_table_panel,
    route_comparison_plot,
    ncol = 2,
    widths = c(0.82, 2.18)
  )

  nationality_output <- file.path(output_dir, "nationality_route_effect_comparison.png")
  nationality_temp <- tempfile(fileext = ".png")
  ggplot2::ggsave(
    nationality_temp, route_comparison_hybrid,
    width = 14, height = 8, units = "in", dpi = 400,
    bg = "white", device = grDevices::png
  )
  if (!file.copy(nationality_temp, nationality_output, overwrite = TRUE)) {
    stop("Could not replace the nationality figure: ", nationality_output)
  }
  unlink(nationality_temp)

  if (FALSE) { # Retained old full-world draft for reference; superseded below.
  # 5. Geographic version of the route comparison. This requires the small
  # CRAN package "maps" (install.packages("maps")); all other outputs still
  # finish if it is unavailable.
  if (requireNamespace("maps", quietly = TRUE)) {
    country_geography <- tibble::tribble(
      ~Nationality, ~map_region, ~label_long, ~label_lat,
      "Austria", "Austria", 14.2, 47.6,
      "Australia and New Zealand", "Australia", 136.0, -25.0,
      "Australia and New Zealand", "New Zealand", 136.0, -25.0,
      "United States", "USA", -101.0, 38.0,
      "Belgium", "Belgium", 4.7, 50.7,
      "United Kingdom", "UK", -4.0, 55.0,
      "Denmark", "Denmark", 10.0, 56.2,
      "Estonia, Latvia, and Lithuania", "Estonia", 25.5, 58.3,
      "Estonia, Latvia, and Lithuania", "Latvia", 25.5, 58.3,
      "Estonia, Latvia, and Lithuania", "Lithuania", 25.5, 58.3,
      "Finland", "Finland", 26.0, 64.5,
      "France", "France", 2.0, 46.3
    )

    map_effects <- route_comparison_table %>%
      dplyr::left_join(country_geography, by = "Nationality")

    label_effects <- map_effects %>%
      dplyr::group_by(Nationality) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(
        summary_results %>%
          dplyr::select(Nationality, `Year-over-Year Percent`, `Difference Total`),
        by = "Nationality"
      ) %>%
      dplyr::mutate(
        map_label = paste0(
          Nationality,
          "\nYoY: ", scales::percent(`Year-over-Year Percent` / 100, accuracy = 1),
          " | Actual change: ", scales::comma(round(`Difference Total`)),
          " (", scales::percent(-`Estimated Decline Percent` / 100, accuracy = 1), ")"
        ),
        Significance = ifelse(`Significant Decline`, "Significant", "Not significant")
      )

    world_map <- ggplot2::map_data("world")
    mapped_world <- world_map %>%
      dplyr::left_join(
        map_effects %>%
          dplyr::select(
            map_region, `Estimated Decline Percent`, `Significant Decline`,
            `2018 WOW Air Service`
          ) %>%
          dplyr::distinct(),
        by = c("region" = "map_region")
      ) %>%
      dplyr::mutate(
        map_alpha = dplyr::case_when(
          is.na(`Estimated Decline Percent`) ~ "No estimate",
          `Significant Decline` ~ "Significant",
          TRUE ~ "Not significant"
        )
      )

    reykjavik <- data.frame(long = -21.9426, lat = 64.1466)
    direct_route_ends <- label_effects %>%
      dplyr::filter(`2018 WOW Air Service` == "Direct route")

    aggregate_callouts <- summary_results %>%
      dplyr::filter(Nationality %in% aggregate_labels) %>%
      dplyr::mutate(
        label_long = dplyr::case_when(
          Nationality == "North America" ~ -105,
          Nationality == "European Union" ~ 42,
          TRUE ~ 105
        ),
        label_lat = dplyr::case_when(
          Nationality == "North America" ~ 66,
          Nationality == "European Union" ~ 69,
          TRUE ~ 24
        ),
        map_label = paste0(
          Nationality,
          "\nYoY: ", scales::percent(`Year-over-Year Percent` / 100, accuracy = 1),
          " | Actual change: ", scales::comma(round(`Difference Total`)),
          " (", scales::percent(`Difference Percent` / 100, accuracy = 1), ")"
        ),
        Significance = ifelse(
          `Significant Cumulative Effect`, "Significant", "Not significant"
        )
      )

    nationality_impact_map <- ggplot2::ggplot() +
      ggplot2::geom_polygon(
        data = mapped_world,
        ggplot2::aes(
          x = long, y = lat, group = group,
          fill = `Estimated Decline Percent`, alpha = map_alpha
        ),
        color = "white", linewidth = 0.12
      ) +
      ggplot2::geom_curve(
        data = direct_route_ends,
        ggplot2::aes(
          x = reykjavik$long, y = reykjavik$lat,
          xend = label_long, yend = label_lat
        ),
        curvature = 0.16, color = "#7A2845", linewidth = 0.55, alpha = 0.75
      ) +
      ggplot2::geom_point(
        data = reykjavik,
        ggplot2::aes(x = long, y = lat),
        shape = 21, fill = "#F2C14E", color = "#4A1930", size = 3
      ) +
      ggrepel::geom_label_repel(
        data = label_effects,
        ggplot2::aes(
          x = label_long, y = label_lat, label = map_label,
          alpha = Significance
        ),
        size = 2.6, label.size = 0.2, fill = "white", color = "grey15",
        min.segment.length = 0, seed = 12345, max.overlaps = Inf,
        box.padding = 0.35, point.padding = 0.2
      ) +
      ggplot2::geom_label(
        data = aggregate_callouts,
        ggplot2::aes(
          x = label_long, y = label_lat, label = map_label,
          alpha = Significance
        ),
        size = 2.7, label.size = 0.25, fill = "#FFF8E7", color = "grey15"
      ) +
      ggplot2::annotate(
        "text", x = -21.9, y = 67.2, label = "Reykjavík",
        size = 3, fontface = "bold", color = "#4A1930"
      ) +
      ggplot2::scale_fill_gradient2(
        low = "#3B82A0", mid = "#F1E9DA", high = "#B23A48",
        midpoint = 0, na.value = "grey88",
        labels = scales::label_number(suffix = "%", accuracy = 1),
        name = "Estimated decline"
      ) +
      ggplot2::scale_alpha_manual(
        values = c(Significant = 1, `Not significant` = 0.35, `No estimate` = 0.45),
        name = "Cumulative effect"
      ) +
      ggplot2::coord_quickmap(xlim = c(-140, 165), ylim = c(-50, 76), expand = FALSE) +
      ggplot2::labs(
        title = "Passenger Impacts and WOW Air's 2018 Route Network",
        subtitle = paste0(
          "Country color shows the estimated April-December 2019 decline; ",
          "faded countries have statistically insignificant effects."
        ),
        x = NULL, y = NULL,
        caption = paste0(
          "Lines show provisional direct routes to Iceland. YoY compares Apr-Dec 2019 with Apr-Dec 2018; ",
          "actual change is actual minus projected passengers. 'Other' is positioned over Asia/rest of world."
        )
      ) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(face = "bold", size = 16),
        plot.subtitle = ggplot2::element_text(size = 10),
        plot.caption = ggplot2::element_text(size = 8, color = "grey35")
      )

    ggplot2::ggsave(
      file.path(output_dir, "nationality_impact_route_map.png"),
      nationality_impact_map, width = 16, height = 9, units = "in", dpi = 400
    )
  } else {
    warning(
      "The nationality impact map was skipped because package 'maps' is not installed. ",
      "Run install.packages('maps') and rerun this script to create it."
    )
  }

  }

  # Redesigned map: Iceland and Europe dominate the composition, with North
  # America and Asia/rest-of-world shown as supporting panels at the side.
  if (requireNamespace("maps", quietly = TRUE)) {
    world_map <- ggplot2::map_data("world")

    country_lookup <- tibble::tribble(
      ~Nationality, ~map_region, ~label_long, ~label_lat, ~panel,
      "Austria", "Austria", 14.2, 47.6, "Europe",
      "United States", "USA", -100.0, 38.0, "North America",
      "Belgium", "Belgium", 4.7, 50.8, "Europe",
      "United Kingdom", "UK", -3.5, 54.5, "Europe",
      "Denmark", "Denmark", 10.0, 56.2, "Europe",
      "Estonia, Latvia, and Lithuania", "Estonia", 25.0, 58.5, "Europe",
      "Estonia, Latvia, and Lithuania", "Latvia", 25.0, 58.5, "Europe",
      "Estonia, Latvia, and Lithuania", "Lithuania", 25.0, 58.5, "Europe",
      "Finland", "Finland", 26.0, 64.5, "Europe",
      "France", "France", 2.0, 46.2, "Europe",
      "Netherlands", "Netherlands", 5.4, 52.2, "Europe",
      "Hong Kong", "Hong Kong", 114.2, 22.3, "Asia / Rest of World",
      "India", "India", 78.9, 22.8, "Asia / Rest of World",
      "Ireland", "Ireland", -8.0, 53.3, "Europe",
      "Israel", "Israel", 34.9, 31.6, "Europe",
      "Italy", "Italy", 12.5, 42.5, "Europe",
      "Japan", "Japan", 138.0, 37.0, "Asia / Rest of World",
      "Canada", "Canada", -94.0, 55.0, "North America",
      "China", "China", 104.0, 35.0, "Asia / Rest of World",
      "Norway", "Norway", 9.0, 62.0, "Europe",
      "Poland", "Poland", 19.0, 52.0, "Europe",
      "Russia", "Russia", 31.0, 59.0, "Europe",
      "Singapore", "Singapore", 103.8, 1.3, "Asia / Rest of World",
      "Spain", "Spain", -4.0, 40.2, "Europe",
      "South Korea", "South Korea", 127.8, 36.2, "Asia / Rest of World",
      "Switzerland", "Switzerland", 8.2, 46.8, "Europe",
      "Sweden", "Sweden", 16.0, 62.0, "Europe",
      "Taiwan", "Taiwan", 121.0, 23.7, "Asia / Rest of World",
      "Germany", "Germany", 10.5, 51.0, "Europe",
      "Other Countries", NA_character_, 102.0, 12.0, "Asia / Rest of World",
      "Australia and New Zealand", "Australia", 137.0, -25.0, "Asia / Rest of World",
      "Australia and New Zealand", "New Zealand", 137.0, -25.0, "Asia / Rest of World"
    )

    map_results <- summary_results %>%
      dplyr::mutate(
        `Estimated Impact Percent` = `Difference Percent`,
        Significance = ifelse(
          `Significant Cumulative Effect`, "Significant", "Not significant"
        ),
        result_label = paste0(
          Nationality,
          "\nYoY change: ", scales::percent(`Year-over-Year Percent` / 100, accuracy = 1),
          "\nActual vs Projected: ", scales::percent(`Difference Percent` / 100, accuracy = 1)
        )
      )

    mapped_world <- world_map %>%
      dplyr::left_join(
        country_lookup %>%
          dplyr::left_join(map_results, by = "Nationality") %>%
          dplyr::select(
            map_region, `Estimated Impact Percent`, Significance
          ) %>%
          dplyr::distinct(),
        by = c("region" = "map_region")
      ) %>%
      dplyr::mutate(
        Significance = dplyr::if_else(
          is.na(Significance), "No estimate", Significance
        )
      )

    map_labels <- country_lookup %>%
      dplyr::group_by(Nationality) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(map_results, by = "Nationality")

    # The low-resolution world polygons do not reliably identify the Canary
    # Islands as Spain, so plot and color them explicitly with Spain's result.
    spain_islands <- tidyr::crossing(
      tibble::tibble(
        island = c("Tenerife", "Gran Canaria"),
        long = c(-16.25, -15.43),
        lat = c(28.46, 28.12)
      ),
      map_results %>%
        dplyr::filter(Nationality == "Spain") %>%
        dplyr::select(`Estimated Impact Percent`, Significance)
    )

    aggregate_labels_map <- map_results %>%
      dplyr::filter(Nationality %in% aggregate_labels) %>%
      dplyr::mutate(
        panel = dplyr::case_when(
          Nationality == "North America" ~ "North America",
          Nationality == "European Union" ~ "Europe",
          TRUE ~ "Asia / Rest of World"
        ),
        label_long = dplyr::case_when(
          Nationality == "North America" ~ -122,
          Nationality == "European Union" ~ 26,
          TRUE ~ 72
        ),
        label_lat = dplyr::case_when(
          Nationality == "North America" ~ 64,
          Nationality == "European Union" ~ 70,
          TRUE ~ 8
        )
      )

    # City-level 2018 network transcribed from the supplied route-map image.
    # Add or remove rows here if the final historical route list changes.
    wow_routes_2018 <- tibble::tribble(
      ~city, ~long, ~lat, ~panel,
      "Stockholm", 18.07, 59.33, "Europe",
      "Edinburgh", -3.19, 55.95, "Europe",
      "Copenhagen", 12.57, 55.68, "Europe",
      "Warsaw", 21.01, 52.23, "Europe",
      "Berlin", 13.41, 52.52, "Europe",
      "Hamburg", 9.99, 53.55, "Europe",
      "Düsseldorf", 6.78, 51.23, "Europe",
      "Brussels", 4.35, 50.85, "Europe",
      "Amsterdam", 4.90, 52.37, "Europe",
      "Dublin", -6.26, 53.35, "Europe",
      "London", -0.13, 51.51, "Europe",
      "Frankfurt", 8.68, 50.11, "Europe",
      "Paris", 2.35, 48.86, "Europe",
      "Salzburg", 13.05, 47.81, "Europe",
      "Lyon", 4.84, 45.76, "Europe",
      "Milan", 9.19, 45.46, "Europe",
      "Barcelona", 2.17, 41.39, "Europe",
      "Alicante", -0.49, 38.35, "Europe",
      "Tenerife", -16.25, 28.46, "Europe",
      "Gran Canaria", -15.43, 28.12, "Europe",
      "Tel Aviv", 34.78, 32.09, "Europe",
      "San Francisco", -122.42, 37.77, "North America",
      "Los Angeles", -118.24, 34.05, "North America",
      "Dallas–Fort Worth", -97.04, 32.90, "North America",
      "St. Louis", -90.20, 38.63, "North America",
      "Chicago", -87.63, 41.88, "North America",
      "Cincinnati", -84.51, 39.10, "North America",
      "Cleveland", -81.69, 41.50, "North America",
      "Detroit", -83.05, 42.33, "North America",
      "Pittsburgh", -80.00, 40.44, "North America",
      "Toronto", -79.38, 43.65, "North America",
      "Montreal", -73.57, 45.50, "North America",
      "Boston", -71.06, 42.36, "North America",
      "New York", -74.01, 40.71, "North America",
      "Washington, DC", -77.04, 38.91, "North America"
    ) %>%
      dplyr::mutate(
        Nationality = dplyr::case_when(
          city %in% c(
            "San Francisco", "Los Angeles", "Dallas–Fort Worth", "St. Louis",
            "Chicago", "Cincinnati", "Cleveland", "Detroit", "Pittsburgh",
            "Boston", "New York", "Washington, DC"
          ) ~ "United States",
          city %in% c("Toronto", "Montreal") ~ "Canada",
          city %in% c("London", "Edinburgh") ~ "United Kingdom",
          city %in% c("Paris", "Lyon") ~ "France",
          city %in% c("Berlin", "Hamburg", "Düsseldorf", "Frankfurt") ~ "Germany",
          city %in% c("Barcelona", "Alicante", "Tenerife", "Gran Canaria") ~ "Spain",
          city == "Stockholm" ~ "Sweden",
          city == "Copenhagen" ~ "Denmark",
          city == "Warsaw" ~ "Poland",
          city == "Brussels" ~ "Belgium",
          city == "Amsterdam" ~ "Netherlands",
          city == "Dublin" ~ "Ireland",
          city == "Salzburg" ~ "Austria",
          city == "Milan" ~ "Italy",
          city == "Tel Aviv" ~ "Israel",
          TRUE ~ NA_character_
        )
      )

    # Fail loudly if a future route-map edit makes the stars and reported
    # country counts disagree.
    route_count_check <- wow_routes_2018 %>%
      dplyr::filter(!is.na(Nationality)) %>%
      dplyr::count(Nationality, name = "map_count") %>%
      dplyr::mutate(expected_count = unname(wow_route_count_2018[Nationality]))
    if (any(route_count_check$map_count != route_count_check$expected_count)) {
      stop("The city-level map routes do not match wow_route_count_2018.")
    }

    map_fill_limit <- max(abs(map_results$`Estimated Impact Percent`), na.rm = TRUE)

    map_scales_and_theme <- list(
      ggplot2::scale_fill_gradient2(
        low = "#C33C54", mid = "#FAFAFA", high = "#327A9A",
        midpoint = 0, limits = c(-map_fill_limit, map_fill_limit),
        na.value = "#ECECEC",
        labels = scales::label_number(suffix = "%", accuracy = 1),
        name = "Actual vs projected (%)",
        guide = ggplot2::guide_colorbar(
          title.position = "top", title.hjust = 0.5,
          barwidth = grid::unit(7, "cm"), barheight = grid::unit(0.35, "cm")
        )
      ),
      ggplot2::scale_alpha_manual(
        values = c(Significant = 1, `Not significant` = 0.32, `No estimate` = 0.55),
        guide = "none"
      ),
      ggplot2::theme_void(base_size = 10),
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.title = ggplot2::element_text(color = "#222222", face = "bold", size = 12),
        legend.text = ggplot2::element_text(color = "#222222"),
        legend.title = ggplot2::element_text(color = "#222222"),
        legend.key = ggplot2::element_rect(fill = "white", color = NA)
      )
    )

    make_route_panel <- function(panel_name, x_limits, y_limits, show_legend = FALSE) {
      panel_routes <- dplyr::filter(wow_routes_2018, panel == panel_name)
      panel_labels <- dplyr::filter(
        map_labels, panel == panel_name, !is.na(result_label)
      )
      panel_aggregates <- dplyr::filter(aggregate_labels_map, panel == panel_name)

      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(
          data = mapped_world,
          ggplot2::aes(
            x = long, y = lat, group = group,
            fill = `Estimated Impact Percent`, alpha = Significance
          ),
          color = "#BDBDBD", linewidth = 0.12
        ) +
        ggplot2::geom_point(
          data = spain_islands,
          ggplot2::aes(
            x = long, y = lat, fill = `Estimated Impact Percent`,
            alpha = Significance
          ),
          shape = 21, color = "#555555", size = 3.2, stroke = 0.35
        ) +
        ggplot2::geom_text(
          data = spain_islands,
          ggplot2::aes(x = long, y = lat, label = island),
          color = "#222222", size = 1.9, nudge_y = -0.9
        ) +
        ggplot2::geom_point(
          data = panel_routes,
          ggplot2::aes(x = long, y = lat),
          shape = 8, color = "#B07A12", size = 1.8, stroke = 0.65
        ) +
        ggrepel::geom_text_repel(
          data = panel_labels,
          ggplot2::aes(
            x = label_long, y = label_lat, label = result_label
          ),
          color = "black", size = 2.05, lineheight = 0.95,
          fontface = "plain",
          min.segment.length = 0, seed = 12345, max.overlaps = Inf,
          box.padding = 0.8, point.padding = 0.45, force = 2,
          max.time = 4, segment.color = "#8A8A8A", segment.alpha = 0.75
        ) +
        ggplot2::geom_label(
          data = panel_aggregates,
          ggplot2::aes(
            x = label_long, y = label_lat, label = result_label
          ),
          color = "black", fill = "white", size = 2.2,
          fontface = "plain",
          label.size = 0.2
        ) +
        ggplot2::coord_quickmap(xlim = x_limits, ylim = y_limits, expand = FALSE) +
        ggplot2::labs(title = panel_name) +
        map_scales_and_theme

      if (!show_legend) p <- p + ggplot2::theme(legend.position = "none")
      p
    }

    europe_map <- make_route_panel("Europe", c(-40, 45), c(24, 75), TRUE) +
      ggplot2::annotate(
        "point", x = -21.94, y = 64.15, shape = 8,
        color = "#8F5F06", size = 3.6, stroke = 0.9
      ) +
      ggplot2::annotate(
        "text", x = -21.94, y = 66.0, label = "Reykjavík",
        color = "black", fontface = "plain", size = 3
      ) +
      ggplot2::labs(title = NULL) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        plot.margin = ggplot2::margin(2, 2, 2, 2)
      )

    north_america_map <- make_route_panel(
      "North America", c(-130, -55), c(24, 68), FALSE
    ) +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(
          color = "grey55", fill = NA, linewidth = 0.6
        ),
        plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
        plot.margin = ggplot2::margin(3, 3, 3, 3)
      )

    asia_map <- make_route_panel(
      "Asia / Rest of World", c(55, 180), c(-48, 58), FALSE
    ) +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(
          color = "grey55", fill = NA, linewidth = 0.6
        ),
        plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
        plot.margin = ggplot2::margin(3, 3, 3, 3)
      )

    # Place the supporting regions over the outer edges of the primary map,
    # avoiding the large empty margins created by a three-column layout.
    nationality_impact_map <- europe_map +
      patchwork::inset_element(
        north_america_map,
        left = 0.005, bottom = 0.61, right = 0.245, top = 0.985,
        align_to = "full", clip = TRUE
      ) +
      patchwork::inset_element(
        asia_map,
        left = 0.755, bottom = 0.61, right = 0.995, top = 0.985,
        align_to = "full", clip = TRUE
      )

    ggplot2::ggsave(
      file.path(output_dir, "nationality_impact_route_map.png"),
      nationality_impact_map, width = 14, height = 9, units = "in", dpi = 400,
      bg = "white"
    )
  } else {
    warning(
      "The nationality impact map was skipped because package 'maps' is not installed. ",
      "Run install.packages('maps') and rerun this script to create it."
    )
  }

  # Compile every successfully estimated nationality model into a paper-style
  # LaTeX table matching the project's other CausalImpact result tables.
  passenger_pstars <- function(p) {
    ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
  }
  latex_escape_nationality <- function(x) {
    x <- gsub("&", "\\\\&", x, fixed = TRUE)
    x <- gsub("%", "\\\\%", x, fixed = TRUE)
    x <- gsub("_", "\\\\_", x, fixed = TRUE)
    x
  }
  format_table_integer <- function(x) {
    formatC(round(as.numeric(x)), format = "d", big.mark = ",")
  }

  nationality_table_data <- summary_results %>%
    dplyr::mutate(
      table_group = dplyr::case_when(
        Nationality %in% aggregate_labels ~ "Panel A: Regional aggregates",
        `Direct WOW Route (Provisional)` ~ "Panel B: Direct WOW Air markets",
        TRUE ~ "Panel C: Other markets"
      ),
      route_display = dplyr::case_when(
        Nationality %in% aggregate_labels ~ "Aggregate",
        TRUE ~ as.character(`Number of Direct Routes`)
      ),
      group_order = match(
        table_group,
        c(
          "Panel A: Regional aggregates",
          "Panel B: Direct WOW Air markets",
          "Panel C: Other markets"
        )
      )
    ) %>%
    dplyr::arrange(group_order, Nationality)

  nationality_table_rows <- character(0)
  table_groups <- unique(nationality_table_data$table_group)
  for (table_group_name in table_groups) {
    nationality_table_rows <- c(
      nationality_table_rows,
      paste0(
        "\\midrule\n\\multicolumn{9}{l}{\\textit{",
        latex_escape_nationality(table_group_name),
        "}} \\\\"
      )
    )

    group_rows <- nationality_table_data %>%
      dplyr::filter(table_group == table_group_name)
    for (i in seq_len(nrow(group_rows))) {
      r <- group_rows[i, ]
      nationality_table_rows <- c(
        nationality_table_rows,
        paste0(
          latex_escape_nationality(r$Nationality), " & ",
          r$route_display, " & ",
          sprintf("%.2f%s", r$`Difference Percent`, passenger_pstars(r$`p-value`)), " & ",
          format_table_integer(r$`Difference Total`), " & ",
          format_table_integer(r$`Cumulative Effect Lower`), " & ",
          format_table_integer(r$`Cumulative Effect Upper`), " & ",
          sprintf("%.2f", r$`Year-over-Year Percent`), " & ",
          sprintf("%.4f", r$`p-value`), " & ",
          format_table_integer(r$`RMSE (pre-period)`),
          " \\\\"
        )
      )
    }
  }

  nationality_table_tex <- c(
    "\\begin{sidewaystable}[p]",
    "\\centering",
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{3.5pt}",
    "\\begin{tabular}{llrrrrrrr}",
    "\\toprule",
    paste0(
      "Passenger nationality & Direct routes & Relative impact (\\%) & 9-month impact & ",
      "95\\% CI lower & 95\\% CI upper & YoY change (\\%) & p-value & RMSE \\\\"
    ),
    nationality_table_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{\\textheight}",
    paste0(
      "\\raggedright\\scriptsize \\textit{Note:} Relative impact is the April--December 2019 ",
      "difference between observed and counterfactual passenger arrivals divided by the counterfactual. ",
      "YoY change compares observed April--December 2019 arrivals with the same months of 2018. ",
      "Direct-route classifications correspond to the provisional 2018 WOW Air route network used in the map. ",
      "RMSE is calculated over the pre-period. Stars denote statistical significance: ",
      "*** $p<0.01$, ** $p<0.05$, * $p<0.10$."
    ),
    "\\end{minipage}",
    "\\end{sidewaystable}"
  )

  writeLines(
    nationality_table_tex,
    file.path(output_dir, "nationality_impact_table.tex")
  )

  nationality_table_markdown <- nationality_table_data %>%
    dplyr::transmute(
      Nationality,
      `Direct Routes` = route_display,
      `Relative Impact (%)` = sprintf(
        "%.2f%s", `Difference Percent`, passenger_pstars(`p-value`)
      ),
      `9-Month Impact` = format_table_integer(`Difference Total`),
      `95% CI Lower` = format_table_integer(`Cumulative Effect Lower`),
      `95% CI Upper` = format_table_integer(`Cumulative Effect Upper`),
      `YoY Change (%)` = sprintf("%.2f", `Year-over-Year Percent`),
      `p-value` = sprintf("%.4f", `p-value`),
      `RMSE (pre-period)` = format_table_integer(`RMSE (pre-period)`)
    )
  writeLines(
    knitr::kable(
      nationality_table_markdown,
      format = "markdown",
      row.names = FALSE,
      caption = "Passenger-Nationality Effects of the WOW Air Bankruptcy"
    ),
    file.path(output_dir, "nationality_impact_table.md")
  )

  openxlsx::write.xlsx(
    list(
      `Actual and Projected` = monthly_results,
      Summary = summary_results,
      `Route Comparison` = route_comparison_table,
      `Publication Table` = nationality_table_data %>%
        dplyr::select(
          Nationality, route_display, `Difference Percent`, `Difference Total`,
          `Cumulative Effect Lower`, `Cumulative Effect Upper`,
          `Year-over-Year Percent`, `p-value`, `RMSE (pre-period)`
        ) %>%
        dplyr::rename(`Direct Routes` = route_display)
    ),
    file = file.path(output_dir, "nationality_actuals_and_projections.xlsx"),
    asTable = TRUE,
    overwrite = TRUE
  )
}

source("Scripts/Combine Passenger Nationality Tables.R")
