# Co-variate Regression & Prediction}

causal <- function(complete, dep, exclude,train_start,train_end,treat_end,series_start,treat_start,alpha) {
  
  date <- which(colnames(complete)=="date")
  
  # IDea is that we can have a pre portion on the graph
  synth_start <- complete[,c(date,dep)]
  synth_start <- synth_start %>% filter(date >= series_start) %>% filter(date <= treat_end)
  alpha <- qnorm(1-alpha/2)
  
  # pick your treatment column
  complete2 <- as.data.frame(complete[dep])
  colnames(complete2) <- colnames(complete)[dep]
  icelandcolumns <- grep(exclude, colnames(complete)) # exclude other Icelandic Information
  complete <- complete[,-c(icelandcolumns)]
  complete <- cbind(complete,complete2)
  
  
  # doing this before the training filter for date gets rid of NA columns, super helpful!
  
  complete  <- complete %>% filter(date >= train_start) %>% filter(date <= treat_end) # to do arrivals 
  complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(colnames(complete2)[1]) %>% relocate(date)
  
  # Extract the observed outcomes
  observed <- as.data.frame(complete[,c(1,2)]) 
  
  # Training Period!
  # Pick a pretreatment Period to cutoff before
  # Filter out the Treatment Period for our Training
  training <- complete %>% filter(date < train_end)
  training <- training[,-1] # Drop the Date Value
  model <- lm(training[,1] ~.,training[-1]) # Optimize to find weights
  weights <- as.matrix(coef(model)) # Pick out a kx1 matrix of Coefficients on regressors
  weights[is.na(weights)] <- 0 # Make Any NA Coefficients = 0 so they multiply out
  # We've Finished Training the model!!
  
  
  # Estimating with the Coefficients 
  # For our Original Data source (And Entire History (Before & After Treatment))
  # Remove the Date Column        
  complete <- complete[,-1]
  complete[,1] <- 1 #Replace observed column (intercept) with value = 1, For Intercept!
  complete <- as.matrix(complete) # Change to Matrix for multiplication
  
  synth_temp <- complete %*% weights
  synth <- as.data.frame(cbind(observed,synth_temp))
  colnames(synth) <- c("date","observed","estimated")
  synth$errors <- synth$observed-synth$estimated
  root_mspe <- synth %>% filter(date >= train_start) %>% filter(date <= train_end)
  root_mspe <- sqrt(sum(root_mspe$errors^2)/length(root_mspe$errors))
  
  stdev <- summary(model)$sigma
  
  synth <- subset(synth, select = -c(observed))
  
  synth <- merge(synth_start, synth, by="date", all.x=T)
  
  # Esimated Cumulative Impact
  cumulative_impact <- sum(synth$errors[c(which(synth$date == treat_start):which(synth$date==treat_end))])
  cumulative_observed <- sum(synth[c(which(synth$date == treat_start):which(synth$date==treat_end)),2])
  # Estimated Monthly Impact
  monthly_impact <- cumulative_impact/length(synth$errors[c(which(synth$date == treat_start):which(synth$date==treat_end))])
  
  
  loss_percent <- cumulative_observed/(cumulative_observed-cumulative_impact) - 1
  
  #relative_loss_percent <- synth$passengers_keflavik_iceland(synth$errors - synth$estimated)
  
  
  
  synth$cumulative_errors <- 0
  
  synth$upper_CI <- synth$estimated + stdev*alpha
  synth$lower_CI <-  synth$estimated - stdev*alpha
  
  
  
  # This first loop accumulates errors
  j <- 0
  ci_width <- 0
  for (i in which(synth$date == treat_start):which(synth$date==treat_end)){
    synth[i,5] <- synth[i,4] + synth[i-1,5]
    
    
    # Run these if we want our CI to accumulate errors  
    # Upper CI
    synth[i,6] <- synth[i,6] +  stdev*alpha*j
    # Lower CI
    synth[i,7] <- synth[i,7] - stdev*alpha*j
    j <- j+1.5*1/12 # basically I grow my CIs by a rate of 1.5 per year
    ci_width <- stdev*j + ci_width
  }
  
  p_value <- cumulative_impact/ci_width
  
  p_value <- ifelse(p_value>0, pnorm(p_value, mean = 0, sd = alpha, lower.tail = TRUE), pnorm(p_value, mean = 0, sd = alpha, lower.tail = FALSE))
  
  p_value <- 1-p_value
  
  
  
  
  
  # We are working backwards, assuming our mu = 0, no deviation, our xbar = cumulative difference
  # our deviation is the width of CI 
  
  
  
  ribbondata <- data.frame(synth[,c(1,5,6)])
  synth_df <- synth
  synth <- ts(synth[,-1], start = c(year(min(synth[,1])), month(min(synth[,1]))), frequency = 12)  
  
  treat_start <- as.numeric(year(treat_start)) + as.numeric(format(as.Date(treat_start), "%j")) / ifelse(leap_year(as.Date(treat_start)), 366, 365)
  
  # Because treatment occurs in the month Prior to observations, subtract a 1 month value
  treat_start <- treat_start - 1/12
  train_end <- as.numeric(year(train_end)) + as.numeric(format(as.Date(train_end), "%j")) / ifelse(leap_year(as.Date(train_end)), 366, 365)
  
  
  
  plot_ts <- autoplot(synth[,c(1,2,5,6)]) + ggtitle("Impact Assesment Assesment Using Only Covariates")+
    labs(x="Date", y="Monthly Passengers") + geom_vline(xintercept = treat_start)+ geom_vline(xintercept = train_end)+
    annotate("text",
             x = treat_start + 0.15,
             y = max(synth[,3], na.rm = TRUE),
             label = "- Bankruptcy",
             hjust = 0)
  
  plot_differences <- autoplot(synth[,c(3)]) +
    ggtitle("Difference Between Observed & Estimated") +
    labs(x = "Date", y = "Monthly Passengers") +
    geom_vline(xintercept = treat_start) +
    geom_vline(xintercept = train_end) +
    geom_hline(yintercept = 0) +
    annotate(
      "text",
      x = treat_start,
      y = max(synth[,3], na.rm = TRUE),
      label = "- Bankruptcy",
      hjust = 0
    ) +
    annotate(
      "text",
      x = train_end - 0.15,
      y = min(synth[,3], na.rm = TRUE) * 0.85,
      label = "Training Period Stopped -",
      hjust = 1,
      vjust = 1
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.03, 0.20))) +
    theme(
      plot.margin = margin(t = 10, r = 60, b = 10, l = 10)
    )
  
  plot_cumul_differences <- autoplot(synth[,c(4)]) +
    ggtitle("Cumulative Differences") +
    labs(x = "Date", y = "Passengers") +
    geom_vline(xintercept = treat_start) +
    geom_vline(xintercept = train_end) +
    annotate(
      "text",
      x = treat_start,
      y = max(synth[,4], na.rm = TRUE),
      label = "- Bankruptcy",
      hjust = 0
    ) +
    annotate(
      "text",
      x = train_end - 0.15,
      y = min(synth[,4], na.rm = TRUE) * 0.70,
      label = "Training Period Stopped -",
      hjust = 1,
      vjust = 1
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.03, 0.20))) +
    theme(
      plot.margin = margin(t = 10, r = 60, b = 10, l = 10)
    )
  
  plot_weight <- barplot(coef(model), names.arg = rownames(as.data.frame(coef(model))), xlab = "Explanatory Variables", ylab = "Weights", main = "Weighting Distribution for Counterfactual", las=2)  
  
  
  rm(list = c("complete","complete2","date","dep","exclude","i","icelandcolumns","model","observed","series_start","synth_start","synth_temp","train_end","train_start","training","treat_end","treat_start","weights"))
  
  result <- list(plot_ts=plot_ts,plot_weight=plot_weight,plot_differences=plot_differences, plot_cumul_differences=plot_cumul_differences, root_mspe=root_mspe,synth=synth,cumulative_impact=cumulative_impact,monthly_impact=monthly_impact, p_value=p_value, loss_percent=loss_percent)
  return(result)
}

# Limit month to relevant columns:
month_passengers <- month[,c(tourism_list)]
temp <- causal(month_passengers, 3,"iceland",'2014-01-01','2019-01-01','2019-12-01','2014-01-01','2019-04-01',.01)


ggsave(
  "Main Figures and Tables/dropped_time_series_differences.png",
  plot = temp$plot_differences,
  width = 8,
  height = 6,
  units = "in",
  dpi = 500
)

ggsave(
  "Main Figures and Tables/dropped_time_series_cumul_difs.png",
  plot = temp$plot_cumul_differences,
  width = 8,
  height = 6,
  units = "in",
  dpi = 500
)


#plot <- temp$plot_differences
#ggsave("Main Figures and Tables/dropped_time_series_differences.png", plot = plot, dpi=500)

plot <- temp$plot_ts
plot
ggsave("Main Figures and Tables/no_time_series_plot.png", plot = plot, dpi=500)

#plot <- temp$plot_cumul_differences
#ggsave("Main Figures and Tables/dropped_time_series_cumul_difs.png", plot = plot, dpi=500)


temp$plot_differences
temp$plot_ts
temp$plot_cumul_differences



library(dplyr)
library(tibble)
library(knitr)
library(zoo)

# helper functions
pstars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

fmt_pct <- function(x, digits = 2) {
  paste0(sprintf(paste0("%.", digits, "f"), x * 100), "\\%")
}

fmt_num <- function(x) {
  format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(x) {
  format(signif(x, 2), scientific = TRUE, trim = TRUE)
}



# Use the preferred BSTS total-passenger estimates reported in Figure 5b.
# When Master.R is run, All Arrivals.R creates total_t immediately before this script.
if (exists("total_t")) {
  post_months <- 9
  preferred_cumulative_impact <- as.numeric(total_t[["9-Month Cumulative Impact"]])

  temp_bsts <- list(
    loss_percent      = as.numeric(total_t[["Relative Impact (%)"]]) / 100,
    monthly_impact    = preferred_cumulative_impact / post_months,
    cumulative_impact = preferred_cumulative_impact,
    p_value           = as.numeric(total_t[["p-value"]]),
    root_mspe         = as.numeric(total_t[["RMSE (priors)"]])
  )
} else {
  # Fallback for sourcing this file by itself after Primary Arrivals.R.
  bsts_summary <- impact_temp$summary

  series_df <- as.data.frame(impact_temp$series)
  series_df$date <- as.Date(paste0("01 ", rownames(series_df)), format = "%d %b %Y")

  pre_start <- as.Date("2014-01-01")
  pre_end   <- as.Date("2019-01-01")

  bsts_rmspe <- series_df %>%
    filter(date >= pre_start, date <= pre_end) %>%
    summarize(rmspe = sqrt(mean((response - point.pred)^2, na.rm = TRUE))) %>%
    pull(rmspe)

  temp_bsts <- list(
    loss_percent      = bsts_summary$RelEffect[1],
    monthly_impact    = bsts_summary$AbsEffect[1],
    cumulative_impact = bsts_summary$AbsEffect[2],
    p_value           = bsts_summary$p[1],
    root_mspe         = bsts_rmspe
  )
}



forecast_tbl <- tibble(
  `Model Used` = c(
    "Simple OLS Regression, Time Series Omitted",
    "BSTS Preferred Model"
  ),
  `Loss Percent` = c(
    paste0(fmt_pct(temp$loss_percent), pstars(temp$p_value)),
    paste0(fmt_pct(temp_bsts$loss_percent), pstars(temp_bsts$p_value))
  ),
  `Monthly Impact` = c(
    fmt_num(temp$monthly_impact),
    fmt_num(temp_bsts$monthly_impact)
  ),
  `Cumulative Impact` = c(
    fmt_num(temp$cumulative_impact),
    fmt_num(temp_bsts$cumulative_impact)
  ),
  `P-Value` = c(
    fmt_p(temp$p_value),
    fmt_p(temp_bsts$p_value)
  ),
  `RMSPE` = c(
    fmt_num(temp$root_mspe),
    fmt_num(temp_bsts$root_mspe)
  )
)


tabular_only <- kable(
  forecast_tbl,
  format = "latex",
  booktabs = FALSE,
  escape = FALSE,
  align = c("l", "l", "r", "r", "r", "r"),
  col.names = c(
    "\\textbf{Model Used}",
    "Loss Percent",
    "Monthly Impact",
    "Cumulative Impact",
    "P-Value",
    "RMSPE"
  )
)

writeLines(tabular_only, "Main Figures and Tables/drop_time_series.tex")
cat(tabular_only)
