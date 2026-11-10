# rmse function

calculate_rmse <- function(x) {
  # Filter the zoo object
  temp <- window(x$series[, c(1, 3)], end = x$model$pre.period[2])
  # Calculate the RMSE
  error <- sqrt(mean((temp[, 1] - temp[, 2])^2))
  return(error)
}


# Create a function to take estimated monthly impact, 95% CI, and add it to a table Archive

#results <- impact_temp
# Create a table with the estimated impact and P-Values

table_func <- function(results, name) {
  rmse <- calculate_rmse(results)
  temp_table <- tibble(
    "Relative Impact (%)" = round(results$summary$RelEffect[1]*100,2),
    "9-Month Cumulative Impact" = round(results$summary$AbsEffect[2],0),
    "95% CI Upper" = round(results$summary$AbsEffect.upper[2],0),
    "95% CI Lower" = round(results$summary$AbsEffect.lower[2],0),
    "p-value" = round(results$summary$p[1],4),
    "RMSE (priors)" = round(rmse,0)
  )
  row.names(temp_table)[1] <- name
  # Return the table
  return(temp_table)
}


table_func_avg_yoy <- function(results, name) {
  rmse <- calculate_rmse(results)
  quantity_digits <- ifelse(grepl("Rate", name), 2, 0)
  temp_table <- tibble(
    "Relative Impact (%)" = round(results$summary$RelEffect[1]*100,2),
    "Average Monthly Impact [ISK (M)]" = round(results$summary$AbsEffect[1], quantity_digits),
    "Monthly Avg. Predicted" = round(results$summary$Pred[1], quantity_digits),
    "Monthly Avg. Observed" = round(results$summary$Actual[1], quantity_digits),
    "YoY Actual Change (%)" = round((results$summary$Actual[1] / mean(impact_temp$series$response[c((length(impact_temp$series$response)-20):(length(impact_temp$series$response)-12)),1], na.rm = TRUE) - 1) * 100, 4),
    "p-value" = round(results$summary$p[1],4),
    "RMSE (priors)" = round(rmse, quantity_digits)
  )
  row.names(temp_table)[1] <- name
  # Return the table
  return(temp_table)
}

# Manuscript labor table/interval plot: nine-month cumulative relative effect.
# Keep table_func_avg_yoy unchanged for the other tables that use it.
table_func_avg_yoy_v2 <- function(results, name) {
  response <- as.numeric(results$series$response)
  prior_year_mean <- mean(response[(length(response) - 20):(length(response) - 12)], na.rm = TRUE)
  predicted_total <- as.numeric(results$summary$Pred[2])
  quantity_digits <- if (grepl("Rate", name)) 2 else 0
  if (!is.finite(predicted_total) || predicted_total == 0) {
    stop("Invalid cumulative counterfactual for ", name)
  }
  out <- data.frame(
    "Relative Impact (%)" = 100 * as.numeric(results$summary$AbsEffect[2]) / predicted_total,
    "95% CI Lower (%)" = 100 * as.numeric(results$summary$AbsEffect.lower[2]) / predicted_total,
    "95% CI Upper (%)" = 100 * as.numeric(results$summary$AbsEffect.upper[2]) / predicted_total,
    "YoY Actual Change (%)" = 100 * (as.numeric(results$summary$Actual[1]) / prior_year_mean - 1),
    "Average Monthly Impact" = round(as.numeric(results$summary$AbsEffect[1]), quantity_digits),
    "Monthly Avg. Predicted" = round(as.numeric(results$summary$Pred[1]), quantity_digits),
    "Monthly Avg. Observed" = round(as.numeric(results$summary$Actual[1]), quantity_digits),
    "p-value" = as.numeric(results$summary$p[1]),
    "RMSE (priors)" = round(calculate_rmse(results), quantity_digits),
    check.names = FALSE
  )
  row.names(out) <- name
  out
}


table_func_avg <- function(results, name) {
  rmse <- calculate_rmse(results)
  temp_table <- tibble(
    "Relative Impact (%)" = round(results$summary$RelEffect[1]*100,2),
    "Average Monthly Impact" = round(results$summary$AbsEffect[1],0),
    "95% CI Upper" = round(results$summary$AbsEffect.upper[1],0),
    "95% CI Lower" = round(results$summary$AbsEffect.lower[1],0),
    "p-value" = round(results$summary$p[1],4),
    "RMSE (priors)" = round(rmse,0)  
  )
  row.names(temp_table)[1] <- name
  # Return the table
  return(temp_table)
}



###### Coefficient Plot Function



coeff_plot <- function(var, name_cex = 1.3, title_cex = 1.2) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  # ---- Compute stats ----
  out <- rbind(
    t(as.data.frame(colMeans(var$model$bsts.model$coefficients))),
    t(as.data.frame(colSums(var$model$bsts.model$coefficients != 0))) /
      nrow(var$model$bsts.model$coefficients)
  )
  out <- t(out) |> as_tibble(rownames = "variable")
  names(out)[2:3] <- c("mean_magnitude", "inclusion_probability")
  out$inclusion_probability <- as.numeric(out$inclusion_probability)
  
  # ---- Keep top 15 and sort ----
  out <- out |>
    arrange(desc(inclusion_probability)) |>
    head(15) |>
    arrange(inclusion_probability)
  
  # ---- Clean variable names ----
  out$clean_name <- toupper(gsub("_", " ", out$variable))
  
  # ---- Left plot: Inclusion probabilities ----
  par(mar = c(3, 24, 3, 1))
  inclusion <- barplot(
    out$inclusion_probability,
    horiz = TRUE,
    main = "Inclusion Probabilities",
    las = 1,
    names.arg = rep("", nrow(out)),   # blank; we'll add manually
    cex.main = title_cex              # slightly larger subtitle
  )
  
  # Add left-side y-axis labels (only for this plot)
  axis(side = 2, at = inclusion, labels = out$clean_name,
       las = 1, cex.axis = name_cex, tick = FALSE)
  
  # ---- Right plot: Coefficient magnitudes ----
  par(mar = c(3, 4, 3, 2))
  barplot(
    out$mean_magnitude,
    horiz = TRUE,
    main = "Coefficient Magnitudes",
    xlab = "", ylab = "",
    yaxt = "n",
    names.arg = rep("", nrow(out)),
    cex.main = title_cex              # match subtitle size
  )
  
  # ---- Return for reference ----
  list(covariates = out, inclusion = inclusion)
}




library(dplyr)
library(kableExtra)
library(stringr)

# helper to convert a kable latex table into a sidewaystable with label + notes
make_sidewaystable <- function(kable_obj,
                               label,
                               notes,
                               file) {
  
  latex_code <- as.character(kable_obj)
  
  # change table env to sidewaystable
  latex_code <- latex_code %>%
    str_replace("\\\\begin\\{table\\}", "\\\\begin{sidewaystable}") %>%
    str_replace("\\\\end\\{table\\}", "\\\\end{sidewaystable}")
  
  # add label right after caption
  latex_code <- latex_code %>%
    str_replace(
      "(\\\\caption\\{[^}]+\\})",
      paste0("\\1\n\\\\label{", label, "}")
    )
  
  # wrap tabular in resizebox
  latex_code <- latex_code %>%
    str_replace(
      "\\\\begin\\{tabular\\}",
      "\\\\resizebox{\\\\textwidth}{!}{%\n\\\\begin{tabular}"
    ) %>%
    str_replace(
      "\\\\end\\{tabular\\}",
      "\\\\end{tabular}\n}"
    )
  
  # add tablenotes before closing sidewaystable
  latex_code <- latex_code %>%
    str_replace(
      "\\\\end\\{sidewaystable\\}",
      paste0(
        "\\\\begin{tablenotes}\n",
        "\\\\scriptsize\n",
        "\\\\item \\\\textit{Note:} ", notes, "\n",
        "\\\\end{tablenotes}\n",
        "\\\\end{sidewaystable}"
      )
    )
  
  writeLines(latex_code, file)
}



# Panel Image Format Set
library(grid)
library(gridExtra)
library(ggplot2)

panel_theme <- theme(
  plot.margin = unit(c(.35, .35, .65, .45), "cm"),   # more bottom/left room
  plot.title = element_text(
    size = 10,
    hjust = 0,                                       # left aligned
    vjust = -8,                                    # lower than before
    margin = margin(t = 6, r = 0, b = 0, l = 8)     # slightly right
  ),
  axis.text.x = element_text(size = 8, margin = margin(t = 3)),
  axis.text.y = element_text(size = 8),
  axis.title.y = element_text(size = 8),
  axis.title.x = element_blank()
)














