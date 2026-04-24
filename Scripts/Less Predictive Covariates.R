#{r Arrival Robustness to Covariates Base}

complete <- month[,c(tourism_list)] # Restart the formula
# what is the selected indicator's column?
#colnames(complete)[12]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]


# Filtered out top 3 covariates and run again
#######

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

variable_drop <- table_func(impact_temp, "All Variables")

check <- coeff_plot(impact_temp)

#####
# This section must be run after a coeff_plot command
#####

check <- check %>%
  arrange(desc(inclusion_probability)) %>%
  head(3) 
View(check)

#

#{r Arrival Robustness to Covariates missing 3}

# Filter out columns in `complete` that match the values in `robust_to_coeffs_list`
complete_minus <- as.data.frame(complete) %>%
  select(-any_of(as.character(check[1, 1]))) %>% select(-any_of(as.character(check[2, 1]))) %>% select(-any_of(as.character(check[3, 1])))


temp<- ts(data = complete_minus[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

variable_drop_1 <- table_func(impact_temp, "Omit Top 3 Covariates")
coeff_plot(impact_temp)
#

#{r Arrival Robustness to Covariates missing 2}
# Filter out columns in `complete` that match the values in `robust_to_coeffs_list`
complete_minus <- as.data.frame(complete) %>%
  select(-any_of(as.character(check[1, 1]))) %>% select(-any_of(as.character(check[2, 1])))



temp<- ts(data = complete_minus[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

variable_drop_2 <- table_func(impact_temp, "Omit Top 2 Covariates")

#

#{r Arrival Robustness to Covariates missing 1}
# Filter out columns in `complete` that match the values in `robust_to_coeffs_list`
complete_minus <- as.data.frame(complete) %>%
  select(-any_of(as.character(check[1, 1]))) 



temp<- ts(data = complete_minus[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

variable_drop_3 <- table_func(impact_temp, "Omit Top Covariate")

#

#{r Arrival Robustness to Covariates Only 3}
icelandcols <- complete[,c(1,2)]
# Filter out columns in `complete` that match the values in `robust_to_coeffs_list`
complete_minus <- as.data.frame(complete) %>%
  select(as.character(check[1, 1]),as.character(check[2, 1]),as.character(check[3, 1]))
icelandcols <- complete[,c(1,2)]

complete_minus <- cbind(icelandcols,complete_minus)

temp<- ts(data = complete_minus[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

variable_drop_4 <- table_func(impact_temp, "Only Top 3 Covariates")

#

#{r Arrival Robustness to Covariates only 2}
icelandcols <- complete[,c(1,2)]
# Filter out columns in `complete` that match the values in `robust_to_coeffs_list`
complete_minus <- as.data.frame(complete) %>%
  select(as.character(check[1, 1]),as.character(check[2, 1]))
icelandcols <- complete[,c(1,2)]

complete_minus <- cbind(icelandcols,complete_minus)

temp<- ts(data = complete_minus[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

variable_drop_5 <- table_func(impact_temp, "Only Top 2 Covariates")

#

#{r Arrival Robustness to Covariates only 1}
icelandcols <- complete[,c(1,2)]
# Filter out columns in `complete` that match the values in `robust_to_coeffs_list`
complete_minus <- as.data.frame(complete) %>%
  select(as.character(check[1, 1]))
icelandcols <- complete[,c(1,2)]

complete_minus <- cbind(icelandcols,complete_minus)

temp<- ts(data = complete_minus[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

variable_drop_6 <- table_func(impact_temp, "Only Top Covariate")

#

#{r Variable Dropping}
dropped_vars <- rbind(variable_drop,variable_drop_1,variable_drop_2,variable_drop_3,variable_drop_4,variable_drop_5,variable_drop_6)

temp <- as.data.frame(t(colMeans(dropped_vars[, , drop = FALSE])))
rownames(temp) <- c("Mean")

# Calculate the mean of each column
dropped_vars <- rbind(dropped_vars,temp)

dropped_vars$`Added/Dropped Covariate` <- rownames(dropped_vars)
dropped_vars <- dropped_vars%>% select("Added/Dropped Covariate", everything())



kable(dropped_vars, caption = "Over/Under Specifications", format = "markdown", row.names = FALSE)
writeLines(kable(dropped_vars, caption = "Over/Under Specifications", format = "markdown", row.names = FALSE), "Additional Tables and Figures/dropped_covariates.md")
#

