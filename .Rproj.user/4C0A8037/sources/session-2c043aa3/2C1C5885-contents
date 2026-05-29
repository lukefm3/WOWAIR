
# No Greenland
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]
complete <- complete[,-c(grep('greenland', colnames(complete)))]

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

country_drop_1 <- table_func(impact_temp, "Greenland")

# No Faroe
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]
complete <- complete[,-c(grep('faroe', colnames(complete)))]

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

country_drop_2 <- table_func(impact_temp, "Faroe Islands")

# No Denmark
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]
complete <- complete[,-c(grep('denmark', colnames(complete)))]

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

country_drop_3 <- table_func(impact_temp, "Denmark")

# No Norway
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]
complete <- complete[,-c(grep('norway', colnames(complete)))]

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

country_drop_4 <- table_func(impact_temp, "Norway")
# Ommitting Greenland & Faroe}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns,
                         grep('faroe', colnames(complete)), 
                         grep('greenland', colnames(complete)))]

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

country_drop_5 <- table_func(impact_temp, "Faroe Islands & Greenland")

# Ommitting Greenland & Denmark}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns,
                         grep('denmark', colnames(complete)), 
                         grep('greenland', colnames(complete)))]

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

country_drop_6 <- table_func(impact_temp, "Greenland & Denmark")

# Ommitting Greenland & Norway}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns,
                         grep('norway', colnames(complete)), 
                         grep('greenland', colnames(complete)))]

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

country_drop_7 <- table_func(impact_temp, "Greenland & Norway")

# Ommitting Denmark & Faroe}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns,
                         grep('faroe', colnames(complete)), 
                         grep('denmark', colnames(complete)))]

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

country_drop_7 <- table_func(impact_temp, "Denmark & Faroe Islands")
# Ommitting Denmark & Norway}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns,
                         grep('norway', colnames(complete)), 
                         grep('denmark', colnames(complete)))]

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

country_drop_8 <- table_func(impact_temp, "Denmark & Norway")
# Ommitting Norway & Faroe}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns,
                         grep('norway', colnames(complete)), 
                         grep('faroe', colnames(complete)))]

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

country_drop_9 <- table_func(impact_temp, "Norway & Faroe Islands")

# Variable Choice Table
dropped_countries <- rbind(country_drop_1, country_drop_2, country_drop_3, country_drop_4, country_drop_5, country_drop_6, country_drop_7, country_drop_8, country_drop_9)

temp <- as.data.frame(t(colMeans(dropped_countries[, , drop = FALSE])))
rownames(temp) <- c("Mean")

# Calculate the mean of each column
dropped_countries <- rbind(dropped_countries,temp)

dropped_countries$`Omitted Control Country` <- rownames(dropped_countries)
dropped_countries <- dropped_countries %>% select("Omitted Control Country", everything())

# round columns 2 to 2 decimlal places and 6 to 4 decimals. round columns 3 to 5 and 7 to 0 decimal places
dropped_countries <- dropped_countries %>%
  mutate(across(2, ~ round(.x, 2))) %>%
  mutate(across(c(3:5, 7), ~ round(.x, 0))) %>%
  mutate(across(6, ~ round(.x, 4)))

dropped_countries <- dropped_countries %>%
  mutate(`Relative Impact (%)` =
           sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)))



#kable(dropped_countries, caption = "Choice of Control Countries", format = "markdown", row.names = FALSE)
#writeLines(kable(dropped_countries, caption = "Choice of Control Countries", format = "latex", row.names = FALSE), "Main Figures and Tables/choice_of_control_countries.tex")


latex_out <- kable(
  dropped_countries,
  caption = "Choice of Control Countries",
  format = "latex",
  row.names = FALSE
)

# Split into lines
latex_lines <- strsplit(latex_out, "\n")[[1]]

# Remove only exact wrapper/caption/centering lines
latex_lines <- latex_lines[
  !grepl("^\\\\begin\\{table\\}", latex_lines) &
    !grepl("^\\\\end\\{table\\}", latex_lines) &
    !grepl("^\\\\centering$", latex_lines) &
    !grepl("^\\\\caption\\{", latex_lines)
]

# Save cleaned table
writeLines(
  latex_lines,
  "Main Figures and Tables/choice_of_control_countries.tex")

