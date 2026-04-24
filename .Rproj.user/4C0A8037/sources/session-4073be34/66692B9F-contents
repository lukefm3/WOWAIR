#{r Country Placebo Check Greenland}
complete <- month[,c(tourism_list)] # Restart the formula
# what is the selected indicator's column?
#colnames(complete)[12]
complete2 <- as.data.frame(complete[64]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_greenland"
complete <- complete[,-c(grep('iceland', colnames(complete)), grep('greenland', colnames(complete)))]


complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2018-01-01') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_greenland) %>% relocate(date)

temp<- ts(data = complete[-c(1)],
          start = 2018,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2018-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

country_placebo_g <- table_func(impact_temp, "Greenland")
#

#{r Country Placebo Check Faroe}
complete <- month[,c(tourism_list)] # Restart the formula
# what is the selected indicator's column?
#colnames(complete)[12]
complete2 <- as.data.frame(complete[69]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_faroe"
complete <- complete[,-c(grep('iceland', colnames(complete)), grep('faroe', colnames(complete)))]


complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2018-01-01') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_faroe) %>% relocate(date)


temp<- ts(data = complete[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2018-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

country_placebo_f <- table_func(impact_temp, "Faroe Islands")
#

#{r Country Placebo Check denmark}
complete <- month[,c(tourism_list)] # Restart the formula
# what is the selected indicator's column?
#colnames(complete)[12]
complete2 <- as.data.frame(complete[72]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_denmark"
complete <- complete[,-c(grep('iceland', colnames(complete)), grep('denmark', colnames(complete)))]


complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_denmark) %>% relocate(date)


temp<- ts(data = complete[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

country_placebo_d <- table_func(impact_temp, "Denmark")

#

#{r Country Placebo Check norway}
complete <- month[,c(tourism_list)] # Restart the formula
# what is the selected indicator's column?
#colnames(complete)[12]
complete2 <- as.data.frame(complete[77]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_norway"
complete <- complete[,-c(grep('iceland', colnames(complete)), grep('norway', colnames(complete)))]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_norway) %>% relocate(date)


temp<- ts(data = complete[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

country_placebo_n <- table_func(impact_temp, "Norway")
#

#{r Country Placebo Check Table}
country_placebo <- rbind(country_placebo_g, country_placebo_f, country_placebo_d, country_placebo_n)

temp <- as.data.frame(t(colMeans(country_placebo[, , drop = FALSE])))
rownames(temp) <- c("Mean")

# Calculate the mean of each column
country_placebo <- rbind(country_placebo,temp)

country_placebo$`Placebo Country` <- rownames(country_placebo)
country_placebo <- country_placebo%>% select("Placebo Country", everything(),-`RMSE (priors)`)



kable(country_placebo, caption = "Placebo Country Test", format = "markdown", row.names = FALSE)
writeLines(kable(country_placebo, caption = "Placebo Country Test", format = "markdown", row.names = FALSE), "placebo_country_test.md")
View(country_placebo)
#

