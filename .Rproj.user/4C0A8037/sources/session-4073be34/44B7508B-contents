
#{r Primary}
complete <- month[,c(tourism_list)] # Restart the formula
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

pre.period <- as.yearmon(c("2014-01-01", "2019-03-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

year_prior <- table_func(impact_temp, "Impact in January 2019")
year_prior

#

#{r 1 Year Prior}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2012-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)
temp<- ts(data = complete[-c(1)],
          start = 2013,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2013-01-01", "2018-03-01"))

post.period <- as.yearmon(c("2018-04-01", "2018-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

year_prior_1 <- table_func(impact_temp, "Impact in January 2018")

#

#{r 2 Years Prior}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2011-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)
temp<- ts(data = complete[-c(1)],
          start = 2012,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2012-01-01", "2017-03-01"))

post.period <- as.yearmon(c("2017-04-01", "2017-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

year_prior_2 <- table_func(impact_temp, "Impact in January 2017")
#

#{r 3 Years Prior}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2010-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)
temp<- ts(data = complete[-c(1)],
          start = 2011,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2011-01-01", "2016-03-01"))

post.period <- as.yearmon(c("2016-04-01", "2016-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

year_prior_3 <- table_func(impact_temp, "Impact in January 2016")
#

#{r 4 Years Prior}
complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2009-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)
temp<- ts(data = complete[-c(1)],
          start = 2010,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2010-01-01", "2015-03-01"))

post.period <- as.yearmon(c("2015-04-01", "2015-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

year_prior_4 <- table_func(impact_temp, "Impact in January 2015")
#

#{r Years_prior_placebo}

prior_placebo <- rbind(year_prior,year_prior_1, year_prior_2, year_prior_3, year_prior_4)

temp <- as.data.frame(t(colMeans(prior_placebo[, , drop = FALSE])))
rownames(temp) <- c("Mean")

# Calculate the mean of each column
prior_placebo <- rbind(prior_placebo ,temp)

prior_placebo$`Artificial Treatment Year` <- rownames(prior_placebo)
prior_placebo  <- prior_placebo %>% select("Artificial Treatment Year", everything())

kable(prior_placebo, caption = "Artificial Treatment Year Test", format = "markdown", row.names = FALSE)
writeLines(kable(prior_placebo, caption = "Artificial Treatment Year Test", format = "markdown", row.names = FALSE), "prior_placebo_test.md")

#
