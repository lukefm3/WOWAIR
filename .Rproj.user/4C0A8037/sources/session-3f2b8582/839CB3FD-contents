

#{r Monthly Exorts}
complete <- month[,c(accounting_list)] # Restart the formula
complete2 <- as.data.frame(complete[,40]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period,model.args=list(niter=5000, nseasons = 12, season.duration = 1))

plot(impact_temp) + ggtitle("Exports, in Icelandic Currency")
summary(impact_temp)
#

#{r Monthly Imports}
complete <- month[,c(accounting_list)] # Restart the formula
complete2 <- as.data.frame(complete[,43]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"

icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period,model.args=list(niter=5000, nseasons = 12, season.duration = 1))

plot(impact_temp) + ggtitle("Imports, in Icelandic Currency")
summary(impact_temp)

