# Transportation Measures


#{r Rental Vehicles}
complete <- month[,c(tourism_list)] # Restart the formula
complete2 <- as.data.frame(complete[23]) # change this to pick out your indicator,
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

T1_table <- table_func(impact_temp, "Registered Rental Vehicles")

T1 <- plot(impact_temp, "pointwise") + ggtitle("Registered Rental Vehicles") + theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"),
                                                                                     plot.title = element_text(size = 10, vjust = -9, hjust = 0),
                                                                                     axis.text.x = element_text(size = 8),  # Adjust x-axis label size
                                                                                     axis.text.y = element_text(size = 8),  # Adjust y-axis label size
                                                                                     axis.title.y = element_text(size = 8)) + ylab("Vehicles Rented")

#

#{r Fuel Use Local}
complete <- month[,c(tourism_list)] # Restart the formula
complete2 <- as.data.frame(complete[,61]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2015-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)



temp <- ts(data = complete[-c(1)],
           start = 2016,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2016-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, , model.args=list(niter=5000, nseasons = 12, season.duration = 1))

T4_table <- table_func(impact_temp, "Local Fuel Purchases (at Pump)")

T4 <- plot(impact_temp, "pointwise") + ggtitle("Local Fuel Purchases (at Pump)") + theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"),
                                                                                         plot.title = element_text(size = 10, vjust = -9, hjust = 0),
                                                                                         axis.text.x = element_text(size = 8),  # Adjust x-axis label size
                                                                                         axis.text.y = element_text(size = 8),  # Adjust y-axis label size
                                                                                         axis.title.y = element_text(size = 8)) + ylab("Volume (m3)")
#

#{r Fuel Use Foreign}
complete <- month[,c(tourism_list)] # Restart the formula
complete2 <- as.data.frame(complete[,60]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2015-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2016,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2016-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, , model.args=list(niter=5000, nseasons = 12, season.duration = 1))

T3_table <- table_func(impact_temp, "Foreign Fuel Purcahses (at Pump)")

T3 <- plot(impact_temp, "pointwise") + ggtitle("Foreign Fuel Purchases (at Pump)") + theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"),
                                                                                           plot.title = element_text(size = 10, vjust = -9, hjust = 0),
                                                                                           axis.text.x = element_text(size = 8),  # Adjust x-axis label size
                                                                                           axis.text.y = element_text(size = 8),  # Adjust y-axis label size
                                                                                           axis.title.y = element_text(size = 8)) + ylab("Volume (m3)")
#

#{r Fuel Use Total}
complete <- month[,c(tourism_list)] # Restart the formula
complete2 <- as.data.frame(complete[,62]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2015-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)



temp <- ts(data = complete[-c(1)],
           start = 2016,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2016-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

T2_table <- table_func(impact_temp, "Total Fuel Purchases (at Pump)")

T2 <- plot(impact_temp, "pointwise") + ggtitle("Total Fuel Purchases (at Pump)") + theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"),
                                                                                         plot.title = element_text(size = 10, vjust = -9, hjust = 0),
                                                                                         axis.text.x = element_text(size = 8),  # Adjust x-axis label size
                                                                                         axis.text.y = element_text(size = 8),  # Adjust y-axis label size
                                                                                         axis.title.y = element_text(size = 8)) + ylab("Volume (m3)")

#

#{r Pointwise AND a Table for these}
vehicle_impact <- grid.arrange(T1,T2,T3,T4)

ggsave("vehicle_impact_plot.png",vehicle_impact, width=10,height=5, units="in")

variable_choice <- rbind(T1_table, T2_table, T3_table, T4_table)


variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Forecasted Impacts to Road Transport", format = "markdown", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Forecasted Impacts to Road Transport", format = "markdown", row.names = FALSE), "hotel_impacts_table.md")

#
