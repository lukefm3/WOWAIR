
#{r Overnight Stays Iceland}
complete <- month[,c(tourism_list)] # Restart the formula
complete2 <- as.data.frame(complete[12]) # change this to pick out your indicator,
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
H1 <- plot(impact_temp, "pointwise") + ggtitle("Overnight Stays in Hotels") +       theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"),
                                                                                          plot.title = element_text(size = 10, vjust = -9, hjust = 0),
                                                                                          axis.text.x = element_text(size = 8),  # Adjust x-axis label size
                                                                                          axis.text.y = element_text(size = 8),  # Adjust y-axis label size
                                                                                          axis.title.y = element_text(size = 8)) + ylab("Overnight Stays in Hotels")

H1_table <- table_func_avg(impact_temp, "Overnight Stays in Hotels")


#

#{r Checkins Total}
complete <- month[,c(tourism_list)] # Restart the formula
complete2 <- as.data.frame(complete[9]) # change this to pick out your indicator,
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
H2 <- plot(impact_temp, "pointwise") + ggtitle("Hotel Check-ins") +       theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"),
                                                                                plot.title = element_text(size = 10, vjust = -9, hjust = 0),
                                                                                axis.text.x = element_text(size = 8),  # Adjust x-axis label size
                                                                                axis.text.y = element_text(size = 8),  # Adjust y-axis label size
                                                                                axis.title.y = element_text(size = 8)) + ylab("Check-ins")
H2_table <- table_func_avg(impact_temp, "Hotel Check-ins")

#

#{r Occupancy Rate}
complete <- month[,c(tourism_list)] # Restart the formula
complete2 <- as.data.frame(complete[7]) # change this to pick out your indicator,
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
H3 <- plot(impact_temp, "pointwise") + ggtitle("Hotel Occupancy Rate") + theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"),
                                                                               plot.title = element_text(size = 10, vjust = -9, hjust = 0),
                                                                               axis.text.x = element_text(size = 8),  # Adjust x-axis label size
                                                                               axis.text.y = element_text(size = 8),  # Adjust y-axis label size
                                                                               axis.title.y = element_text(size = 8)) + ylab("Occupancy Rate")
H3_table <- table_func_avg(impact_temp, "Hotel Occupancy Rate")

#

#{r Pointwise AND a Table for these}
hotel_impact <- grid.arrange(H1, H2, H3)

ggsave("hotel_impact_plot.png",hotel_impact, width=10,height=5, units="in")

variable_choice <- rbind(H1_table, H2_table, H3_table)

temp <- as.data.frame(t(colMeans(variable_choice[, , drop = FALSE])))
rownames(temp) <- c("Mean")

# Calculate the mean of each column
variable_choice <- rbind(variable_choice,temp)

variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Forecasted Impacts to Hotel Accommodations", format = "markdown", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Forecasted Impacts to Hotel Accommodations", format = "markdown", row.names = FALSE), "Main Figures and Tables/hotel_impacts_table.md")

#
