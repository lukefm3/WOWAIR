
# Create a DID Regression to Compare To

# Yit <- β0 + β1*Postt + β2*Treati + β3*(Postt*Treati) + β4*FixedEffecti + β5*FixedEffectt(year) + β6*FixedEffectt(month) + εit

# Simple DID 1 year prior

passengers_only_list <- c(1,7,11,72,122,123)

month_did <- month[,passengers_only_list]

colnames(month_did) <- c("date", "greenland","iceland","faroe","norway","denmark")
month_did_long <- pivot_longer(month_did, cols = -date, names_to = "country", values_to = "pass_arrivals")
month_did_long$post <- ifelse(month_did_long$date>"2018-03-01",1,0)
month_did_long$treat <- ifelse(month_did_long$country == "iceland",1,0)
month_did_long$fixed_ef <- interaction(month_did_long$country, month_did_long$date)
month_did_long$month <- month(month_did_long$date)
month_did_long$year <- year(month_did_long$date)

month_did_long <- month_did_long %>% filter(date<"2020-01-01") %>% filter(date > "2017-12-31")

month_did_long$log_arrivals <- log(month_did_long$pass_arrivals)

model2 <- lm(log_arrivals ~ (country=="iceland") + post + (country=="iceland"):post + factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

# Comparatively...
model3 <- lm(log_arrivals ~ (country=="greenland") + post + (country=="greenland"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model4 <- lm(log_arrivals ~ (country=="faroe") + post + (country=="faroe"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model5 <- lm(log_arrivals ~ (country=="norway") + post + (country=="norway"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model6 <- lm(log_arrivals ~ (country=="denmark") + post + (country=="denmark"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)



tidy_model <- rbind(tidy(model2)[20,], tidy(model3)[20,], tidy(model4)[20,], tidy(model5)[20,], tidy(model6)[20,])

# Add stars to the estimate based on p-values
tidy_model$estimate <- paste0(round(tidy_model$estimate,4), ifelse(tidy_model$p.value < 0.001, "***", ifelse(tidy_model$p.value < 0.01, "**", ifelse(tidy_model$p.value < 0.05, "*", ""))))

table_prior_1 <-  tidy_model[,c(1,2)]


# Simple DID 2 year prior

passengers_only_list <- c(1,7,11,72,122,123)

month_did <- month[,passengers_only_list]

colnames(month_did) <- c("date", "greenland","iceland","faroe","norway","denmark")
month_did_long <- pivot_longer(month_did, cols = -date, names_to = "country", values_to = "pass_arrivals")
month_did_long$post <- ifelse(month_did_long$date>"2018-03-01",1,0)
month_did_long$treat <- ifelse(month_did_long$country == "iceland",1,0)
month_did_long$fixed_ef <- interaction(month_did_long$country, month_did_long$date)
month_did_long$month <- month(month_did_long$date)
month_did_long$year <- year(month_did_long$date)

month_did_long <- month_did_long %>% filter(date<"2020-01-01") %>% filter(date > "2016-12-31")

month_did_long$log_arrivals <- log(month_did_long$pass_arrivals)

model2 <- lm(log_arrivals ~ (country=="iceland") + post + (country=="iceland"):post + factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)


# Comparatively...
model3 <- lm(log_arrivals ~ (country=="greenland") + post + (country=="greenland"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model4 <- lm(log_arrivals ~ (country=="faroe") + post + (country=="faroe"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model5 <- lm(log_arrivals ~ (country=="norway") + post + (country=="norway"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model6 <- lm(log_arrivals ~ (country=="denmark") + post + (country=="denmark"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

tidy_model <- rbind(tidy(model2)[21,], tidy(model3)[21,], tidy(model4)[21,], tidy(model5)[21,], tidy(model6)[21,])

# Add stars to the estimate based on p-values
tidy_model$estimate <- paste0(round(tidy_model$estimate,4), ifelse(tidy_model$p.value < 0.001, "***", ifelse(tidy_model$p.value < 0.01, "**", ifelse(tidy_model$p.value < 0.05, "*", ""))))

table_prior_2 <-  tidy_model[,c(1,2)]

# Simple DID 3 year prior

passengers_only_list <- c(1,7,11,72,122,123)

month_did <- month[,passengers_only_list]

colnames(month_did) <- c("date", "greenland","iceland","faroe","norway","denmark")
month_did_long <- pivot_longer(month_did, cols = -date, names_to = "country", values_to = "pass_arrivals")
month_did_long$post <- ifelse(month_did_long$date>"2018-03-01",1,0)
month_did_long$treat <- ifelse(month_did_long$country == "iceland",1,0)
month_did_long$fixed_ef <- interaction(month_did_long$country, month_did_long$date)
month_did_long$month <- month(month_did_long$date)
month_did_long$year <- year(month_did_long$date)

month_did_long <- month_did_long %>% filter(date<"2020-01-01") %>% filter(date > "2015-12-31")

month_did_long$log_arrivals <- log(month_did_long$pass_arrivals)

model2 <- lm(log_arrivals ~ (country=="iceland") + post + (country=="iceland"):post + factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

# Comparatively...
model3 <- lm(log_arrivals ~ (country=="greenland") + post + (country=="greenland"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model4 <- lm(log_arrivals ~ (country=="faroe") + post + (country=="faroe"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model5 <- lm(log_arrivals ~ (country=="norway") + post + (country=="norway"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model6 <- lm(log_arrivals ~ (country=="denmark") + post + (country=="denmark"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

View(tidy(model2))
tidy_model <- rbind(tidy(model2)[22,], tidy(model3)[22,], tidy(model4)[22,], tidy(model5)[22,], tidy(model6)[22,])


# Add stars to the estimate based on p-values
tidy_model$estimate <- paste0(round(tidy_model$estimate,4), ifelse(tidy_model$p.value < 0.001, "***", ifelse(tidy_model$p.value < 0.01, "**", ifelse(tidy_model$p.value < 0.05, "*", ""))))

table_prior_3 <-  tidy_model[,c(1,2)]


# Simple DID 4 year prior

passengers_only_list <- c(1,7,11,72,122,123)

month_did <- month[,passengers_only_list]

colnames(month_did) <- c("date", "greenland","iceland","faroe","norway","denmark")
month_did_long <- pivot_longer(month_did, cols = -date, names_to = "country", values_to = "pass_arrivals")
month_did_long$post <- ifelse(month_did_long$date>"2018-03-01",1,0)
month_did_long$treat <- ifelse(month_did_long$country == "iceland",1,0)
month_did_long$fixed_ef <- interaction(month_did_long$country, month_did_long$date)
month_did_long$month <- month(month_did_long$date)
month_did_long$year <- year(month_did_long$date)

month_did_long <- month_did_long %>% filter(date<"2020-01-01") %>% filter(date > "2014-12-31")

month_did_long$log_arrivals <- log(month_did_long$pass_arrivals)

model2 <- lm(log_arrivals ~ (country=="iceland") + post + (country=="iceland"):post + factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

# Comparatively...
model3 <- lm(log_arrivals ~ (country=="greenland") + post + (country=="greenland"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model4 <- lm(log_arrivals ~ (country=="faroe") + post + (country=="faroe"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model5 <- lm(log_arrivals ~ (country=="norway") + post + (country=="norway"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model6 <- lm(log_arrivals ~ (country=="denmark") + post + (country=="denmark"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

tidy_model <- rbind(tidy(model2)[23,], tidy(model3)[23,], tidy(model4)[23,], tidy(model5)[23,], tidy(model6)[23,])


# Add stars to the estimate based on p-values
tidy_model$estimate <- paste0(round(tidy_model$estimate,4), ifelse(tidy_model$p.value < 0.001, "***", ifelse(tidy_model$p.value < 0.01, "**", ifelse(tidy_model$p.value < 0.05, "*", ""))))

table_prior_4 <-  tidy_model[,c(1,2)]

# Simple DID 5 year prior

passengers_only_list <- c(1,7,11,72,122,123)

month_did <- month[,passengers_only_list]

colnames(month_did) <- c("date", "greenland","iceland","faroe","norway","denmark")
month_did_long <- pivot_longer(month_did, cols = -date, names_to = "country", values_to = "pass_arrivals")
month_did_long$post <- ifelse(month_did_long$date>"2018-03-01",1,0)
month_did_long$treat <- ifelse(month_did_long$country == "iceland",1,0)
month_did_long$fixed_ef <- interaction(month_did_long$country, month_did_long$date)
month_did_long$month <- month(month_did_long$date)
month_did_long$year <- year(month_did_long$date)

month_did_long <- month_did_long %>% filter(date<"2020-01-01") %>% filter(date > "2013-12-31")

month_did_long$log_arrivals <- log(month_did_long$pass_arrivals)

model2 <- lm(log_arrivals ~ (country=="iceland") + post + (country=="iceland"):post + factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

# Comparatively...
model3 <- lm(log_arrivals ~ (country=="greenland") + post + (country=="greenland"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model4 <- lm(log_arrivals ~ (country=="faroe") + post + (country=="faroe"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model5 <- lm(log_arrivals ~ (country=="norway") + post + (country=="norway"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model6 <- lm(log_arrivals ~ (country=="denmark") + post + (country=="denmark"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

tidy_model <- rbind(tidy(model2)[24,], tidy(model3)[24,], tidy(model4)[24,], tidy(model5)[24,], tidy(model6)[24,])


# Add stars to the estimate based on p-values
tidy_model$estimate <- paste0(round(tidy_model$estimate,4), ifelse(tidy_model$p.value < 0.001, "***", ifelse(tidy_model$p.value < 0.01, "**", ifelse(tidy_model$p.value < 0.05, "*", ""))))

table_prior_5 <-  tidy_model[,c(1,2)]


# Simple DID 6 year prior

passengers_only_list <- c(1,7,11,72,122,123)

month_did <- month[,passengers_only_list]

colnames(month_did) <- c("date", "greenland","iceland","faroe","norway","denmark")
month_did_long <- pivot_longer(month_did, cols = -date, names_to = "country", values_to = "pass_arrivals")
month_did_long$post <- ifelse(month_did_long$date>"2018-03-01",1,0)
month_did_long$treat <- ifelse(month_did_long$country == "iceland",1,0)
month_did_long$fixed_ef <- interaction(month_did_long$country, month_did_long$date)
month_did_long$month <- month(month_did_long$date)
month_did_long$year <- year(month_did_long$date)

month_did_long <- month_did_long %>% filter(date<"2020-01-01") %>% filter(date > "2012-12-31")

month_did_long$log_arrivals <- log(month_did_long$pass_arrivals)

model2 <- lm(log_arrivals ~ (country=="iceland") + post + (country=="iceland"):post + factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

# Comparatively...
model3 <- lm(log_arrivals ~ (country=="greenland") + post + (country=="greenland"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model4 <- lm(log_arrivals ~ (country=="faroe") + post + (country=="faroe"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model5 <- lm(log_arrivals ~ (country=="norway") + post + (country=="norway"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

model6 <- lm(log_arrivals ~ (country=="denmark") + post + (country=="denmark"):post +  factor(country) + factor(year(date)) + factor(month(date)), data = month_did_long)

tidy_model <- rbind(tidy(model2)[25,], tidy(model3)[25,], tidy(model4)[25,], tidy(model5)[25,], tidy(model6)[25,])


# Add stars to the estimate based on p-values
tidy_model$estimate <- paste0(round(tidy_model$estimate,4), ifelse(tidy_model$p.value < 0.001, "***", ifelse(tidy_model$p.value < 0.01, "**", ifelse(tidy_model$p.value < 0.05, "*", ""))))

table_prior_6 <-  tidy_model[,c(1,2)]

# cbind and print DID table

DID_table <- cbind(table_prior_1[,2],table_prior_2[,2],table_prior_3[,2],table_prior_4[,2],table_prior_5[,2],table_prior_6[,2])
rownames(DID_table) <- c("Iceland","Greenland","Faroe Islands","Norway","Denmark")
colnames(DID_table) <- c("1 year prior","2 years prior","3 years prior","4 years prior","5 years prior","6 years prior")

# print the DID_table with knitr and save
kable(DID_table, format = "markdown")

DID_table <- kable(DID_table, format = "latex")

writeLines(DID_table, "Main Figures and Tables/DID_table.tex")

DID_table
