View(BIKESHARE)
attach(BIKESHARE)
detach(BIKESHARE)

head(BIKESHARE)

boxplot(casual~day, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~day, data = BIKESHARE, varwidth = TRUE)

boxplot(casual~holiday, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~holiday, data = BIKESHARE, varwidth = TRUE)

boxplot(casual~weather, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~weather, data = BIKESHARE, varwidth = TRUE)

boxplot(casual~season, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~season, data = BIKESHARE, varwidth = TRUE)

boxplot(casual~hr, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~hr, data = BIKESHARE, varwidth = TRUE)

boxplot(casual~temp, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~temp, data = BIKESHARE, varwidth = TRUE)

boxplot(casual~feelslike, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~feelslike, data = BIKESHARE, varwidth = TRUE)

boxplot(casual~windspeed, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~windspeed, data = BIKESHARE, varwidth = TRUE)

boxplot(casual~hum, data = BIKESHARE, varwidth = TRUE)
boxplot(registered~hum, data = BIKESHARE, varwidth = TRUE)

model = lm(casual ~ . - registered, data = BIKESHARE)
summary(model)

model2 = lm(casual ~ . - registered - feelslike, poly(temp, degree = 2, raw = TRUE) + poly(hr, degree = 2, raw = TRUE) + poly(weather, degree = 2, raw = TRUE), data = BIKESHARE)
summary(model2)

IsSaturdayOrSunday = ifelse((day==7) | (day==1), 1, 0)

model3 = lm(casual ~ . - registered + IsSaturdayOrSunday*poly(temp, degree = 2, raw = TRUE), poly(hr, degree = 2, raw = TRUE) + poly(weather, degree = 2, raw = TRUE), data = BIKESHARE)
summary(model3)

model4 = lm(casual ~ . - registered + IsSaturdayOrSunday*poly(hr, degree = 2, raw = TRUE), data = BIKESHARE)
summary(model4)

model5 = lm(sqrt(casual) ~ . - registered - feelslike + IsSaturdayOrSunday*poly(hr, degree = 2, raw = TRUE) + poly(temp, degree = 2, raw = TRUE) + as.factor(season) + as.factor(holiday) + as.factor(weather), data = BIKESHARE)
summary(model5)

isPeakHour = ifelse((hr==7|hr==8|hr==17|hr==18), 1, 0)

model6 = lm(registered ~ . - casual - feelslike - windspeed + temp*isPeakHour*poly(day, degree = 2, raw = TRUE) + as.factor(season) + as.factor(weather), data = BIKESHARE)
summary(model6)

plot(model5)


plot(model6)


BIKESHARE_quantitative = subset(BIKESHARE, select = c(registered,hr,holiday,day,weather,temp,feelslike,hum,windspeed))
cor(BIKESHARE_quantitative)

library("car")
crPlots(model)
