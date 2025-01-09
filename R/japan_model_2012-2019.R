par(mfrow = c(1, 1))

japan_dt <- readRDS("data/japan_dt.RDS")

japan_zoo <- zoo(japan_dt$visitors, japan_dt$date)
japan_ts <- ts(japan_zoo, start = 1990, frequency = 12)
saveRDS(japan_ts, file = "data/japan_ts.RDS")

japan_filtered <- window(japan_ts, start=c(2012, 1), end=c(2019, 12))
plot(japan_filtered, main="Turystyka w Japonii (2011-2019)", xlab="Czas", ylab="Liczba turystów")
decompose(japan_filtered)
ts_decompose(japan_filtered) 
japan_dt_filtered <- japan_dt[japan_dt$date > as.Date("2011-12-31") & japan_dt$date < as.Date("2019-12-31") ]
plot_ly(japan_dt_filtered[order(japan_dt_filtered$date), ], x = ~date, y = ~visitors, type = 'scatter', mode = 'lines')


training <- window(japan_filtered, end=c(2018, 12))
test <- window(japan_filtered, start=c(2019, 1))

ndiffs(training) 
Acf(training)
Pacf(training)

auto_model <- auto.arima(training) 
summary(fit_arima)

model_arima <- arima(training, order = c(1,0,0))
summary(model_arima)


residuals <- residuals(model_arima)
plot(residuals)
Acf(residuals, main="ACF reszt") 



model_season <- tslm(training ~ trend + season)
summary(model_season)

plot(training, main = "Trend Liniowy z Sezonowością", ylab = "Liczba pasażerów", xlab = "Rok")
lines(fitted(model_season), col = "red", lwd = 2)
legend("topleft", legend = c("Dane rzeczywiste", "Trend + sezonowość"), col = c("black", "red"), lty = 1)

residuals <- residuals(model_season)
plot(residuals)
Acf(residuals, main="ACF reszt") 

# h = 1*12 because, forecast is for 1 year for 12 months
f<-forecast(model_season, level=c(95), h=12)
plot(f)
?forecast
f$mean-test
mse<-sum((f$mean-test)^2)


AIC(model_season,auto_model)
