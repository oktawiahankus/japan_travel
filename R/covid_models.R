# funkcja tworząca wykresy ggplot dla obiektu ts
plot_ts <- function(ts, name){
  dt <- as.data.table(ts_to_prophet(ts))
  
  ggplot(dt, aes(x = ds, y = y)) +
    geom_line(color = "darkblue") +
    theme_minimal() +
    labs(x = "Rok",
         y = "Liczba turystów",
         caption = name) +
    theme(plot.caption = element_text(hjust = .5))
}

# podział danych na treningowe i testowe
training_ts <- window(empty_ts, start = c(2012, 1), end = c(2023, 10))
test_ts <-  window(empty_ts, start = c(2023, 11))

# 1. liniowa interpolacja
interpolation_ts <- na_interpolation(training_ts)
plot_ts(interpolation_ts, "Estymacja za pomocą interpolacji")

# 2. średnia ruchoma
ma_filter_ts <- na_ma(empty_ts, weighting = "simple")
plot_ts(ma_filter_ts, "Estymacja za pomocą średniej ruchomej")

# 3. filtr kalmana
kalman_ts <- na_kalman(training_ts)
plot_ts(kalman_ts, "Estymacja za pomocą filtru kalmana")
################################################################################
kalman_fit <- auto.arima(kalman_ts)

# to jest niepotrzebne, bo uwzględnia to kalman_fit
# kalman_12 <- diff(kalman_ts, lag = 12)
# ts_plot(kalman_12)
# ts_decompose(kalman_12)


kalman_fcast <- forecast(kalman_fit, h = length(test_ts))
kalman_dt <- as.data.table(kalman_fcast)
colnames(kalman_dt)[1] <- "y"
test_dt <- as.data.table(ts_to_prophet(test_ts))
kalman_dt[, ds := test_dt$ds]

kalman_plot <- plot_ly(test_dt, x = ~ds, y = ~y, name = 'test time series', type = 'scatter', mode = 'lines') %>%
  add_trace(data = kalman_dt, y = ~y, name = 'fitted', mode = 'lines')
# trochę słabo :((

plot_ly(training_dt, x = ~ds, y = ~y, name = 'test time series', type = 'scatter', mode = 'lines') %>%
  add_trace(y = kalman_fit$fitted, mode = 'lines', name = 'fitted')

# drugi sposób
# wartości tylko do covida
covid_cut <- window(training_ts, end = c(2020, 3))
ts_plot(covid_cut)

Acf(covid_cut)
Pacf(covid_cut)

before_covid <- auto.arima(covid_cut)
covid_len <- empty_dt[is.na(visitors), .N]

before_covid_fcast <- forecast(before_covid, h = covid_len)
before_covid_facst_dt <- as.data.table(before_covid_fcast)
new_dt <- copy(empty_dt)
new_dt[is.na(visitors), visitors := before_covid_facst_dt[[1]]]

plot_ly(japan_covid_dt, x = ~date, y = ~visitors, type = 'scatter', mode = 'lines', name = "original") %>%
  add_trace(data = new_dt, y = ~visitors, name = "predicted covid")
# te przewidywania wyglądają całkiem nieźle !!!

new_zoo <- zoo(new_dt$visitors, new_dt$date)
new_ts <- ts(new_zoo, start = 2012, frequency = 12)

ts_plot(new_ts)
ts_decompose(new_ts)
# wygląda mega sezonowo
Acf(new_ts, lag = 50)
Pacf(new_ts, lag = 50)
# pewnie różnicowanie krokiem 12 byłoby git, trend cały czas jest mega dziwny
# ale tutaj jakoś lepiej wyglądają reisdua po dekompzycji

# trzeba podzielić

new_train <- window(new_ts, start = c(2012, 1), end = c(2023, 10))
new_test <-  window(new_ts, start = c(2023, 11))

new_fit <- auto.arima(new_train)
# drift - trohcę jak stały trend - po prostu przesunięcie o stałą
new_fcast <- forecast(new_fit, h = length(test_ts))
new_fcast_dt <- as.data.table(new_fcast)
new_fcast_dt[, ds := test_dt$ds]
colnames(new_fcast_dt)[1] <- "y"

plot_ly(test_dt, x = ~ds, y = ~y, name = 'test time series', type = 'scatter', mode = 'lines') %>%
  add_trace(data = new_fcast_dt, y = ~y, name = 'fitted', mode = 'lines')
# lepiej 

# a jak to w ogóle wygląda z treningowymi danymi ???

training_dt <- as.data.table(ts_to_prophet(training_ts))

plot_ly(training_dt, x = ~ds, y = ~y, name = 'test time series', type = 'scatter', mode = 'lines') %>%
  add_trace(y = new_fit$fitted, mode = 'lines', name = 'fitted')
# dopasowanie jest całkiem niezłe

# żeby je porównać możemy policzyć MSE w sumie 
# i jeszcze policzyć AIC
AIC(new_fit)
AIC(kalman_fit)

# można też spróbować wszytsko za pomocą filtru Kalmana - ale ja nie za bardzo 
# wiem jak on działa, a wszystko w internecie jest słabo zrozumiałe
