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

mse_calc <- function(true, estimates){
  mean((estimates - true)^2)
}

# podział danych na treningowe i testowe
training_ts <- window(empty_ts, start = c(2012, 1), end = c(2023, 10))
test_ts <-  window(empty_ts, start = c(2023, 11))
test_dates <- test_dt$ds
test_len <- length(test_ts)

test_plot <- plot_ts(test_ts, "Zbiór treningowy")

forecast_to_dt <- function(fcast){
  data.table("ds" = test_dates,
             "y" = fcast$mean)
}

# 1. liniowa interpolacja
interpolation_ts <- na_interpolation(training_ts)
plot_ts(interpolation_ts, "Estymacja za pomocą interpolacji")

# 2. średnia ruchoma
ma_filter_ts <- na_ma(empty_ts, weighting = "simple")
plot_ts(ma_filter_ts, "Estymacja za pomocą średniej ruchomej")

# 3. filtr kalmana
kalman_ts <- na_kalman(training_ts)
plot_ts(kalman_ts, "Estymacja za pomocą filtru kalmana")

kalman_fit <- auto.arima(kalman_ts)
kalman_fcast <- forecast(kalman_fit, h = test_len)

kalman_fcast_dt <- forecast_to_dt(kalman_fcast)

kalman_plot <- test_plot +
  geom_line(data = kalman_fcast_dt, aes(y = y, color = "Kalman")) +
  labs(color = "Model") +
  scale_color_manual(values = c("Kalman" = "hotpink"))

# potem zrobić tabelkę z podsumowaniem mse i AIC
# czy powinniśmy zrobić jeszcze jakieś badanie residuów?

# 4. predykcja na podstawie przeszłych wartości
before_covid_ts <- window(training_ts, end = c(2020, 3))
plot_ts(before_covid_ts, "Przed padndemią COVID-19")

before_covid_fit <- auto.arima(before_covid_ts)
covid_len <- sum(is.na(training_ts))

before_covid_fcast <- forecast(before_covid, h = covid_len)

full_training_ts <- copy(training_ts)
full_training_ts[is.na(full_training_ts)] <- before_covid_fcast$mean

plot_ts(full_training_ts, "Predykcja na podstawie przeszłych wartości")

past_fit <- auto.arima(full_training_ts)
past_fcast <- forecast(past_fit, h = test_len)

past_fcast_dt <- forecast_to_dt(past_fcast)

past_plot <- test_plot +
  geom_line(data = past_fcast_dt, aes(y = y, color = "past")) +
  labs(color = "Model") +
  scale_color_manual(values = c("past" = "hotpink"))


all_plot <- test_plot +
  geom_line(data = kalman_fcast_dt, aes(y = y, color = "Kalman")) +
  geom_line(data = past_fcast_dt, aes(y = y, color = "past")) +
  labs(color = "Model") +
  scale_color_manual(values = c("past" = "hotpink", "Kalman" = "blue"))

# tutaj prorównanie mse i aic

all_dt <- copy(test_dt)
all_dt[, `:=`(kalman = kalman_fcast_dt$y,
              past = past_fcast_dt$y)]

all_dt <- melt(all_dt, measure.vars = c("kalman", "past"),
               id.vars = c("ds", "y"), variable.name = "model")

all_dt <- all_dt[, .(mse = mse_calc(y, value)), by = model]
all_dt[, aic := c(kalman_fit$aic, past_fit$aic)]

# 5. auto.arima in kalman
arima_kalman_ts <- na_kalman(training_ts, model = "auto.arima")
plot_ts(arima_kalman_ts, "Estymacja za pomocą filtru kalmana z użyciem auto.arima")
# już tu widać, że będzie źle
# w sumie to nawet nie wiem jak to działa
arima_kalman_fit <- auto.arima(arima_kalman_ts)
arima_kalman_fcast <- forecast(arima_kalman_fit, h = test_len)

arima_kalman_fcast_dt <- forecast_to_dt(arima_kalman_fcast)

arima_kalman_plot <- test_plot +
  geom_line(data = arima_kalman_fcast_dt, aes(y = y, color = "arima-Kalman")) +
  labs(color = "Model") +
  scale_color_manual(values = c("arima-Kalman" = "hotpink"))
