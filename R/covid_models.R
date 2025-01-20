# funkcja tworząca wykresy ggplot dla obiektu ts
plot_ts <- function(ts, name){
  dt <- as.data.table(ts_to_prophet(ts))
  
  ggplot(dt, aes(x = ds, y = y)) +
    geom_line(color = "darkblue") +
    theme_minimal() +
    labs(x = "Rok",
         y = "Liczba turystów",
         caption = name) +
    theme(plot.caption = element_text(hjust = .5, size = 14))
}

mse_calc <- function(true, estimates){
  mean((estimates - true)^2)
}

# podział danych na treningowe i testowe
training_ts <- window(empty_ts, start = c(2012, 1), end = c(2023, 10))
test_ts <-  window(empty_ts, start = c(2023, 11))
test_dates <- test_dt$ds
test_len <- length(test_ts)

test_plot <- plot_ts(test_ts, "Predykcja wartości")

forecast_to_dt <- function(fcast){
  data.table("ds" = test_dates,
             "y" = fcast$mean)
}

covid_len <- sum(is.na(training_ts))

# 1. predykcja na podstawie przeszłych wartości

before_covid_ts <- window(training_ts, end = c(2020, 3))
ggtsdisplay(before_covid_ts, 
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "Przed pandemią COVID-19")

nsdiffs(before_covid_ts)
ndiffs(before_covid_ts)

before_fit_1 <- arima(before_covid_ts, order = c(1, 0, 1),
                      seasonal = list(order = c(1, 0, 1), period = 12))

ggtsdisplay(before_fit_1$residuals,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "residua - ARIMA(1,0,1)x(1,0,1)[12]")

before_fit_2 <- arima(before_covid_ts, order = c(5, 0, 6),
                      seasonal = list(order = c(1, 0, 1), period = 12))

ggtsdisplay(before_fit_2$residuals,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "residua - ARIMA(5,0,6)x(1,0,1)[12]")

before_fit_3 <- arima(before_covid_ts, order = c(2, 0, 3),
                      seasonal = list(order = c(1, 0, 1), period = 12))

ggtsdisplay(before_fit_3$residuals,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "residua - ARIMA(2,0,3)x(1,0,1)[12]")

# coeftest(before_fit_6)

# model automatyczny
before_fit_auto <- auto.arima(before_covid_ts)

ggtsdisplay(before_fit_auto$residuals,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "residua - ARIMA(2,0,2)x(1,0,1)[12]") 

before_fcast <- forecast(before_fit_3, h = covid_len)
before_with_new <- c(before_covid_ts, before_fcast$mean)
before_with_new_ts <- ts(before_with_new, start = c(2012, 1), frequency = 12)

plot_ts(before_with_new_ts, "Przed pandemią COVID-19 z predykcją") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-10-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

# 2. przewidywanie na podstawie przeszłości
past_covid_ts <- window(training_ts, start = c(2022, 11))

ggtsdisplay(past_covid_ts,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "Po pandemii COVID-19")

# żeby mieć predykcję w tył musielibyśmy odwrócić szereg
# ts(rev(past_covid_ts), start = c(2022, 11), frequency = 12)
# a potem dodać predykcję, znów odwrócić i ustawić start na 2020, 10
# ale tu nie dostajemy modelu, bo za mało danych

past_fit <- auto.arima(past_ts)
# model to tak naprawdę daje tylko średnia z tego, co jest 

# jak spróbujemy coś przewidzieć, dostaniemy średnią covid_len razy
past_fcast <- forecast(past_fit, h = covid_len)
past_with_new <- c(rev(past_ts), past_fcast$mean)
past_with_new_ts <- ts(rev(rev_with_new), start = c(2020, 4), frequency = 12)

plot_ts(past_with_new_ts, "Po pandemii COVID-19 z predykcją") +
  annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2022-10-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

# teraz możemy to złączyć z predykcją dla przed
before_past <- data.table(before = as.vector(before_fcast$mean),
                          past = as.vector(rev_fcast$mean))

before_past[, mean := (before + past)/2]

before_past_train_ts <- copy(training_ts)
before_past_train_ts[is.na(before_past_train_ts)] <- before_past$mean

ggtsdisplay(before_past_train_ts,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "Uzupełnienie pandemii COVID-19 z użyciem wartości przed i po")

plot_ts(before_past_train_ts, "Uzupełnienie pandemii COVID-19 z użyciem wartości przed i po") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-10-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

before_past_fit <- auto.arima(before_past_train_ts)

ggtsdisplay(before_past_fit$residuals, lag = 100,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "residua - ARIMA(2,0,2)(1,0,1)[12] - z predykcją przed i po")

# 3. przewidywanie tylko z przeszłością
before_fit <- auto.arima(before_with_new_ts)

ggtsdisplay(before_fit$residuals, lag = 100,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "residua - ARIMA(2,0,2)(1,0,1)[12] - z predykcją przed")

# 4. liniowa interpolacja
interpolation_ts <- na_interpolation(training_ts)

plot_ts(interpolation_ts, "Estymacja za pomocą interpolacji") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-11-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

quartz(h = 6, w = 9)
# 5. średnia ruchoma
ma_filter_ts <- na_ma(empty_ts, weighting = "simple")

plot_ts(ma_filter_ts, "Estymacja za pomocą średniej ruchomej") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-11-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

# 6. filtr kalmana
kalman_ts <- na_kalman(training_ts)

plot_ts(kalman_ts, "Estymacja za pomocą filtru kalmana") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-11-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

kalman_fit <- auto.arima(kalman_ts)

ggtsdisplay(kalman_fit$residuals, lag = 100,
            theme = list(theme_minimal(), 
                         theme(plot.title = element_text(hjust = .5, size = 14))), 
            main = "residua - ARIMA(2,0,2)(0,1,1)[12] - filtr kalmana")

# predykcja zbioru testowego 

before_past_fcast <- forecast(before_past_fit, h = test_len)
before_past_fcast_dt <- forecast_to_dt(before_past_fcast)

before_fcast <- forecast(before_fit, h = test_len)
before_fcast_dt <- forecast_to_dt(before_fcast)

kalman_fcast <- forecast(kalman_fit, h = test_len)
kalman_fcast_dt <- forecast_to_dt(kalman_fcast)


all_plot <- test_plot +
  geom_line(data = kalman_fcast_dt, aes(y = y, color = "Kalman")) +
  geom_line(data = before_fcast_dt, aes(y = y, color = "przed")) +
  geom_line(data = before_past_fcast_dt, aes(y = y, color = "przed i po")) +
  labs(color = "Model") +
  scale_color_manual(values = c("przed i po" = "darkviolet", "przed" = "hotpink", "Kalman" = "blue"))

# tutaj prorównanie mse i aic

all_dt <- copy(test_dt)
all_dt[, `:=`(kalman = kalman_fcast_dt$y,
              before = before_fcast_dt$y,
              before_past = before_past_fcast_dt$y)]

all_dt <- melt(all_dt, measure.vars = c("kalman", "before", "before_past"),
               id.vars = c("ds", "y"), variable.name = "model")

all_dt <- all_dt[, .(mse = mse_calc(y, value)), by = model]
all_dt[, aic := c(kalman_fit$aic, before_fit$aic, before_past_fit$aic)]

gt(all_dt)
