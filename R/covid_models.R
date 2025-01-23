# funkcja tworząca wykresy ggplot dla obiektu ts
plot_ts <- function(ts){
  dt <- as.data.table(ts_to_prophet(ts))
  
  ggplot(dt, aes(x = ds, y = y)) +
    geom_line(color = "darkblue") +
    theme_minimal() +
    labs(x = "Rok",
         y = "Liczba turystów") +
    theme(plot.caption = element_text(hjust = .5, size = 14))
}

# podział danych na treningowe i testowe
training_ts <- window(empty_ts, start = c(2012, 1), end = c(2023, 10))
test_ts <-  window(empty_ts, start = c(2023, 11))

test_dt <- as.data.table(ts_to_prophet(test_ts))

saveRDS(test_dt, "data/test_dt.RDS")
test_dates <- test_dt$ds

test_len <- length(test_ts)

test_plot <- plot_ts(test_ts)

forecast_to_dt <- function(fcast){
  data.table("ds" = test_dates,
             "y" = fcast$mean)
}

covid_len <- sum(is.na(training_ts))
quartz(h = 6, w = 9)

# 1. predykcja na podstawie przeszłych wartości
before_covid_ts <- window(training_ts, end = c(2020, 3))

# Przed pandemią COVID-19
ggtsdisplay(before_covid_ts, 
            theme = theme_minimal())

nsdiffs(before_covid_ts)
ndiffs(before_covid_ts)

before_fit_1 <- arima(before_covid_ts, order = c(1, 0, 1),
                      seasonal = list(order = c(1, 0, 1), period = 12))

# residua - ARIMA(1,0,1)x(1,0,1)[12]
Acf(before_fit_1$residuals, main = "ACF reszt")
# ggtsdisplay(before_fit_1$residuals,
#             theme = list(theme_minimal(), 
#                          theme(plot.title = element_text(hjust = .5, size = 14))))

before_fit_2 <- arima(before_covid_ts, order = c(5, 0, 6),
                      seasonal = list(order = c(1, 0, 1), period = 12))

# residua - ARIMA(5,0,6)x(1,0,1)[12]
Acf(before_fit_2$residuals, main = "ACF reszt")
# ggtsdisplay(before_fit_2$residuals,
#             theme = list(theme_minimal(), 
#                          theme(plot.title = element_text(hjust = .5, size = 14))))

before_fit_3 <- arima(before_covid_ts, order = c(2, 0, 3),
                      seasonal = list(order = c(1, 0, 1), period = 12))

# residua - ARIMA(2,0,3)x(1,0,1)[12]
Acf(before_fit_3$residuals, main = "ACF reszt")
# ggtsdisplay(before_fit_3$residuals,
#             theme = list(theme_minimal(), 
#                          theme(plot.title = element_text(hjust = .5, size = 14))))

# coeftest(before_fit_6)

# model automatyczny
before_fit_auto <- auto.arima(before_covid_ts)

# residua - ARIMA(2,0,2)x(1,0,1)[12]
Acf(before_fit_auto$residuals, main = "ACF reszt")
# ggtsdisplay(before_fit_auto$residuals,
#             theme = list(theme_minimal(), 
#                          theme(plot.title = element_text(hjust = .5, size = 14)))) 

before_fcast <- forecast(before_fit_3, h = covid_len)
before_with_new <- c(before_covid_ts, before_fcast$mean)
before_with_new_ts <- ts(before_with_new, start = c(2012, 1), frequency = 12)

# Przed pandemią COVID-19 z predykcją
plot_ts(before_with_new_ts) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-10-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

# 2. przewidywanie na podstawie przeszłości
past_covid_ts <- window(training_ts, start = c(2022, 11))

# Po pandemii COVID-19
ggtsdisplay(past_covid_ts,
            theme = theme_minimal())

# żeby mieć predykcję w tył musielibyśmy odwrócić szereg
# ts(rev(past_covid_ts), start = c(2022, 11), frequency = 12)
# a potem dodać predykcję, znów odwrócić i ustawić start na 2020, 10
# ale tu nie dostajemy modelu, bo za mało danych

past_fit <- auto.arima(past_covid_ts)
# model to tak naprawdę daje tylko średnia z tego, co jest 

# jak spróbujemy coś przewidzieć, dostaniemy średnią covid_len razy
past_fcast <- forecast(past_fit, h = covid_len)
past_with_new <- c(rev(past_covid_ts), past_fcast$mean)

past_with_new_ts <- ts(rev(past_with_new), start = c(2020, 4), frequency = 12)

# Po pandemii COVID-19 z predykcją
plot_ts(past_with_new_ts) +
  annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2022-10-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

# teraz możemy to złączyć z predykcją dla przed
before_past <- data.table(before = as.vector(before_fcast$mean),
                          past = as.vector(past_fcast$mean))

before_past[, mean := (before + past)/2]

before_past_train_ts <- copy(training_ts)
before_past_train_ts[is.na(before_past_train_ts)] <- before_past$mean

# ggtsdisplay(before_past_train_ts,
#             theme = list(theme_minimal(),
#                          theme(plot.title = element_text(hjust = .5, size = 14))),
#             main = "Uzupełnienie pandemii COVID-19 z użyciem wartości przed i po")

# Uzupełnienie pandemii COVID-19 z użyciem wartości przed i po
plot_ts(before_past_train_ts) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-10-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

# Acf(before_past_train_ts, main = "ACF reszt")
# Pacf(before_past_train_ts, main = "PACF reszt")

before_past_fit <- auto.arima(before_past_train_ts)

# residua - ARIMA(2,0,2)(1,0,1)[12] - z predykcją przed i po
Acf(before_past_fit$residuals, main = "ACF reszt")
# ggtsdisplay(before_past_fit$residuals, lag = 100,
#             theme = list(theme_minimal(), 
#                          theme(plot.title = element_text(hjust = .5, size = 14))))

# 3. przewidywanie tylko z przeszłością
before_train_ts <- copy(training_ts)
before_train_ts[is.na(before_train_ts)] <- before_fcast$mean

plot_ts(before_train_ts) +
  annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2022-10-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)


before_fit <- auto.arima(before_train_ts)

# residua - ARIMA(2,0,2)(1,0,1)[12] - z predykcją przed
Acf(before_fit$residuals, main = "ACF reszt")
# ggtsdisplay(before_fit$residuals, lag = 100,
#             theme = list(theme_minimal(), 
#                          theme(plot.title = element_text(hjust = .5, size = 14))))

# 4. liniowa interpolacja
interpolation_ts <- na_interpolation(training_ts)

# Estymacja za pomocą interpolacji
plot_ts(interpolation_ts) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-11-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

# 5. średnia ruchoma
ma_filter_ts <- na_ma(empty_ts, weighting = "simple")

# Estymacja za pomocą średniej ruchomej
plot_ts(ma_filter_ts) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-11-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

# 6. filtr kalmana
kalman_ts <- na_kalman(training_ts)

# Estymacja za pomocą filtru kalmana
plot_ts(kalman_ts) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-11-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

kalman_fit <- auto.arima(kalman_ts)

# residua - ARIMA(2,0,2)(0,1,1)[12] - filtr kalmana
Acf(kalman_fit$residuals, main = "ACF reszt")
# ggtsdisplay(kalman_fit$residuals, lag = 100,
#             theme = list(theme_minimal(),
#                          theme(plot.title = element_text(hjust = .5, size = 14))))

# 7. filtr kalamana - metoda z auto.arima 
# sprawdzenie czegoś
aa_kalman_ts <- na_kalman(training_ts, model = "auto.arima")

# Estymacja za pomocą filtru kalmana - auto.arima
plot_ts(aa_kalman_ts) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-11-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08)

aa_kalman_fit <- auto.arima(aa_kalman_ts)

# residua - ARIMA(2,0,2)(0,1,1)[12] - filtr kalmana - auto.arima
Acf(aa_kalman_fit$residuals, main = "ACF reszt")

# predykcja zbioru testowego 

before_past_fcast <- forecast(before_past_fit, h = test_len)
before_past_fcast_dt <- forecast_to_dt(before_past_fcast)

# tu jest coś nie tak , bo to chyba nie ten model, co trzeba
before_fcast <- forecast(before_fit, h = test_len)
before_fcast_dt <- forecast_to_dt(before_fcast)

kalman_fcast <- forecast(kalman_fit, h = test_len)
kalman_fcast_dt <- forecast_to_dt(kalman_fcast)

# sprawdzenie
aa_kalman_fcast <- forecast(aa_kalman_fit, h = test_len)
aa_kalman_fcast_dt <- forecast_to_dt(aa_kalman_fcast)

all_plot <- ggplot(test_dt, aes(x = ds, y = y, color = "Dane testowe")) +
  geom_line() +
  theme_minimal() +
  labs(x = "Rok",
       y = "Liczba turystów",
       color = "Model") +
  theme(plot.caption = element_text(hjust = .5, size = 14))

all_plot <- all_plot +
  geom_line(data = kalman_fcast_dt, aes(y = y, color = "Model 1")) + # Kalman
  geom_line(data = before_past_fcast_dt, aes(y = y, color = "Model 2")) + # przed i po
  geom_line(data = before_fcast_dt, aes(y = y, color = "Model 3")) + # przed
  geom_line(data = aa_kalman_fcast_dt, aes(y = y, color = "aa_kalman")) +
  scale_color_manual(values = c("Dane testowe" = "darkblue", "Model 1" = "blue", 
                                "Model 2" = "darkviolet", "Model 3" = "hotpink",
                                "aa_kalman" = "red"))

# tutaj prorównanie mse i aic
mse_calc <- function(true, estimates){
  mean((estimates - true)^2)
}

mae_calc <- function(true, estimates){
  mean(abs(estimates - true))
}

all_dt <- copy(test_dt)
all_dt[, `:=`(kalman = kalman_fcast_dt$y,
              before = before_fcast_dt$y,
              before_past = before_past_fcast_dt$y)]

all_dt <- melt(all_dt, measure.vars = c("kalman", "before", "before_past"),
               id.vars = c("ds", "y"), variable.name = "model")

all_dt <- all_dt[, .(mse = mse_calc(y, value),
                     mae = mae_calc(y, value)), by = model]
all_dt[, `:=`(rmse = sqrt(mse),
              aic = c(kalman_fit$aic, before_fit$aic, before_past_fit$aic),
              bic = c(kalman_fit$bic, before_fit$bic, before_past_fit$bic))]

# to daje też liczbę stopni swobody
AIC(kalman_fit, before_fit, before_past_fit)
# Warning message:
#   In AIC.default(kalman_fit, before_fit, before_past_fit) :
#   models are not all fitted to the same number of observations
# pewnie przez to, że w kalmanie jest różnicowanie 

saveRDS(kalman_fcast_dt, file = "data/kalman_fcast_dt.RDS")

