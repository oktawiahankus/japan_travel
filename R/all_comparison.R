
wszystkie_plot <- test_plot +
  geom_line(data = kalman_fcast_dt, aes(y = y, color = "Kalman")) +
  geom_line(data = before_past_fcast_dt, aes(y = y, color = "przed i po")) +
  labs(color = "Model")

model_auto = auto.arima(train)
forecast_auto = forecast(model_auto, level = c(95), h = test_len)

forecast_auto_dt <- forecast_to_dt(forecast_auto)

wszystkie_plot +
  geom_line(data = forecast_auto_dt, aes(y = y, color = "madzia"))

training <- window(japan_filtered, end=c(2018, 12))
auto_model <- auto.arima(training) 

pred_len <- 4*12 + 11 + test_len - 1

forecast_auto_jula <- forecast(auto_model, h = pred_len)
forecast_window <- window(forecast_auto_jula$mean, start = c(2023, 11))
forecast_window_dt <- data.table("ds" = test_dates,
                                 "y" = forecast_window)

wszystkie_plot +
  geom_line(data = forecast_auto_dt, aes(y = y, color = "madzia")) +
  geom_line(data = forecast_window_dt, aes(y = y, color = "jula"))
