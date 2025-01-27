# japan_filtered <- window(japan_ts, start=c(2012, 1), end=c(2024, 12))
# plot(japan_filtered, main="Turystyka w Japonii (2012-2024)", xlab="Czas", ylab="Liczba turystów")
#  
# japan_dt_filtered <- japan_dt[date > as.Date("2011-12-31") & date < as.Date("2023-11-01")]
# train = window(japan_filtered, end=c(2023, 10))
# test = window(japan_filtered, start=c(2023, 11))
# model_auto = auto.arima(train)
# 
# saveRDS(model_auto, "data/with_covid_model.RDS")
# 
# training <- window(japan_filtered, end=c(2018, 12))
# auto_model <- auto.arima(training) 
# 
# saveRDS(auto_model, "data/before_2020_model.RDS")

kalman_model <- readRDS("data/kalman_model.RDS")
with_covid_model <- readRDS("data/with_covid_model.RDS")

# ten w sumie niepotrzebny, bo nic takiego nie robi
# before_2020_model <- readRDS("data/before_2020_model.RDS")

test_dt <- readRDS("data/test_dt.RDS")

future_dates <- c("2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01",
                  "2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01",
                  "2025-05-01", "2025-06-01", "2025-07-01", "2025-08-01")

test_len <- nrow(test_dt)
pred_len <- test_len + 12

kalman_fcast <- forecast(kalman_model, h = pred_len)
kalman_fcast <- window(kalman_fcast$mean, start = c(2024, 09))

kalman_fcast_dt <- data.table("ds" = as.Date(future_dates),
                              "y" = kalman_fcast)

with_covid_fcast <- forecast(with_covid_model, h = pred_len)
with_covid_fcast <- window(with_covid_fcast$mean, start = c(2024, 09))

with_covid_fcast_dt <- data.table("ds" = as.Date(future_dates),
                                  "y" = with_covid_fcast)

japan_covid_dt <- readRDS("data/japan_covid_dt.RDS")
colnames(japan_covid_dt) <- c("ds", "y")

future_plot <- ggplot(kalman_fcast_dt, aes(x = ds, y = as.numeric(y), color = "Model 3")) +
  geom_line() +
  geom_line(data = with_covid_fcast_dt, aes(y = y, color = "Model 2")) +
  geom_line(data = japan_covid_dt, aes(y = y, color = "Pełne dane")) +
  theme_minimal() +
  labs(x = "Rok",
       y = "Liczba turystów",
       color = "Model") +
  theme(plot.caption = element_text(hjust = .5, size = 14)) +
  scale_color_manual(values = c("Model 2" = "darkviolet", "Model 3" = "hotpink",
                                "Pełne dane" = "black"))

