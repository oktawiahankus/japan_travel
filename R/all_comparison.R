kalman_fcast_dt <- readRDS("data/kalman_fcast_dt.RDS")
with_covid_fcast <- readRDS("data/with_covid_fcast.RDS")
before_2020_fcast <- readRDS("data/before_2020_fcast.RDS")
test_dt <- readRDS("data/test_dt.RDS")

test_dates <- c("2023-11-01", "2023-12-01", "2024-01-01", "2024-02-01", 
                "2024-03-01", "2024-04-01", "2024-05-01", "2024-06-01", 
                "2024-07-01", "2024-08-01")

test_len <- length(test_dates)

with_covid_fcast_dt <- data.table("ds" = as.Date(test_dates),
                                  "y" = with_covid_fcast$mean)

before_2020_fcast_dt <- data.table("ds" = as.Date(test_dates),
                                   "y" = before_2020_fcast)

all_comparison_plot <- ggplot(test_dt, aes(x = ds, y = y, color = "Dane testowe")) +
  geom_line() +
  theme_minimal() +
  labs(x = "Rok",
       y = "Liczba turystów",
       color = "Model") +
  theme(plot.caption = element_text(hjust = .5, size = 14))

all_comparison_plot <- all_comparison_plot +
  geom_line(data = before_2020_fcast_dt, aes(y = y, color = "Model 1")) + # jula
  geom_line(data = with_covid_fcast_dt, aes(y = y, color = "Model 2")) + # magda
  geom_line(data = kalman_fcast_dt, aes(y = y, color = "Model 3")) + # oktawia (kalman)
  labs(color = "Model") +
  scale_color_manual(values = c("Dane testowe" = "darkblue", "Model 1" = "blue", 
                                "Model 2" = "darkviolet", "Model 3" = "hotpink"))


japan_covid_dt <- readRDS("data/japan_covid_dt.RDS")
colnames(japan_covid_dt) <- c("ds", "y")

# ja bym dała jeszcze taki wykres, bo tu trochę widać dlaczego modele są takie,
# a nie inne
all_comparison_plot +
  geom_line(data = japan_covid_dt, aes(y = y, color = "Pełne dane")) +
  scale_color_manual(values = c("Dane testowe" = "darkblue", "Model 1" = "blue", 
                                "Model 2" = "darkviolet", "Model 3" = "hotpink",
                                "Pełne dane" = "black"))


# madzia - ja tutaj cos probuje np zeby zrobic to jako jeden slajd na prezentacji?

full_comparison_plot <- all_comparison_plot +
  geom_line(data = japan_covid_dt, aes(y = y, color = "Pełne dane")) +
  scale_color_manual(values = c("Dane testowe" = "darkblue", "Model 1" = "blue", 
                                "Model 2" = "darkviolet", "Model 3" = "hotpink",
                                "Pełne dane" = "black"))

# Stworzenie zbliżenia (ten sam wykres, ale przycięty do końcowego okresu)
zoom_plot <- full_comparison_plot +
  coord_cartesian(xlim = c(as.Date("2023-11-01"), max(test_dt$ds)), 
                  ylim = c(min(kalman_fcast_dt$y, na.rm = TRUE), max(test_dt$y, na.rm = TRUE))) +
  theme(legend.position = "none", axis.title = element_blank())+ # Ukrywamy legendę w insetcie
  labs(x = NULL, y = NULL)

# Wklejenie zbliżenia jako inset w lewym dolnym rogu

full_comparison_plot +
  annotation_custom(ggplotGrob(zoom_plot), 
                    xmin = as.Date("2012-01-01"), xmax = as.Date("2017-01-01"),
                    ymin = 2000, ymax = 12000)  # Dostosowanie miejsca wklejenia



# to do przeklikania w pliku "model_2012_2024_wersjaczysta.R
# tak, żeby potem tylko wczytać RDS
# i nie robić wszystkiego od początku

# japan_filtered <- window(japan_ts, start=c(2012, 1), end=c(2024, 12))
# plot(japan_filtered, main="Turystyka w Japonii (2012-2024)", xlab="Czas", ylab="Liczba turystów")
# 
# japan_dt_filtered <- japan_dt[japan_dt$date > as.Date("2011-12-31") & japan_dt$date < as.Date("2023-11-01") ]
# train = window(japan_filtered, end=c(2023, 10))
# test = window(japan_filtered, start=c(2023, 11))
# model_auto = auto.arima(train)
# forecast_auto = forecast(model_auto, level = c(95), h = length(test))
# 
# saveRDS(forecast_auto, "data/with_covid_fcast.RDS")

# z tym tak samo, tylko w pliku "japan_model_2012-2019.R"
# training <- window(japan_filtered, end=c(2018, 12))
# auto_model <- auto.arima(training) 
# 
# pred_len <- 4 * 12 + 10 + test_len
# forecast_auto_jula <- forecast(auto_model, h = pred_len)
# forecast_window <- window(forecast_auto_jula$mean, start = c(2023, 11))
# MSE_julka = mean((test - forecast_window)^2)
# sqrt(MSE_julka)
# mean(abs(test - forecast_window))
# 
# saveRDS(forecast_window, "data/before_2020_fcast.RDS")
