# jeżeli nie byłyby wczytane
# japan_dt <- readRDS("data/japan_dt.RDS")
# japan_ts <- readRDS("data/japan_ts.RDS")

japan_covid_dt <- japan_dt[date > as.Date("2011-12-31")]
japan_covid_ts <- window(japan_ts, start = c(2012, 1))

saveRDS(japan_covid_dt, "data/japan_covid_dt.RDS")

# kwiecień 2020 - zamknięcie granic
# październik 2022 - wznowienie turystyki 
empty_dt <- copy(japan_covid_dt)
empty_dt[date > as.Date("2020-03-31") & date < as.Date("2022-10-31"), visitors := NA]
empty_ts <- ts(empty_dt$visitors, start = 2012, frequency = 12)
# okazuje się, że wystarczy tak, nie trzeba zanmieniać na zoo

# Liczba turystów w latach 2012-2024 z wyłączeniem danych covidowych
empty_plot <- ggplot(empty_dt, aes(x = date, y = visitors))+
  geom_line(color = "darkblue") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-10-01"), 
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08) +
  theme_minimal() +
  labs(x = "Rok",
       y = "Liczba turystów") +
  theme(plot.caption = element_text(hjust = .5, size = 14))
