
par(mfrow = c(1,1))

japan_dt <- readRDS("data/japan_dt.RDS")

japan_zoo <- zoo(japan_dt$visitors, japan_dt$date)
japan_ts <- ts(japan_zoo, start = 1990, frequency = 12)
saveRDS(japan_ts, file = "data/japan_ts.RDS")

japan_filtered <- window(japan_ts, start=c(2012, 1), end=c(2019, 12))
plot(japan_filtered, main="Turystyka w Japonii (2012-2019)", xlab="Czas", ylab="Liczba turystów")

ggplot(data=japan_dt_filtered,aes(x=date,y=visitors))+
  geom_line(color='darkblue')+
  theme_minimal()+
  labs(x='Rok',y='Liczba turystów')


decompose(japan_filtered)
ts_decompose(japan_filtered) 
japan_dt_filtered <- japan_dt[japan_dt$date > as.Date("2011-12-31") & japan_dt$date < as.Date("2019-12-31") ]
plot_ly(japan_dt_filtered[order(japan_dt_filtered$date), ], x = ~date, y = ~visitors, type = 'scatter', mode = 'lines')


training <- window(japan_filtered, end=c(2018, 12))
test <- window(japan_filtered, start=c(2019, 1))
ts_plot(test)
ndiffs(training) 
Acf(training)
Pacf(training)

auto_model <- auto.arima(training) 
summary(auto_model)


model_arima <- arima(training, order = c(1,0,4))
summary(model_arima)


residuals <- residuals(auto_model)
plot(residuals)
Acf(residuals, main="ACF reszt") 



model_season <- tslm(training ~ trend + season)
summary(model_season)
AIC(model_season)

plot(training, main = "Trend Liniowy z Sezonowością", ylab = "Liczba pasażerów", xlab = "Rok")
lines(fitted(model_season), col = "red", lwd = 2)
legend("topleft", legend = c("Dane rzeczywiste", "Trend + sezonowość"), col = c("black", "red"), lty = 1)

residuals <- residuals(model_season)
plot(residuals)
Acf(residuals, main="ACF reszt") 

# h = 1*12 because, forecast is for 1 year for 12 months
f<-forecast(model_arima, level=c(95), h=12)
plot(test)
lines(f$mean)

f$mean-test


AIC(model_arima,auto_model)

#dekompozycja

decomposed_data <- decompose(training)


# b

plot(decomposed_data)

# c
acf(decomposed_data$random, na.action = na.pass)

# d
?decompose
decomposed_multiplicative <- decompose(training, type = "multiplicative")

plot(decomposed_multiplicative)

Acf(decomposed_multiplicative$random)
Acf(decomposed_add$random)


#dekompozycja w ggplocie 
decomposed_data <- data.frame(
  czas = rep(time(training), 4),
  wartość = c(
    decomposed_multiplicative$x,
    decomposed_multiplicative$trend,
    decomposed_multiplicative$seasonal,
    decomposed_multiplicative$random
  ),
  komponent = factor(
    rep(c("Oryginalne dane", "Trend", "Sezonowość", "Reszty"), each = length(training)),
    levels = c("Oryginalne dane", "Trend", "Sezonowość", "Reszty")
  )
)


ggplot(decomposed_data, aes(x = czas, y = wartość)) +
  geom_line() +
  facet_wrap(~ komponent, ncol = 1, scales = "free_y") +
  labs(
    title = "Dekompozycja szeregu (multiplikatywna)",
    x = "Czas",
    y = "Wartość"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


decomposed_add <- decompose(training, type = "additive")

plot(decomposed_add)

decomposed_data <- data.frame(
  czas = rep(time(training), 4),
  wartość = c(
    decomposed_add$x,
    decomposed_add$trend,
    decomposed_add$seasonal,
    decomposed_add$random
  ),
  komponent = factor(
    rep(c("Oryginalne dane", "Trend", "Sezonowość", "Reszty"), each = length(training)),
    levels = c("Oryginalne dane", "Trend", "Sezonowość", "Reszty")
  )
)


ggplot(decomposed_data, aes(x = czas, y = wartość)) +
  geom_line() +
  facet_wrap(~ komponent, ncol = 1, scales = "free_y") +
  labs(
    title = "Dekompozycja szeregu (addytywna)",
    x = "Czas",
    y = "Wartość"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )



# e
ts_decomposed <- ts_decompose(training, type = "both")


# f
trend_ma <- ts_ma(training, n = 4)

trend_ma2<- trend_ma$ma_4
df_ma<-data.frame(czas = rep(time(training)),training,trend_ma=c(NA,NA,NA,NA,trend_ma2,NA,NA,NA,NA),trend)

ggplot(df_ma, aes(x = czas)) +
  geom_line(aes(y = training, color = 'Szereg')) +
  geom_line(aes(y = trend, color = 'Trend (dekompozycja)')) +
  geom_line(aes(y = trend_ma, color = 'Trend MA(4)')) +
  theme_minimal() +
  labs(x = 'Czas', y = 'Wartość', color = 'Legenda') +
  scale_color_manual(values = c('Szereg' = 'black', 'Trend (dekompozycja)' = 'hotpink', 'Trend MA(4)' = 'blue'))

# g
trend<-decomposed_add$trend
train_df <- ts_to_prophet(training)
trend_df<-ts_to_prophet(trend_ma$ma_4)
trend_df<-ts_to_prophet(trend)

train_df <- train_df %>%
  select(ds, original = y) %>%
  left_join(trend_df, by = "ds") %>%
  mutate(detrended = original - y)


View(train_df)
plot.ts(train_df$detrended)

# h
train_df <- train_df %>%
  mutate(year = year(ds), month = month(ds))

plot_ly(data = train_df, x = ~year, y = ~detrended, type = "scatter", mode = "lines")

monthly_avg <- train_df %>%
  group_by(month) %>%
  summarize(month_avg = mean(detrended, na.rm = TRUE))


train_df <- train_df %>%
  left_join(monthly_avg, by = "month")


# i

train_df <- train_df %>%
  mutate(noise = detrended - month_avg)


ts.plot(train_df$noise)
acf_ma<-Acf(train_df$noise)
acf_dec<-Acf(train_df$noise)
Acf(train_df$noise)
