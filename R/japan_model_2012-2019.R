
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
residuals <- residuals(model_arima)
Box.test(residuals, lag = 12, type = "Ljung-Box")

residuals <- residuals(auto_model)
Acf(residuals)
?Box.test
Box.test(residuals, lag = 20, type = "Ljung-Box")

plot(residuals)
Acf(residuals, main="ACF reszt") 

BIC(model_arima)
BIC(auto_model)
BIC(model_season)

model_season <- tslm(training ~ trend + season)
summary(model_season)

residuals <- residuals(model_season)
Acf(residuals, main="ACF reszt") 
AIC(model_season)

plot(training, main = "Trend Liniowy z Sezonowością", ylab = "Liczba pasażerów", xlab = "Rok")
lines(fitted(model_season), col = "red", lwd = 2)
legend("topleft", legend = c("Dane rzeczywiste", "Trend + sezonowość"), col = c("black", "red"), lty = 1)



# h = 1*12 because, forecast is for 1 year for 12 months
f1<-forecast(model_arima, level=c(95), h=12)
f2<-forecast(auto_model, level=c(95), h=12)
f3<-forecast(model_season, level=c(95), h=12)

data<-data.frame(czas=time(test),test,f1$mean,f2$mean,f3$mean)


ggplot(data, aes(x = czas)) +
  geom_line(aes(y = test, color = 'Szereg')) +
  geom_line(aes(y = f1.mean, color = 'Model 1')) +
  geom_line(aes(y = f2.mean, color = 'Model 2')) +
  geom_line(aes(y = f3.mean, color = 'Model 3')) +
  theme_minimal() +
  labs(x = 'Czas', y = 'Wartość', color = 'Legenda') +
  scale_color_manual(values = c('Szereg' = 'black', 'Model 1' = 'hotpink', 'Model 2' = 'blue', 'Model 3'= 'darkviolet'))

sum((test-f1$mean)^2)
sum((test-f2$mean)^2)
sum((test-f3$mean)^2)


sum((test-f1$mean))
sum((test-f2$mean))
sum((test-f3$mean))

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
