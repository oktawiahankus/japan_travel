library(zoo)
library(forecast)
library(TSstudio)
library(plotly)
library(ggplot2)
library(latex2exp)
library(astsa) # sarima.for
setwd("/Users/magdalenapotok/Desktop/japan_travel/data")
japan_dt <- readRDS("japan_dt.RDS")
japan_zoo <- zoo(japan_dt$visitors, japan_dt$date)

#dande koncza sie na 2024-08
japan_ts <- ts(japan_zoo, start = 1990, frequency = 12)
japan_filtered <- window(japan_ts, start=c(2012, 1), end=c(2024, 12))
plot(japan_filtered, main="Turystyka w Japonii (2012-2024)", xlab="Czas", ylab="Liczba turystów")

japan_dt_filtered <- japan_dt[japan_dt$date > as.Date("2011-12-31") & japan_dt$date < as.Date("2024-12-31") ]

ggplot(data=japan_dt_filtered,aes(x=date,y=visitors))+
  geom_line(color='darkblue')+
  theme_minimal()+
  labs(x='Rok',y='Liczba turystów')


train = window(japan_filtered, end=c(2023, 10))
test = window(japan_filtered, start=c(2023, 11))

ts_plot(test)
ts_plot(train)

# szukamy modelu dla train najpierw recznie ndiffs, Acf i Pacf
ndiffs(train) # trzeba raz zroznicowac
diff_train = diff(train, differences = 1, lag = frequency(train))
ndiffs(diff_train) # stacjonarny

par(mfrow = c(1, 2)) # Dwa wykresy obok siebie
Acf(train)
Acf(diff_train, main="ACF różnicowanych danych", lag = 24) # wg mnie MA(q) q = 4 albo 6 nwm 
Pacf(diff_train, main="PACF różnicowanych danych", lag = 24) 
nsdiffs(train)
auto.arima(train) #q = 1, p = 1 ok

# modele przerozne
model_auto = auto.arima(train)
model_arima = arima(train, order = c(1,1,4)) 
model_season = tslm(train ~ trend + season)
model_ets = ets(train)


summary(model_auto)
summary(model_arima)
summary(model_season) 
summary(model_ets)

AIC(model_auto, model_arima, model_season, model_ets) #auto model lepszy, ale nie duzo tbh
BIC(model_auto, model_arima, model_season, model_ets) #znowu nieduzo lepszy
# auto model on top

#imo nie ma co chyba z tym model season i ets bo slabe

# badanie reszt
residuals_auto = residuals(model_auto)
residuals_arima = residuals(model_arima)

plot(residuals_auto)
plot(residuals_arima)

qqnorm(residuals_auto)
qqline(residuals_auto) 

qqnorm(residuals_arima)
qqline(residuals_arima)

Acf(residuals_auto)
Acf(residuals_arima) # spoko oba

# ten arima i sarima podobne 


# jak przewiduja???
forecast_auto = forecast(model_auto, level = c(95), h = length(test))
forecast_arima = forecast(model_arima, level = c(95), h = length(test))

# mozna tez uzyc funkcji sarima.for, bo byla na jakiejs tam liscie, v to jest odrazu forecast, ale dodam go do ggplot
model_sarima = sarima.for(train, n.ahead = length(test), p = 1, d = 1, q = 1, P = 1, D = 0, Q = 2, S = 12) # wartosci wspolczynnikow wybralam z model_auto (auto.arima)

data = data.frame(czas=time(test),test,forecast_auto$mean,forecast_arima$mean, model_sarima$pred)


ggplot(data, aes(x = czas)) +
  geom_line(aes(y = test, color = 'Dane testowe')) +
  geom_line(aes(y = forecast_auto.mean, color = 'SARIMA(1,1,1)(1,0,2)_{12}')) +
  geom_line(aes(y = forecast_arima.mean, color = 'eARIMA(1,1,4)')) + # eArima zeby dane testowe byly wyzej w legendzie xD
  geom_line(aes(y = model_sarima.pred, color = 'SSarima')) + #sarima z funkcji sarima.for
  theme_minimal() +
  labs(x = 'Czas', y = 'Wartość', color = 'Legenda') +
  scale_color_manual(values = c('Dane testowe' = 'black', 'SARIMA(1,1,1)(1,0,2)_{12}' = 'hotpink', 'eARIMA(1,1,4)' = 'blue', 'SSarima' = 'darkviolet'),
                     labels = c('Dane testowe' = 'Dane testowe', 
                                'SARIMA(1,1,1)(1,0,2)_{12}' = expression(SARIMA(1,1,1)(1,0,2)[12]), 
                                'eARIMA(1,1,4)' = expression(ARIMA(1,1,4)),
                                 'SSarima' = expression(SARIMA.FOR(1,1,1)(1,0,2)[12]))) #nie jestem przekonana czy wgl umieszczac ten model
# SARIMA calkiem niezle!
# ARIMA ???slabo

# jakies statystyki 
sum((test - forecast_auto$mean)^2)
sum((test - forecast_arima$mean)^2) # okur ale duzoxd
sum((test - model_sarima$pred)^2) # ten ma najmniej ale nie wiem, to ten sam model tylko pewnie jakis inny sposob dobierania tych wspolczynnikow


# Analiza spektralna 
# sluzy do okreslenia s 
# robi sie dla szeregow stacjonarnych, wiec 

# 1. Analiza z logarytmowaniem widma dla lepszej widoczności:
spectrum(diff_train, spans = c(2, 2), log = "yes", 
         main = "Logarytmiczne spektrum mocy - Turystyka w Japonii (2012-2024)")

# 2. Odczytanie dominujących częstotliwości:
spec <- spectrum(diff_train, spans = c(2, 2), plot = FALSE)
dom_freq <- spec$freq[which.max(spec$spec)] # Znalezienie dominującej częstotliwości
dom_period <- 1 / dom_freq                 # Okres w jednostkach czasu
cat("Dominująca częstotliwość:", dom_freq, "\nOkres:", dom_period, "miesięcy\n")

# 3. Wykorzystanie pakietu forecast do analizy FFT (Fast Fourier Transform):
library(forecast)
fft_result <- fourier(ts(japan_filtered, frequency = 12), K = 2) # K: liczba harmonicznych
print(fft_result)
plot_ly(japan_)
