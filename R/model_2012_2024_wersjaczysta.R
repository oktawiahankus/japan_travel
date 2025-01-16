library(zoo)
library(forecast)
library(TSstudio)
library(plotly)
library(ggplot2)
library(latex2exp)
library(astsa) # sarima.for
library(tseries)
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
ts_decompose(train, type = "both") 
# on dla airpassengers wybral multiplikatywna, wiec tez tak zrobimy moze -- metoda analogicznego podejscia do wykladu
#1. widac ze moment pandemii zaburza nam szereg, trend bylby staly jak w latach 2012-2019, ale covid to psuje
# 2. widac sezonowosc, w ciagu roku mamy 3 maksima lokalne?
# spodziewamy sie wiec, ze szereg nie bedzie stacjonarny, przekonajmy sie
Acf(train, lag = 24) # duza czesc obserwacji lezy poza obszarem istotnosci, szereg nie  jest stacjo
nd <- ndiffs(train)
nsd <- nsdiffs(train)

cat("Liczba potrzebnych różnic (d):", nd, "\n")
cat("Liczba potrzebnych różnic sezonowych (D):", nsd, "\n")
diff_train = diff(train, differences = 1, lag = frequency(train))
par(mfrow = c(1, 2))
plot(train)
abline(h = mean(train), col = "red")
plot(diff_train) # no nie powiedzialabym, ze wariancja tutaj jest stala XD ale chyba nic nie uzyskamy lepszego
abline(h = mean(diff_train), col = "red")
# dodac przerywane szare linie co roku! bedzie widac sezonowosc lepiej


par(mfrow = c(1, 2)) # Dwa wykresy obok siebie
Acf(diff_train, main="ACF różnicowanych danych", lag = 24) # wg mnie MA(q) q = 4 albo 6 nwm 
Pacf(diff_train, main="PACF różnicowanych danych", lag = 24) 


auto.arima(train) #q = 1, p = 1 ok

# modele przerozne
model_auto = auto.arima(train)
model_arima616 = arima(train, order = c(6,1,6))
model_sarima616 = arima(train, order = c(6,1,6), seasonal = list(order = c(2,0,1), period = 12))
model_sarima114 = arima(train, order = c(1,1,4), seasonal = list(order = c(2,0,1), period = 12))
model_sarima116 = arima(train, order = c(1,1,6), seasonal = list(order = c(2,0,1), period = 12))

model_season = tslm(train ~ trend + season)
model_ets = ets(train)


summary(model_auto)
summary(model_arima616)
summary(model_season) 
summary(model_ets)

AIC(model_auto, model_arima616, model_sarima616, model_sarima114,model_sarima116) #auto model lepszy, ale nie duzo tbh
BIC(model_auto, model_arima616, model_sarima616, model_sarima114,model_sarima116) #znowu nieduzo lepszy
# wybieramy 3 najlepsze na podstawie AIC i BIC
# model_auto, model_arima616, model_sarima114

#imo nie ma co chyba z tym model season i ets bo slabe

# badanie reszt
residuals_auto = residuals(model_auto)
residuals_arima616 = residuals(model_arima616)
residuals_sarima114 = residuals(model_sarima114)

plot(residuals_auto)
plot(residuals_arima616)
plot(residuals_sarima114)

qqnorm(residuals_auto)
qqline(residuals_auto) 

qqnorm(residuals_arima)
qqline(residuals_arima)
par(mfrow = c(1,3))
Acf(residuals_auto)
Acf(residuals_arima616) # ups tu wystaje xd
Acf(residuals_sarima114)



# jak przewiduja???
forecast_auto = forecast(model_auto, level = c(95), h = length(test))
forecast_arima616 = forecast(model_arima616, level = c(95), h = length(test))
forecast_sarima114 = forecast(model_sarima114, level = c(95), h = length(test))
# mozna tez uzyc funkcji sarima.for, bo byla na jakiejs tam liscie, v to jest odrazu forecast, ale dodam go do ggplot
model_sarima = sarima.for(train, n.ahead = length(test), p = 1, d = 1, q = 1, P = 1, D = 0, Q = 2, S = 12) # wartosci wspolczynnikow wybralam z model_auto (auto.arima)

data = data.frame(
  czas=time(test),
  test,
  forecast_auto$mean,
  forecast_arima616$mean, 
  forecast_sarima114$mean)


ggplot(data, aes(x = czas)) +
  geom_line(aes(y = test, color = 'Dane testowe')) +
  geom_line(aes(y = forecast_auto.mean, color = 'SARIMA(1,1,1)(1,0,2)_{12}')) +
  geom_line(aes(y = forecast_arima616.mean, color = 'eARIMA(1,1,4)')) + # eArima zeby dane testowe byly wyzej w legendzie xD
  geom_line(aes(y = forecast_sarima114.mean, color = 'SSarima')) + #sarima z funkcji sarima.for
  theme_minimal() +
  labs(x = 'Czas', y = 'Wartość', color = 'Legenda') +
  scale_color_manual(values = c('Dane testowe' = 'black', 'SARIMA(1,1,1)(1,0,2)_{12}' = 'hotpink', 'eARIMA(1,1,4)' = 'blue', 'SSarima' = 'darkviolet'),
                     labels = c('Dane testowe' = 'Dane testowe', 
                                'SARIMA(1,1,1)(1,0,2)_{12}' = expression(SARIMA(1,1,1)(1,0,2)[12]), 
                                'eARIMA(1,1,4)' = expression(ARIMA(1,1,4)),
                                 'SSarima' = expression(SARIMA(1,1,4)(1,0,2)[12])))
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
