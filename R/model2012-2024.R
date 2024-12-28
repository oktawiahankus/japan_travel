# modelowanie 2012 - 2024 

library(zoo)
library(forecast)
library(TSstudio)
library(plotly)
setwd("/Users/magdalenapotok/Desktop/japan_travel/data")
japan_dt <- readRDS("japan_dt.RDS")
japan_zoo <- zoo(japan_dt$visitors, japan_dt$date) #tworzy obiekt serii czasowej, visitors to dane, date indeks czasowy
japan_ts <- ts(japan_zoo, start = 1990, frequency = 12)
japan_filtered <- window(japan_ts, start=c(2012, 1), end=c(2024, 12))
plot(japan_filtered, main="Turystyka w Japonii (2012-2024)", xlab="Czas", ylab="Liczba turystów")
decompose(japan_filtered)
ts_decompose(japan_filtered) 
japan_dt_filtered = japan_dt[japan_dt$date > as.Date("2011-12-31") & japan_dt$date < as.Date("2024-12-31") ]
plot_ly(japan_dt_filtered[order(japan_dt_filtered$date), ], x = ~date, y = ~visitors, type = 'scatter', mode = 'lines')


train = window(japan_filtered, end=c(2022, 12))
test = window(japan_filtered, start=c(2023, 1))


# narazie sobie zrobie ARIMA model, ale pewnie SARIMA lepiej - bo sezonowosc?

plot(train)
ndiffs(train) # mówi ile trzeba razy różnicować, żeby był stacjonarny -> potrzebujemy stacjo żeby robić wykresy Acf i Pacf
diff_train = diff(train, differences = 1, lag = frequency(train))
ndiffs(diff_train) # essa
par(mfrow = c(1, 2)) # Dwa wykresy obok siebie
Acf(diff_train, main="ACF różnicowanych danych") # wg mnie MA(q), q = 4, bo to ostatni wystający (ale funkcja w R automatycznie wybiera q = 2 nie wiem :( )
Pacf(diff_train, main="PACF różnicowanych danych") # tutaj widać, że AR(p), p = 1 i to się zgadza z funkcją z R, więc git
# a więc p = 1, d = 1, q = 2/4 (?)
fit_arima = auto.arima(train) # tutaj q = 2 :/

model_arima_4 = arima(train, order = c(1, 1, 4))
summary(model_arima_4)

model_arima_2 = arima(train, order = c(1,1,2))
summary(model_arima_2)

AIC(model_arima_2,model_arima_4) # arima_2 lepszy

# Czy reszty modelu są białym szumem?
residuals = residuals(model_arima_2)
plot(residuals, main="Reszty modelu ARIMA(1,1,2)")
Acf(residuals, main="ACF reszt") # nie ma korelacji między błędami
Box.test(residuals, lag=12, type="Ljung-Box") # sprawdza autokorelacje dla pierwszych 12 opóźnień, reszty są losowe, bo p > 0.05


forecast_arima = forecast::forecast(model_arima_2, h = length(test)) # czy my tak robiliśmy prognozę na zajęciach?xd bo nie było mnie
plot(forecast_arima)

accuracy(forecast_arima,  test) # nwm tbh co to oznacza jeszcze

# Sarima chyba lepszy, no bo u nas widac sezonowosc 
#install.packages("astsa")
library(astsa)
fit_arima = auto.arima(train) # p = 0, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12
sarima.for(train, n.ahead = 24, p = 0, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12)
plot(japan_filtered, ylim= c(0,40000))


# w przyszlosci do zrb ses model, holt model, hw model - sprawdzenia 