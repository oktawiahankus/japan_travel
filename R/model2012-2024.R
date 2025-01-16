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

# dekompozycja szeregu

ts_decompose(japan_filtered, type = "both") 
# ciezko cos z tego wywnioskowac, ogolnie komponenta sezonowa coprawda jest taka sama niezaleznie od trendu(?)
# ale jest dosc mala, wiec nie ma to az tak wplywu na dane?? nie kumam troche
# dobra ale z tych reszt mozna juz zobaczyc, ze multiplikatywny lepiej oddaje,
# bo widac, ze w momencie gdy trend spada, to  dzieja sie jakies losowe rzeczy -> reszty sa proporcjonalne
# do poziomu trendu, wieksze odchylenia sa w okresach niskiego trendu

dec_mult = decompose(japan_filtered, type = "multiplicative")
dec_add = decompose(japan_filtered, type = "additive")
res_mult = dec_mult$random
res_add = dec_add$random
res_mult = na.omit(res_mult)
res_add = na.omit(res_add)
sqrt(mean(res_mult^2)) # blad sredniokwadratowy jest jakos tak znaczaco mniejszy xd
sqrt(mean(res_add^2))

# usuwanie trendu

# metoda sredniej ruchomej
par(mfrow = c(1,1))
plot(dec_mult$trend)
plot(japan_filtered)
lines(ma(japan_filtered, order =2 ), col = "red")
lines(ma(japan_filtered, order =3 ), col = "blue")
lines(ma(japan_filtered, order =4 ), col = "green")
lines(ma(japan_filtered, order =5 ), col = "orange")


japan_dt_filtered = japan_dt[japan_dt$date > as.Date("2011-12-31") & japan_dt$date < as.Date("2024-12-31") ]
plot_ly(japan_dt_filtered[order(japan_dt_filtered$date), ], x = ~date, y = ~visitors, type = 'scatter', mode = 'lines')

plot_ly(japan_dt_filtered, x = ~date, y = ~visitors, type = 'scatter', mode = 'lines') %>%
  layout(shapes = list(
    list(type = "rect", x0 = "2020-04-01", x1 = "2022-10-01", y0 = 0, y1 = max(japan_dt_filtered$visitors, na.rm = TRUE), 
         fillcolor = "rgba(255, 0, 0, 0.2)", line = list(width = 0), layer = "below"),
    list(type = "line", x0 = "2021-07-01", x1 = "2021-07-01", y0 = 0, y1 = max(japan_dt_filtered$visitors, na.rm = TRUE), 
         line = list(dash = "dot", color = "blue"))
  ))

ggplot(japan_dt_filtered, aes(x = date, y = visitors)) +
  geom_line(color = "darkblue") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-10-01"), 
           ymin = 0, ymax = max(japan_dt_filtered$visitors, na.rm = TRUE), 
           fill = "red", alpha = 0.08) +
  geom_vline(xintercept = as.Date("2021-07-01"), linetype = "dotted", color = "blue") +
  labs(
    x = "Rok",
    y = "Liczba turystów"
  ) +
  theme_minimal()

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
par(mfrow = c(1,1))
plot(forecast_arima)

accuracy(forecast_arima,  test) # nwm tbh co to oznacza jeszcze

# Sarima chyba lepszy, no bo u nas widac sezonowosc 
#install.packages("astsa")
library(astsa)
fit_arima = auto.arima(train) # p = 0, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12
par(mfrow = c(1,2))
sarima.for(train, n.ahead = 24, p = 0, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12)
plot(japan_filtered, ylim= c(0,40000))


# w przyszlosci do zrb ses model, holt model, hw model - sprawdzenia 


# model ses - Simple Exponential smoothing - uzywany do szeregow czasowych bez sezonowosci i trendu (tutaj nie bardzo)
#install.packages("fpp2")
library(fpp2)
ses_model <- ses(train, h = length(test)) # h określa horyzont prognozy
summary(ses_model)

plot(ses_model) #xd no widac ze potrzebujemy bardziej zaawansowanego modelu

# holt - uwzglednia dane z trendem, ale bez sezonowosci

holt_model <- holt(train, h = length(test))
summary(holt_model)

plot(holt_model) #dalej slabo, to sa za proste modele

#ets() Model wygładzenia wykładniczego - uwzględnia trend i sezonowość

ets_model <- ets(train)
summary(ets_model)

plot(ets_model)
# Prognoza przy użyciu ETS
ets_forecast <- forecast(ets_model, h = length(test))
plot(ets_forecast)
AIC(ses_model$model, holt_model$model, ets_model, model_arima_2) #deklasacja ze strony arimy



# te rzeczy jakeis dziwne ale to pozniej ogarne
MSE_func = function(actual, predicted){
  return(mean((actual - predicted)^2))
}
arima_forecast = forecast(model_arima_2, h = length(test))
MSE_func(test, arima_forecast$mean)
MSE_func(test, ets_forecast$mean)

#test dieboldo-mariano
dm.test(residuals(model_arima_2), residuals(ets_model), alternative = "two.sided")
