library(zoo)
library(forecast)
library(TSstudio)
library(plotly)
library(ggplot2)
library(latex2exp)
library(astsa) # sarima.for
library(tseries)
# library(ggseas)
library(tidyr)
library(data.table)
library(gridExtra)
setwd("/Users/magdalenapotok/Desktop/japan_travel/data")
japan_dt <- readRDS("japan_dt.RDS")
japan_zoo <- zoo(japan_dt$visitors, japan_dt$date)

#dande koncza sie na 2024-08
japan_ts <- ts(japan_zoo, start = 1990, frequency = 12)
japan_filtered <- window(japan_ts, start=c(2012, 1), end=c(2024, 12))
plot(japan_filtered, main="Turystyka w Japonii (2012-2024)", xlab="Czas", ylab="Liczba turystów")

japan_dt_filtered <- japan_dt[japan_dt$date > as.Date("2011-12-31") & japan_dt$date < as.Date("2023-11-01") ]
 

library(scales)
ggplot(data=japan_dt_filtered,aes(x=date,y=visitors))+
  geom_line(color='darkblue')+
  theme_minimal()+
  scale_x_date(date_breaks = "1 year", 
               limits = as.Date(c('2012-01-01','2023-11-01')),
               date_labels = "%Y")+
  geom_vline(xintercept = as.Date('2012-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2013-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2014-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2015-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2016-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2017-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2018-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2019-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2020-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2021-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2022-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2023-01-01'), linetype="dashed", color = "grey")+
  geom_vline(xintercept = as.Date('2023-10-01'), linetype="dashed", color = "grey")+
  labs(x='Rok',y='Liczba turystów')

#geom_hline(yintercept=2.25, linetype="dashed", color = "red")+ 
train = window(japan_filtered, end=c(2023, 10))
test = window(japan_filtered, start=c(2023, 11))
?date_format
ts_plot(test)
ts_plot(train)

# decompose od julki
decomposed_multiplicative = decompose(train, type = "multiplicative") 
decomposed_data <- data.frame(
  czas = rep(time(train), 4),
  wartość = c(
    decomposed_multiplicative$x,
    decomposed_multiplicative$trend,
    decomposed_multiplicative$seasonal,
    decomposed_multiplicative$random
  ),
  komponent = factor(
    rep(c("Oryginalne dane", "Trend", "Sezonowość", "Reszty"), each = length(train)),
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

# on dla airpassengers wybral multiplikatywna, wiec tez tak zrobimy moze -- metoda analogicznego podejscia do wykladu
#1. widac ze moment pandemii zaburza nam szereg, trend bylby staly jak w latach 2012-2019, ale covid to psuje
# 2. widac sezonowosc, w ciagu roku mamy 3 maksima lokalne?
# spodziewamy sie wiec, ze szereg nie bedzie stacjonarny, przekonajmy sie
?Acf 
ACF_train = Acf(train, lag = 24) # duza czesc obserwacji lezy poza obszarem istotnosci, szereg nie  jest stacjo
plot(ACF_train, main = " ")
nd <- ndiffs(train)
nsd <- nsdiffs(train)

cat("Liczba potrzebnych różnic (d):", nd, "\n")
cat("Liczba potrzebnych różnic sezonowych (D):", nsd, "\n")
diff_train = diff(train, differences = 1, lag = frequency(train))
par(mfrow = c(1, 2))
plot(train)

#zmiana train -> data frame
df_train <- japan_dt[japan_dt$date > as.Date("2011-12-31") & japan_dt$date < as.Date("2023-11-01") ]

#zmiana diff_train -> data.frame :(
raw_diff_train <- "
        Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct    Nov    Dec
2013  -2082   -479  -1534   1296    222  -2638  -1564  -2160  -1726  -2473    562   1611
2014   -311   -142   1947   3503  -3364    -56   -404   2408  -1006   2584   -157  -1555
2015   -352  -2109   2731  -1105   2275  -1357   4732   -334   2247  -2609   -402  -1267
2016   -381  -1278  -1364   -947  -1458   1030    472   -883  -1606   2262    346     92
2017  -1438    504  -1270    716   -200   -765  -1708    986    875   -710    930    270
2018   -497  -1091   2641    922     14   1606   1637   -800  -1740  -1346    106     50
2019    212    442  -2369    277     13    675   -452   4545   2258   2498  -1741    625
2020   1544   1139  -1219 -16763 -15885 -15887 -15913 -17877 -14188  -9067  -6987  -8058
2021  -6542  -9297 -10327   2503   7666   3551  17641   9853   3299   -548  -1032  -3966
2022   -432   2779   7179   6730   3322   6483  -6161  -1734   9550   5885   8233  14777
2023   7026   6026   4619   8248   7471   6474   3584   6989   -530   4329      NA     NA
"

df_diff_train <- read.table(text = raw_diff_train, header = TRUE)
df_diff_train$Year <- as.numeric(rownames(df_diff_train))
rownames(df_diff_train) <- NULL
df_diff_train_long <- melt(setDT(df_diff_train), 
                           id.vars = "Year", 
                           variable.name = "Month", 
                           value.name = "visitors")
df_diff_train_long[, date := as.Date(paste(Year, Month, "1"), format = "%Y %b %d")]
df_diff_train_final <- df_diff_train_long[, .(date, visitors)]
df_diff_train_final = na.omit(df_diff_train_final)
df_diff_train_final = df_diff_train_final[order(df_diff_train_final$date)]
df_diff_train = df_diff_train_final 

# ggploty
gg_df_train = ggplot(data=df_train,aes(x=date,y=visitors))+
  geom_line(color='darkblue')+
  theme_minimal()+
  scale_x_date(date_breaks = "1 year", 
               limits = as.Date(c('2012-01-01','2023-11-01')),
               date_labels = "%Y")+
  geom_hline(yintercept=mean(train), linetype="dashed", color = "red")+
  labs(x='Rok',y='Liczba turystów')

gg_df_diff_train = ggplot(data=df_diff_train,aes(x=date,y=visitors))+
  geom_line(color='darkblue')+
  theme_minimal()+
  scale_x_date(date_breaks = "1 year", 
               limits = as.Date(c('2013-01-01','2023-11-01')),
               date_labels = "%Y")+
  geom_hline(yintercept=mean(diff_train), linetype="dashed", color = "red")+
  labs(x='Rok',y='Liczba turystów')
grid.arrange(gg_df_train,gg_df_diff_train, ncol = 2)

plot(diff_train) # no nie powiedzialabym, ze wariancja tutaj jest stala XD ale chyba nic nie uzyskamy lepszego
abline(h = mean(diff_train), col = "red")
diff_train
# dodac przerywane szare linie co roku! bedzie widac sezonowosc lepiej


par(mfrow = c(1, 2)) # Dwa wykresy obok siebie
Acf(diff_train, main="ACF zróżnicowanych danych", lag = 24) # wg mnie MA(q) q = 4 albo 6 nwm 
Pacf(diff_train, main="PACF zróżnicowanych danych", lag = 24) 


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

round(AIC(model_auto, model_arima616, model_sarima616, model_sarima114,model_sarima116),2) #auto model lepszy, ale nie duzo tbh
round(BIC(model_auto, model_arima616, model_sarima616, model_sarima114,model_sarima116),2) #znowu nieduzo lepszy
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
Acf(residuals_arima616, main = "ARIMA(6,1,6)") # ups tu wystaje xd
Acf(residuals_sarima114, main = "SARIMA(1,1,4)(2,0,1)[12]")
Acf(residuals_auto, main = "SARIMA(1,1,1)(2,0,1)[12]")



# jak przewiduja???
forecast_auto = forecast(model_auto, level = c(95), h = length(test))
forecast_arima616 = forecast(model_arima616, level = c(95), h = length(test))
forecast_sarima114 = forecast(model_sarima114, level = c(95), h = length(test))
# mozna tez uzyc funkcji sarima.for, bo byla na jakiejs tam liscie, v to jest odrazu forecast, ale dodam go do ggplot
model_sarima = sarima.for(train, n.ahead = length(test), p = 1, d = 1, q = 1, P = 1, D = 0, Q = 2, S = 12) # wartosci wspolczynnikow wybralam z model_auto (auto.arima)

data = data.frame(
  czas=as.Date(time(test)),
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
                                'SARIMA(1,1,1)(1,0,2)_{12}' = expression(SARIMA(1,1,1)(2,0,1)[12]), 
                                'eARIMA(1,1,4)' = expression(ARIMA(6,1,6)),
                                 'SSarima' = expression(SARIMA(1,1,4)(2,0,1)[12])))+
  scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2023-11-01','2024-08-01')),
               date_labels = "%Y %b")

# SARIMA calkiem niezle!
# ARIMA ???slabo

# jakies statystyki 

#MSE
MSE_auto = mean((test - forecast_auto$mean)^2)
MSE_arima616 = mean((test - forecast_arima616$mean)^2) # okur ale duzoxd
MSE_sarima114 = mean((test - forecast_sarima114$mean)^2) # ten ma najmniej ale nie wiem, to ten sam model tylko pewnie jakis inny sposob dobierania tych wspolczynnikow

#RMSD sqrt(mean((true - forecast)^2))
sqrt(MSE_auto)
sqrt(MSE_arima616)
sqrt(MSE_sarima114)

#MAE mean absolute error sum
mean(abs(test - forecast_auto$mean))
mean(abs(test - forecast_arima616$mean))
mean(abs(test - forecast_sarima114$mean))

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



# Analiza spektralna v2 - polecenia z list zadan + koentarz chatgpt do tego
# Obliczanie spektrum mocy na danych treningowych
AP = window(japan_filtered, end=c(2018, 12))
plot(AP)
boxplot(AP~cycle(AP))
x <- diff(log(AP))
plot(x)
spectrum(as.matrix(AP),log="no")
axis(1,at=c(/12))
spectrum(spec_data, log = "no")$freq

auto.arima(spec_data)
ARMAspec(auto.arima(spec_data))
?spectrum
raw.spec <- spec.pgram(spec_data, taper = 0)
plot(raw.spec, log = "no")
spectrum(spec_data)
spectrum(spec_data, span = 3, main = "Spektrum mocy z wygładzeniem (span=3)", log = "no")
# Eksperymentuj z różnymi wartościami span, np.:
spectrum(train, span = 5, main = "Spektrum mocy z wygładzeniem (span=5)")
#install.packages("TSA")
library(TSA)
ARMAspec(auto.arima(train))
ARMAspec(model = list(ar = 0.1), main = "Gęstość spektralna dla ARIMA(1,1,1)")
#Funkcja periodogram() w pakiecie TSA służy do rysowania niewygładzonego periodogramu, który pokazuje rozkład mocy dla różnych częstotliwości.
periodogram(train, main = "Periodogram dla danych treningowych")


model_sarima_madzia <- auto.arima(train)
# Parametry modelu (AR, MA, SAR, SMA)
ar_coeff <- coef(model_sarima)["ar1"]
ma_coeff <- coef(model_sarima)["ma1"]
sar_coeff <- coef(model_sarima)["sar1"]
sma_coeff <- coef(model_sarima)["sma1"]
sma_coeff2 <- coef(model_sarima)["sma2"]
par(mfrow = c(1,2))
# Gęstość spektralna teoretyczna modelu SARIMA
ARMAspec(model = list(
  ar = c(ar_coeff),
  ma = c(ma_coeff),
  sar = c(sar_coeff),
  sma = c(sma_coeff, sma_coeff2)
), main = "Gęstość spektralna teoretyczna dla SARIMA(1,1,1)(1,0,2)[12]")
# Spektrum mocy danych treningowych
spectrum(train, main = "Spektrum mocy danych empirycznych", span = 5)


