library(imputeTS)
library(tseries)

# kwiecień 2020 - zamknięcie granic
# październik 2022 - wznowienie turystyki 

# lipiec/sierpień 2021 - Igrzyska Olimpijskie
japan_covid_dt <- japan_dt[date > as.Date("2011-12-31")]
plot_ly(japan_covid_dt, x = ~date, y = ~visitors, type = 'scatter', mode = 'lines')

japan_covid_ts <- window(japan_ts, start = c(2012, 1))
ts_plot(japan_covid_ts, slider = TRUE)
ts_decompose(japan_covid_ts)

# nie za bardzo wiem, co nam mówią te wykresy, mam wrażenie, że ciężko cokolwiek
# z nich odczytać 
Acf(japan_covid_ts, lag = 100)
Pacf(japan_covid_ts, lag = 100)

# różnicowanie krokiem 1 wydaje się nie mieć sensu, bo trend jest raczej stały
# (nie licząc covida)
# trochę jak odjęcie stałej


empty_dt <- copy(japan_covid_dt)
empty_dt[date > as.Date("2020-03-31") & date < as.Date("2022-10-31"), visitors := NA]
empty_zoo <- zoo(empty_dt$visitors, empty_dt$date)
empty_ts <- ts(empty_zoo, start = 2012, frequency = 12)

# mamy szereg z wyciętym covidem
ts_plot(empty_ts)

# to nie działa, kiedy mamy braki danych
ts_decompose(empty_ts)

# 1. na.approx - przybliżanie brakujących wartości za pomocą liniowej interpolacji
first_ts <- na.approx(empty_ts)
ts_plot(first_ts)
# możemy zobaczyć, że jest mega słabo, no ale nie możemy się dziwić, jak 
# jak przybliżama szereg czasowy funkcją liniową

# 2. na_kalman
second_ts <- na_kalman(empty_ts)
ts_plot(second_ts)
# wygląda całkiem nieźle !!!

# trzeba to ograniczyć do zbioru treningowego
# czyli chyba najlepiej do października 2023 - żeby mieć jeszcze rok po covidzie,
# a zbiór testowy to będzie od listopada 2023 do końca zbioru 

ts_decompose(second_ts)

# 3. zrobić przewidywanie na podstawie auto.arima - wcześniejsze dane 

# kiedy usunąć sezonowość? przed uzupełnianiem, czy po???
# po, bo nie działa na przed

training_ts <- window(empty_ts, start = c(2012, 1), end = c(2023, 10))
ts_plot(training_ts)

test_ts <-  window(empty_ts, start = c(2023, 11))
ts_plot(test_ts)

# wydaje mi się, że to trochę mało danych testowych, ale na razie niech tak 
# będzie potem można zrobić inczaej i sprawdzić, jak to działa

# nie no to może być ok, bo i tak najlepsze estymacje dostajemy zawsze dla 
# bliskich danych

approx_ts <- na_kalman(training_ts)
ts_plot(approx_ts)
# widzimy seconowość, trend tak niekoniecznie 
ts_decompose(approx_ts)
# czy szum jest stacjonarny??

# widzimy sezonowość
Acf(approx_ts, lag = 50)
# tutaj też to chyba widać, co w 12 mamy największą
Pacf(approx_ts, lag = 50)

# trend jest bardzo dziwny, sezonowość jakaś...
ts_decompose(approx_ts, type = "both")
# dekompozycja multiplikatywna raczej nic nie zmienia

# to usuwałoby tylko sezonowość, bo trend nie jest liniowy, 
# nie będzie działać tak jakbyśmy chcieli
ts_plot(diff(diff(approx_ts), lag = 12))

# wyestymować trend jakoś
decompose(approx_ts)
# używa filtra średniej ruchomej 

# różnicowanie krokiem 1
diff_trainig <- diff(training_ts)
ts_plot(diff_trainig)
# nie dostajemy wartości po covidzie

Acf(diff_trainig, lag = 50)
Pacf(diff_trainig, lag = 50)
# nie wydaje się pomagać

# ta funkcja mówi, że nie musimy wcale różnicować 
ndiffs(training_ts)
# tylko, że pewnie NA nam psują wynik

# różnicowanie krokiem 12
diff_trainig_12 <- diff(training_ts, lag = 12)
ts_plot(diff_trainig_12)
# nie dostajemy wartości po covidzie

Acf(diff_trainig_12, lag = 50)
Pacf(diff_trainig_12, lag = 50)

# nie będzie działać bo mamy braki
adf.test(training_ts)

# zająć się szeregiem z brakami estymowanymi za pomocą filtru kalmana
kalman_ts <- na_kalman(training_ts)
ts_plot(kalman_ts)

# widzimy sezonowość
Acf(kalman_ts, lag = 50)
# tutaj też to chyba widać, co w 12 mamy największą
Pacf(kalman_ts, lag = 50)

# nie rozumiem zbytnio tych testów
adf.test(kalman_ts)
# mała p-wartość - możemy odrzucić H_0 ---> szereg jest stacjonarny
kpss.test(kalman_ts)
# duża p-wartość - nie możemy odrzucić H_0 ---> szereg jest stacjonarny

# to też nam pokazuje, że nie trzeba nic robić ??
ndiffs(kalman_ts)

kalman_fit <- auto.arima(kalman_ts)

# to jest niepotrzebne, bo uwzględnia to kalman_fit
# kalman_12 <- diff(kalman_ts, lag = 12)
# ts_plot(kalman_12)
# ts_decompose(kalman_12)


kalman_fcast <- forecast(kalman_fit, h = length(test_ts))
kalman_dt <- as.data.table(kalman_fcast)
colnames(kalman_dt)[1] <- "y"
test_dt <- as.data.table(ts_to_prophet(test_ts))
kalman_dt[, ds := test_dt$ds]

kalman_plot <- plot_ly(test_dt, x = ~ds, y = ~y, name = 'test time series', type = 'scatter', mode = 'lines') %>%
  add_trace(data = kalman_dt, y = ~y, name = 'fitted', mode = 'lines')

# trochę słabo :((

# drugi sposób
# wartości tylko do covida
covid_cut <- window(training_ts, end = c(2020, 3))
ts_plot(covid_cut)

Acf(covid_cut)
Pacf(covid_cut)

before_covid <- auto.arima(covid_cut)
covid_len <- empty_dt[is.na(visitors), .N]

before_covid_fcast <- forecast(before_covid, h = covid_len)
before_covid_facst_dt <- as.data.table(before_covid_fcast)
new_dt <- copy(empty_dt)
new_dt[is.na(visitors), visitors := before_covid_facst_dt[[1]]]

plot_ly(japan_covid_dt, x = ~date, y = ~visitors, type = 'scatter', mode = 'lines', name = "original") %>%
  add_trace(data = new_dt, y = ~visitors, name = "predicted covid")
# te przewidywania wyglądają całkiem nieźle !!!

new_zoo <- zoo(new_dt$visitors, new_dt$date)
new_ts <- ts(new_zoo, start = 2012, frequency = 12)

ts_plot(new_ts)
ts_decompose(new_ts)
# wygląda mega sezonowo
Acf(new_ts, lag = 50)
Pacf(new_ts, lag = 50)
# pewnie różnicowanie krokiem 12 byłoby git, trend cały czas jest mega dziwny
# ale tutaj jakoś lepiej wyglądają reisdua po dekompzycji

new_fit <- auto.arima(new_ts)
# drift - trohcę jak stały trend - po prostu przesunięcie o stałą
new_fcast <- forecast(new_fit, h = length(test_ts))
new_fcast_dt <- as.data.table(new_fcast)
new_fcast_dt[, ds := test_dt$ds]
colnames(new_fcast_dt)[1] <- "y"

plot_ly(test_dt, x = ~ds, y = ~y, name = 'test time series', type = 'scatter', mode = 'lines') %>%
  add_trace(data = new_fcast_dt, y = ~y, name = 'fitted', mode = 'lines')
# też bardzo słabo 
