# data is a table imported from data.txt using 'Import Dataset'
# Japan National Tourism Organization (JNTO)
saveRDS(data, file = "data/raw_data.RDS")
raw_data <- readRDS("data/raw_data.RDS")


japan_dt <- as.data.table(raw_data)
japan_dt[, Growth.Rate... := NULL]
colnames(japan_dt) <- c("country", "month", "year", "visitors")
japan_dt[, `:=`(month = substr(month, 1, 3),
                visitors = as.numeric(visitors))]
# lubridate::my - converts character of format "Month Year" to date 
japan_dt[, date := my(paste(month, year, sep = " "))]
japan_dt <- japan_dt[, .(visitors = sum(visitors, na.rm = TRUE)), by = date]
# deleting lack of data, probably not gathered yet
japan_dt <- japan_dt[visitors > 0]
saveRDS(japan_dt, file = "data/japan_dt.RDS")
japan_dt <- readRDS("data/japan_dt.RDS")

japan_zoo <- zoo(japan_dt$visitors, japan_dt$date)
japan_ts <- ts(japan_zoo, start = 1990, frequency = 12)
plot(japan_ts)
# na razie wygląda to całkiem git, ale nie wiem, czy takie ręczne ustawianie 
# jest ok, czy nie, ale chyba inaczej się nie da, bo nie wczytywało automatcznie
# frequency - the number of observations per unit of time, 
# czyli by się zgadzało 


plot_ly(japan_dt[order(japan_dt$date), ], x = ~date, y = ~visitors, type = 'scatter', mode = 'lines')

# można to jeszcze rozbić na dane ze względu na kraje, skąd turysta
# predykcja brakujących wartości - co by było, gdyby nie było COVIDU?
# jakie starty przez COVID? (pieniądze)

# wybór lat 2010-2019, przed covidem 10 lat
# funkcja window zwraca obiek ts - to całkiem cool
japan_filtered <- window(japan_ts, start=c(2010, 1), end=c(2019, 12))
# on chciał, żeby koniecznie skrócić dane, bo nie widać sezonowości na wszystkich latach
plot(japan_filtered, main="Turystyka w Japonii (2010-2019)", xlab="Czas", ylab="Liczba turystów")
# w sumie miał racje, tu już ją widać ^
ts_decompose(japan_filtered) # trend wzrostowy, widać sezonowość, całkiem epickie

# tutaj ramka danych, bo lubię bardzo tę funkcję plot_ly xd
japan_dt_filtered = japan_dt[japan_dt$date > as.Date("2009-12-31") & japan_dt$date < as.Date("2020-01-01") ]
plot_ly(japan_dt_filtered[order(japan_dt_filtered$date), ], x = ~date, y = ~visitors, type = 'scatter', mode = 'lines')

# szereg z covidem 2010-2024, to będzie 15 lat, czyli skrócone dane
japan_covid <- window(japan_ts, start=c(2010, 1), end=c(2024, 12)) #japan_filtered_covid? dluga nazwa 
plot(japan_covid, main="Turystyka w Japonii (2010-2024)", xlab="Czas", ylab="Liczba turystów")
ts_decompose(japan_covid) #widać, że covid rozwala

# i co, chyba trzeba dobrać model do filtered i filtered z covid
# coś tam zaprognozować na podstawie bez covid i zobaczyć czy się pokrywa z danymi historycznymi

