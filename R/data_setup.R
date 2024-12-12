# data is a table imported from data.txt using 'Import Dataset'
# Japan National Tourism Organization (JNTO)
saveRDS(data, file = "data/raw_data.RDS")
raw_data <- readRDS("data/raw_data.RDS")

japan_dt <- as.data.table(raw_data)
japan_dt[, Growth.Rate... := NULL]
colnames(japan_dt) <- c("country", "month", "year", "visitors")


saveRDS(japan_dt, file = "data/japan_dt.RDS")
