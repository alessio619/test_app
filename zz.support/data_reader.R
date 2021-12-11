


# : ----------------------------



# A. PACKAGES =======================================================================

library(here)
library(data.table)
library(magrittr)
library(arrow)



# B. DATA UPLOAD ====================================================================

dts = readRDS(file = here('02.data', 'dts.rds')) %>% as.data.table()

dts[, datetime := lubridate::ymd_h(paste(date, hour, sep = ' '))]

dts[, peak := fcase(peak == '0', 'Off-Peak',
                    peak == '1', 'Peakload')]


# SYS ZONE 0
dts[, POD_ENC := fifelse(POD_ENC == 'c7s8EHUg48/1ykRbT3PaFw==', 'Federico_1', POD_ENC)]
dts[, POD_ENC := fifelse(POD_ENC == 'CX7/rutFF9RqqLm/7WOqhw==', 'Federico_2', POD_ENC)]
dts[, POD_ENC := fifelse(POD_ENC == 'g8inU57+ShL6hC9yO2b7lg==', 'Federico_3', POD_ENC)]

dts[, POD_ENC := fifelse(POD_ENC == '/+KcaOZXW70mwhYnHkEZxQ==', 'Valeria_1', POD_ENC)]
dts[, POD_ENC := fifelse(POD_ENC == 'ixKPGvVMABOt2anW+3cZqg==', 'Valeria_2', POD_ENC)]

dts[, POD_ENC := fifelse(POD_ENC == '+TcIDsBumDwIVMtMvaIyEg==', 'Valeria_3', POD_ENC)]



dts_a = dts[1:2919520] 
dts_b = dts[2919521:5839040] 
dts_c = dts[5839041:8758559] 

dts_a[, ownership := 'Company_A']
dts_b[, ownership := 'Company_B']
dts_c[, ownership := 'Company_C']

saveRDS(dts_a, file = here('02.data', 'regular_update', 'dts_a.rds'))
saveRDS(dts_b, file = here('02.data', 'regular_update',  'dts_b.rds'))
saveRDS(dts_c, file = here('02.data', 'regular_update',  'dts_c.rds'))



### Export  

dts = rbind(dts_a, dts_b, dts_c) ; rm(dts_a, dts_b, dts_c)
saveRDS(dts, file = here('02.data', 'regular_update', 'dts.rds'))





file_ru_1 <- readRDS(here('02.data', 'regular_update', 'dts_c.rds'))



dts_c = read_parquet(here('02.data', 'regular_update', 'dts_c.parquet'))
dts_b = read_parquet(here('02.data', 'regular_update', 'dts_b.parquet'))
dts_a = read_parquet(here('02.data', 'regular_update', 'dts_a.parquet'))


dts_c[, ownership := 'Company_C']
dts_a[, ownership := 'Company_A']
dts_b[, ownership := 'Company_B']


write_parquet(dts_c, sink = here('02.data', 'regular_update', 'dts_c.parquet'), version = '2.0')
write_parquet(dts_a, sink = here('02.data', 'regular_update', 'dts_a.parquet'), version = '2.0')
write_parquet(dts_b, sink = here('02.data', 'regular_update', 'dts_b.parquet'), version = '2.0')

