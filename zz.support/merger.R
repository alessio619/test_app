       
library(openxlsx)
library(data.table)
library(here)
library(skimr)

### DEBUG -------------------------------------------------------------------------------------------
        
        setwd('..')
        

# LV CLUSTERS

        name_ru_0 <- 'LV_SM_volumes_cluster.rds'
        
        path_0 = file.path(getwd())
        path_0b = file.path(path_0, 'regular_run', '6.Sales.Matrix', 'LV', 'Current.run', 'LV.feed.SM', name_ru_0)
        
        data_ru_0 = readRDS(file = path_0b) 
        
        lv_volumes_clusters = data_ru_0 %>% janitor::clean_names() %>% as.data.table()
        
      skim(lv_volumes_clusters)
          
# LV 1 
                
        name_ru_1 <- 'LV_tariffs_20211103.xlsx'
        
        path_1b = file.path(path_0, 'regular_run', '1.Current.Input', 'Input.LV', 'LV.SM.input', name_ru_1)
        
        lv_tariff_avgPrices = read.xlsx(path_1b,
                                sheet = 'total_avg_price',
                                detectDates = TRUE) %>% as.data.table()
        
        lv_tariff_avgPrices = melt(lv_tariff_avgPrices, id.vars = c('tariff', 'SECTOR', 'year_month_MWh'), variable.name = 'date', value.name = 'tariff_value')
        
        lv_tariff_avgPrices[, year := year(date)] %>% 
                          .[, month := month(date)]
        

      skim(lv_tariff_avgPrices)

# LV 2 
        
        name_ru_2 <- 'LV_loss_factor.xlsx'
        
        path_2b = file.path(path_0, 'regular_run', '1.Current.Input', 'Input.LV', 'LV.SM.input', name_ru_2)
        
        lv_lossFactors = read.xlsx(path_2b,
                                sheet = 'LV_loss_factor',
                                detectDates = TRUE) %>% as.data.table()        
        
     skim(lv_tariff_avgPrices)
        
           
# LV 3
        
        name_ru_3 <- 'LV_hedged_costs_vol_20211103.xlsx'
        
        path_3b = file.path(path_0, 'regular_run', '1.Current.Input', 'Input.LV', 'LV.SM.input', name_ru_3)
        
        lv_volCosts_hedged_vol = read.xlsx(path_3b,
                                   sheet = 'hedged_vol',
                                   detectDates = TRUE) %>% as.data.table()

        lv_volCosts_hedged_cost = read.xlsx(path_3b,
                                   sheet = 'hedged_cost',
                                   detectDates = TRUE) %>% as.data.table()

        lv_volCosts_generation_vol = read.xlsx(path_3b,
                                   sheet = 'generation_vol',
                                   detectDates = TRUE) %>% as.data.table()

        lv_volCosts_generation_cost = read.xlsx(path_3b,
                                   sheet = 'generation_cost',
                                   detectDates = TRUE) %>% as.data.table()    
        
    skim(lv_volCosts_hedged_vol)
    skim(lv_volCosts_hedged_cost)
    skim(lv_volCosts_generation_vol)
    skim(lv_volCosts_generation_cost)
        
# LV 4
        
        name_ru_4 <- 'HPFC_costs_20211103.xlsx'
        
        path_4b = file.path(path_0, 'regular_run','1.Current.Input', 'Input.LV', 'LV.SM.input', name_ru_4)
        
        lv_HPFC_costs = read.xlsx(path_4b,
                                   detectDates = TRUE) %>% as.data.table()
        
        
        
        
        
        
## 2. Compute hourly revenues by cluster ======

## 2.1 merge LV_SM_volumes_cluster and 2.LV_tariffs.xlsx by year month tariff ----

lv_tariff_avgPrices[, date := NULL]

lv_volumes_clusters[, tariff := as.character(tariff)]
lv_volumes_clusters_long = merge(lv_volumes_clusters, lv_tariff_avgPrices, by = c('year', 'month', 'tariff'), all.x = TRUE) 

rm(lv_volumes_clusters, lv_tariff_avgPrices)

## 2.2 compute revenues -----
## multiply delivery_volume by the tariff_value

lv_volumes_clusters_long[, revenues := delivery_volume * tariff_value]


## 3. Compute procured volumes =========

## 3.1 merge  LV_SM_volumes_cluster and 3.LV_loss_factor.xlsx by year month -----

lv_lossFactors[, DATE := NULL]

lv_volumes_clusters_long = merge(lv_volumes_clusters_long, lv_lossFactors, by = c('year', 'month'), all.x = TRUE)

rm(lv_lossFactors)


## 3.2 compute procured_volume by multiplying delivery_volume by the LV_loss_factor

lv_volumes_clusters_long[, procured_volume := delivery_volume * LV_loss_factor]


## 4. Generate a DB with total hourly procured volumes by segment =====


lv_volumes_clusters_long[, procured_volume_segm := sum(procured_volume), by = c('date', 'hour', 'segment')]




## 5. define sources of procured volumes by segment

lv_volCosts_generation_vol = melt(lv_volCosts_generation_vol, id.vars = c('date', 'hour'), variable.name = 'segment', value.name = 'generation_volume_segm')  
lv_volCosts_hedged_vol = melt(lv_volCosts_hedged_vol, id.vars = c('date', 'hour'), variable.name = 'segment', value.name = 'hedged_volume_segm')  

lv_volumes_clusters_long = merge(lv_volumes_clusters_long, lv_volCosts_generation_vol, by = c('date', 'hour', 'segment'), all.x = TRUE)
lv_volumes_clusters_long = merge(lv_volumes_clusters_long, lv_volCosts_hedged_vol, by = c('date', 'hour', 'segment'), all.x = TRUE)

rm(lv_volCosts_hedged_vol, lv_volCosts_generation_vol)

lv_volumes_clusters_long[, procured_prop := procured_volume/procured_volume_segm]


lv_volumes_clusters_long[, hedged_volume := hedged_volume_segm * procured_prop]
lv_volumes_clusters_long[, generation_volume := generation_volume_segm * procured_prop]

lv_volumes_clusters_long[, procured_volume_segm := NULL]

lv_volumes_clusters_long[, hedged_volume_segm := NULL]

lv_volumes_clusters_long[, generation_volume_segm := NULL]

lv_volumes_clusters_long[, open_volume := procured_volume - hedged_volume - generation_volume]


## 7. Calculate costs ==========

lv_volCosts_generation_cost = melt(lv_volCosts_generation_cost, id.vars = c('date', 'hour'), variable.name = 'segment', value.name = 'generation_unit_cost')  
lv_volCosts_hedged_cost = melt(lv_volCosts_hedged_cost, id.vars = c('date', 'hour'), variable.name = 'segment', value.name = 'hedged_unit_cost')  

lv_volumes_clusters_long = merge(lv_volumes_clusters_long, lv_volCosts_generation_cost, by = c('date', 'hour', 'segment'), all.x = TRUE)
lv_volumes_clusters_long = merge(lv_volumes_clusters_long, lv_volCosts_hedged_cost, by = c('date', 'hour', 'segment'), all.x = TRUE)

rm(lv_volCosts_hedged_cost, lv_volCosts_generation_cost)



## 7.2 merge the previous point DB with the excel file 5.HPFC&costs.xlsx 

lv_volumes_clusters_long = merge(lv_volumes_clusters_long, lv_HPFC_costs, by = c('date', 'hour'), all.x = TRUE)

rm(lv_HPFC_costs)


gc()


lv_volumes_clusters_long[, hedged_cost := hedged_unit_cost *  hedged_volume]
lv_volumes_clusters_long[, generation_cost := generation_unit_cost * generation_volume]
lv_volumes_clusters_long[, open_cost := HPFC * open_volume]
lv_volumes_clusters_long[, balancing_cost := procured_volume * balancing_cost]
lv_volumes_clusters_long[, uplift_cost := procured_volume * uplift_cost]
lv_volumes_clusters_long[, OPEX_cost := procured_volume * OPEX_cost]
lv_volumes_clusters_long[, total_cost := hedged_cost + generation_cost + open_cost + balancing_cost + uplift_cost + OPEX_cost]

## 8. compute margin: revenues - total_cost ============

lv_volumes_clusters_long[, margin := revenues - total_cost]


## 9.  generate a variable for peak/off peak hours: peak == 1 if hour > 8 & hour < 21, 0 otherwise =========

lv_volumes_clusters_long[, peak := fifelse(hour > 8 & hour < 21, 0, 1)]


## 10. drop unit costs variables (hedged_unit_cost, HPFC, ...)

lv_volumes_clusters_long[, HPFC := NULL]
lv_volumes_clusters_long[, hedged_unit_cost := NULL]
lv_volumes_clusters_long[, generation_unit_cost := NULL]
lv_volumes_clusters_long[, SECTOR := NULL]
lv_volumes_clusters_long[, type_viz := 'expected']

data_ru_all = lv_volumes_clusters_long

data_ru_all[, yearmonth := format(date, "%Y-%m")]        