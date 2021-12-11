


# : ----------------------------



# A. PACKAGES =======================================================================

library(here)
library(data.table)
library(plotly)





# B. DATA UPLOAD ====================================================================

dts = readRDS(here('02.data', 'dts.rds'))



# C. PLOTS ==========================================================================


### VARS STRUCTURE

# POD_EC = ID



## AGGR

# MACROSEGMENT

# usage 
# system_zone 
# comm_category 
# tariff 
# cluster 
# segment 
# client_type 
# voltage 
# type_viz 



## TIME AGGR

# year 
# month 
# hour 
# date --


## OTHER AGGR

# peak 





### 01. Volumes ---------------------------------------------

# delivery_volume 
# procured_volume 
# hedged_volume 
# generation_volume 
# open_volume 


# Graph 1: 

### Procured 



temp = dts[, .(tot_procured_volume = sum(procured_volume)), keyby = .(system_zone, month)] 

nam = names(temp)
nam2 = nam[!nam %in% c('month', 'tot_procured_volume')]

temp2 = temp[, ..nam2]

temp$groupped <- apply(temp2, 1, function(x) paste(x, collapse = "-"))


temp_g = ggplot(temp,

        aes(
            x = month,
            y = tot_procured_volume,
            fill = groupped)
           ) + 
        
    geom_col() + 
    
        xlab('') + ylab('MWh') +
    
        theme_minimal() + scale_fill_brewer(palette = 2) 
    


ggplotly(temp_g)



### COMPONENTS


temp = dts[, .(tot_hedged_volume = sum(hedged_volume), tot_generation_volume = sum(generation_volume), tot_open_volume = sum(open_volume)), keyby = .(system_zone)] 

nam = names(temp)
nam2 = nam[!nam %in% c('tot_hedged_volume', 'tot_generation_volume', 'tot_open_volume')]

temp2 = temp[, ..nam2]

temp$groupped <- apply(temp2, 1, function(x) paste(x, collapse = "-"))

temp = melt(temp, id.vars = c('groupped'), measure.vars = c('tot_hedged_volume', 'tot_generation_volume', 'tot_open_volume'), variable.name = 'components', value.name = 'volume')

temp_g = ggplot(temp,

        aes(
            x = groupped,
            y = volume,
            fill = components)
           ) + 
        
    geom_col() + 
    
        xlab('') + ylab('MWh') +
    
        theme_minimal() + scale_fill_brewer(palette = 2) 
    


ggplotly(temp_g)




### 02. Costs ----------------------------------------------

# hedged_cost 
# generation_cost 
# open_cost 
# balancing_cost 
# uplift_cost 
# OPEX_cost 
# total_cost 





### 03. Revenues ----------------------------------------------

# revenues 





### 04. Margins ----------------------------------------------

# margin 



