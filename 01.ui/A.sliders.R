

### SLIDERS ###    


side_bar_width = '100%'



# : =============================================================



# A. REGULAR UPDATE =====================================



## Upload -----------------------------

input_upload_rusa <- 
    
   radioButtons(
      inputId = "input_upload_rusa",
      label = "",
      choices = c("Regular Run" = 'regular_run', "Sensitivity Analysis" = 'sensitivity'),
      selected = 'regular_run',
      width = side_bar_width)




input_upload_1 <- 

      fileInput("file1", "Choose LV File",
                multiple = FALSE)


input_upload_2 <- 

      fileInput("file2", "Choose MV File",
                multiple = FALSE)


input_upload_3 <- 

      fileInput("file3", "Choose HV File",
                multiple = FALSE)

input_upload_4 <- 

      fileInput("file4", "Choose EDS File",
                multiple = FALSE)





## Aggregation -------------------------


category_aggregation_ru <- 
    
    selectInput(
          
       inputId = 'category_aggregation_ru',
       label = 'Aggregation level',
       choices = c('Voltage' = 'voltage',
                   'Supplier' = 'supply',
                   'Client type' = 'client_type',
                   'Segment' = 'segment',
                   'Zone' = 'system_zone',
                   'Tariff Type' = 'tariff_type'),

       choices = c('Cluster' = 'cluster', 'Voltage' = 'voltage', 'Client type' = 'client_type', 'Peakload vs Off-Peak' = 'peak', 'Segment' = 'macrosegment', 'Zone' = 'system_zone', 'Tariff Type' = 'tariff_type'),
       width = side_bar_width,
       selected = 'voltage',
       multiple = TRUE)



time_granularity_ru <- 
    
    selectInput(
          
       inputId = 'voltages',
       label = 'Select Voltages',
       choices = c('All' = 'All', 'LV' = 'LV', 'MV' = 'MV', 'HV' = 'HV'),
       width = side_bar_width,
       selected = 'All',
       multiple = TRUE)
       inputId = 'time_granularity_ru',
       label = 'Time Granularity',
       choices = c('Hourly' = 'datetime', 'Date' = 'date', 'Monthly' = 'yearmonth'),
       width = side_bar_width,
       selected = 'yearmonth')



type_view_ru <- 
    
   radioButtons(
      inputId = "viz_type_ru",
      label = "Visualiation Type",
      choices = c("Contracted" = 'contracted', "Expected" = 'expected', 'Both' = 'both'),
      selected = 'contracted',
      width = side_bar_width)


datarange_ru <- 
  
  dateRangeInput(
      inputId = "daterange_ru",
      label = "Date range:",
      start = "2021-01-01",
      end = "2023-12-31")





## Export ----------------------------------

export_ru_user <-
    
    textInput(
              
       inputId = 'export_name_ru_user',
       label = 'User name',
       placeholder = 'Insert user name for file export.',
       width = side_bar_width)


export_ru_file <-
    
    textInput(
              
       inputId = 'export_name_ru_file',
       label = 'File name',
       placeholder = 'Insert output file name for file export.',
       width = side_bar_width)


















# : =================================================================================








