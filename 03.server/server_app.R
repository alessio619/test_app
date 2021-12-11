


server_app <- function(input, output, session) {
    
          
        ### Notification Center                                        
      
        ### Upload files Progress Bar    
    
        waitress <- Waitress$new("#upload_files_ru", theme = 'overlay', min = 0, max = 4)
        
        
        ### Spinners for Plot creation   
        
        waiter <- Waiter$new(c("plot_ru_volumes_procured", 'plot_ru_volumes_procured_components', 'plot_ru_volumes_procured_components_perc', 
                               'plot_ru_costs_total', 'plot_ru_costs_components', 'plot_ru_costs_components_perc', 
                               'plot_ru_revenues_total', 'plot_ru_revenues_components', 'plot_ru_revenues_components_perc', 'plot_ru_margin_total',
                               'plot_ru_margin_components', 'plot_ru_margin_components_perc_1', 'plot_ru_margin_components_perc_2', 
                               'table_ru_summary', 'table_ru_summary_unit', 
                               'plot_ru_unit_costs', 'plot_ru_unit_revenues', 'plot_ru_unit_margins',
                               'plot_ru_unit_costs_period', 'plot_ru_unit_revenues_period', 'plot_ru_unit_margins_period'),
                             
                             html = spin_2(), color = transparent(0.5))                             
                               
                               
        waiter2 <- Waiter$new(c('plot_ru_volumes_procured_pod_agg', 'plot_ru_volumes_procured_components_pod_agg', 'plot_ru_volumes_procured_components_perc_pod_agg',
                               'plot_ru_costs_total_agg', 'plot_ru_costs_components_agg', 'plot_ru_costs_components_perc_agg',
                               'plot_ru_revenues_total_agg', 'plot_ru_revenues_components_agg', 'plot_ru_revenues_components_perc_agg', 
                               'plot_ru_margin_total_agg', 'plot_ru_margin_components_perc_agg_1', 'plot_ru_margin_components_perc_agg_2'),
                             
                              html = spin_2(), color = transparent(0.5))
        
        
        ### Modal Dialogs     
        
        modal_confirm <- modalDialog(
            
                                "Are you sure you want to Archive?",
                                title = "Archive LV Sales Matrix Update",
                                fade = FALSE,
                                size = 'xl',
                                easyClose = TRUE,
                                
                                footer = tagList(
                                            actionButton("cancel", "Cancel"),
                                            actionButton("ok", "Archive", class = "btn btn-info")
                                            
                                            
                                        ))
        
        
    
  # A. UPLOAD =======================================================================================================================
    
    
    
    ## 1. REGULAR UPDATE -------------------------------------------------------------------------------------------
    
    observeEvent(input$upload_files_ru, {
    
        
        ### INITIATE CSS LOADER     
        
        waitress$inc(1)
        
        
        
        ### Table File 1 -------------------------------------------------------------------------------------------
        
        data_ru_lv = NULL
        data_ru_lv <- readr::read_csv2(file = input$file1$datapath) %>% as.data.table() 

        summed_ru_1 <- summary_upload(data_ru_lv)
        
        
        
        output$data_ru_1_table <- 
        
                    
          renderReactable(

              
            reactable(
                
                summed_ru_1, 
                
                defaultColDef = colDef(headerStyle = list(background = "#A8C0CE", color = '#fff'), format = colFormat(digits = 0)),

                bordered = TRUE,
                highlight = TRUE,
                defaultPageSize = 20,
                showPageSizeOptions = FALSE
                
                    )
                        )
        
        
        
        waitress$inc(1)
        
        
        
### Table File 2 -------------------------------------------------------------------------------------------
        
        data_ru_mv = NULL
        data_ru_mv <- readr::read_csv2(file = input$file2$datapath) %>% as.data.table() 

        summed_ru_2 <- summary_upload(data_ru_mv)
        

        output$data_ru_2_table <- 
        
                    
          renderReactable(

              
            reactable(
                
                summed_ru_2, 
                
                defaultColDef = colDef(headerStyle = list(background = "#A8C0CE", color = '#fff'), format = colFormat(digits = 0)),

                bordered = TRUE,
                highlight = TRUE,
                defaultPageSize = 20,
                showPageSizeOptions = FALSE
                
                    )
                        )
        
        
        
        waitress$inc(1)        
        
        
        
### Table File 3 -------------------------------------------------------------------------------------------
        
        data_ru_hv = NULL
        data_ru_hv <- readr::read_csv2(file = input$file3$datapath) %>% as.data.table()

        summed_ru_3 <- summary_upload(data_ru_hv)
        
         output$data_ru_3_table <- 
        
                    
          renderReactable(

              
            reactable(
                
                summed_ru_3, 
                
                defaultColDef = colDef(headerStyle = list(background = "#A8C0CE", color = '#fff'), format = colFormat(digits = 0)),

                bordered = TRUE,
                highlight = TRUE,
                defaultPageSize = 20,
                showPageSizeOptions = FALSE
                
                    )
                        )
        
        
        
        waitress$inc(1)           
        
        
        
        
### Table File 4 -------------------------------------------------------------------------------------------
        
        data_ru_eds = NULL
        data_ru_eds <- readr::read_csv2(file = input$file4$datapath) %>% as.data.table()

        summed_ru_4 <- summary_upload(data_ru_eds)
        
        
        
        output$data_ru_4_table <- 
        
                    
          renderReactable(

              
            reactable(
                
                summed_ru_4, 
                
                defaultColDef = colDef(headerStyle = list(background = "#A8C0CE", color = '#fff'), format = colFormat(digits = 0)),

                bordered = TRUE,
                highlight = TRUE,
                defaultPageSize = 20,
                showPageSizeOptions = FALSE
                
                    )
                        )
        
        
        
        waitress$inc(1)
        

        
        ## MERGE UPLOADED FILES --------
        
        list_df = list(data_ru_lv, data_ru_mv, data_ru_hv, data_ru_eds)
        list_df = list_df[!is.na(list_df)]

        data_ru_all = rbindlist(list_df) %>% as.data.table()
            data_ru_all[, yearmonth := lubridate::ym(yearmonth)]
            data_ru_all[, date := yearmonth]
            
            
        summed_ru_5 <- summary_upload(data_ru_all)
        
        
        output$data_ru_all_table <- 

            
         renderReactable(

            reactable(
                
                summed_ru_5, 
                
                defaultColDef = colDef(headerStyle = list(background = "#768591", color = '#fff'), format = colFormat(digits = 0)),

                bordered = TRUE,
                highlight = TRUE,
                defaultPageSize = 20,
                showPageSizeOptions = FALSE
                
                    )
                        )
        
        
        
        waitress$inc(1)
        
        
        
        

## Update RU v SA -----------------------------------------------------------------------------    
    

         
         
         ### CSS LOADER CLOSE
         
         waitress$close()
         
         
        
         
         
         
         is_ru = as.character(input$input_upload_rusa)
         
         output$archive_button <- renderUI({

                   if (is_ru == 'regular_run') {
                       
          actionButton(
          
                inputId = 'archive_bttn_ru',
                label = 'Archive',
                icon = icon('download'),
                width = side_bar_width,
                class = "btn-warning"
                
                )
    
                   } else {
                           
                       
          actionButton(
          
                inputId = 'placehoder_button',
                label = 'Not Possible to Archive in SA',
                icon = icon('exclamation'),
                width = side_bar_width,
                class = "btn-warning"
                
                )
                   }
             
         })

                  
                  
                  

 
    
         
         
         
    
    
         
# : ====================================================================================================================================================
                  
    
            
         
         
         
         
         
         
# B1. ANALYSIS =========================================================================================================================================

    
    
## 1. REGULAR UPDATE ----------------------------------------------------------------------------------------------------------
    

### a. Execute & Visualize --------------------------------------------------    
     
            observeEvent(input$running_bttn_ru, {
        
                
            ### Show CSS Loaders
                
            waiter$show()
     
                
            ### Update Date Range Show           
                
                 first_date = first(data_ru_all$date)
                 last_date = last(data_ru_all$date)
     
            updateDateRangeInput(
         
                    session, "daterange_ru",
                        label = "Date range:",
                        start = first_date,
                        end = last_date)
    
            
     
             
        ### Shared Dataset ====
         
        ### Shared Dataset ====
         
            if (input$voltages == 'All') {
             
                dts_ru_general = reactive(data_ru_all[date >= input$daterange_ru[1] & date <= input$daterange_ru[2]])

            } else {
                
                dts_ru_general = reactive(data_ru_all[date >= input$daterange_ru[1] & date <= input$daterange_ru[2] & voltage == input$voltages])

            } 
                


                     
    
                
                
### Results Table ----------------------------------------------------------    
                
                output$table_ru_summary <- 
                    
                    
                    renderReactable( {
                      
                       
                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_delivery_volume = sum(tot_delivery_volume),
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_hedged_volume = sum(tot_hedged_volume),
                                                       tot_generation_volume = sum(tot_generation_volume),
                                                       tot_open_volume = sum(tot_open_volume),
                                                       
                                                       # Costs
                                                       tot_total_cost = sum(tot_total_cost),
                                                       tot_hedged_cost = sum(tot_hedged_cost),
                                                       tot_generation_cost = sum(tot_generation_cost),
                                                       tot_open_cost = sum(tot_open_cost),
                                                       tot_balancing_cost = sum(tot_balancing_cost),
                                                       tot_uplift_cost = sum(tot_uplift_cost),
                                                       tot_OPEX_cost = sum(tot_OPEX_cost),
                                                       
                                                       # tot_revenues & Margin
                                                       tot_revenues = sum(tot_revenues),
                                                       tot_margin = sum(tot_margin)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c('tot_delivery_volume',
                                                              'tot_procured_volume',
                                                              'tot_hedged_volume',
                                                              'tot_generation_volume',
                                                              'tot_open_volume',
                                                              'tot_total_cost',
                                                              'tot_hedged_cost',
                                                              'tot_generation_cost',
                                                              'tot_open_cost',
                                                              'tot_balancing_cost',
                                                              'tot_uplift_cost',
                                                              'tot_OPEX_cost',
                                                              'tot_revenues',
                                                              'tot_margin')]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]

                        dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'), measure.vars = c('tot_delivery_volume',
                                                              'tot_procured_volume',
                                                              'tot_hedged_volume',
                                                              'tot_generation_volume',
                                                              'tot_open_volume',
                                                              'tot_total_cost',
                                                              'tot_hedged_cost',
                                                              'tot_generation_cost',
                                                              'tot_open_cost',
                                                              'tot_balancing_cost',
                                                              'tot_uplift_cost',
                                                              'tot_OPEX_cost',
                                                              'tot_revenues',
                                                              'tot_margin'), variable.name = 'components', value.name = 'values')
                        
                        
                        dts_ru_2 = dcast(
                            
                                    dts_ru_2,
                                    
                                         components ~ groupped,
                                         value.var = 'values'
                                    
                                     )
                        
                        
                        dts_ru_2[, components := c('Delivered Volumes', 
                                                   'Procured Volumes',
                                                   'Hedged Volumes', 
                                                   'Generation Volumes',
                                                   'Open Volumes',
                                                   'Total Costs',
                                                   'Hedged Cost',
                                                   'Generation Cost',
                                                   'Open Cost',
                                                   'Balancing Cost',
                                                   'Uplift Cost',
                                                   'OPEX Cost',
                                                   'Total tot_revenues',
                                                   'Margins')]
                        
                        names_ru_all = names(dts_ru_2)[-1]
                        
                        dts_ru_2$All <- rowSums(dts_ru_2[, ..names_ru_all])
                        
                        setcolorder(dts_ru_2, c('components', 'All'))
                        
                        
           ### Reactable             
            
            reactable(
                
                dts_ru_2, 
                
                defaultColDef = colDef(headerStyle = list(background = "#6593A6", color = '#fff'), format = colFormat(digits = 0)),
                
                  columns = list(
                    components = colDef(name = '', minWidth = 140, style = list(position = "sticky", left = 0, backgroundColor = '#eee', borderRight = "2px solid #eee", zIndex = 1)),
                    All = colDef(name = 'All Groups', style = list(borderRight = "1px solid #eee"))),
                
                bordered = TRUE,
                highlight = TRUE,
                defaultPageSize = 20,
                showPageSizeOptions = FALSE,
                
                  rowStyle = function(index) {
                     if (dts_ru_2[index, "components"] %in% c('Total tot_revenues', 'Total Costs', 'Procured Volumes', 'Delivered Volumes', 'Margins')) {
                                        list(background = "rgba(0, 0, 0, 0.05)",
                                             fontWeight = 'bold')
                     } 
                    }
                
                    )
                    
            })   
                
                
 ### Unit Results Table ----------------------------------------------------------    
                              
                
             output$table_ru_summary_unit <- 
                    
                    renderReactable( {
                       
                       
                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_delivery_volume = sum(tot_delivery_volume, na.rm = TRUE),
                                                       tot_procured_volume = sum(tot_procured_volume, na.rm = TRUE),
                                                       tot_hedged_volume = sum(tot_hedged_volume, na.rm = TRUE),
                                                       tot_generation_volume = sum(tot_generation_volume, na.rm = TRUE),
                                                       tot_open_volume = sum(tot_open_volume, na.rm = TRUE),
                                                       
                                                       # Costs
                                                       tot_total_cost = sum(tot_total_cost, na.rm = TRUE),
                                                       tot_hedged_cost = sum(tot_hedged_cost, na.rm = TRUE),
                                                       tot_generation_cost = sum(tot_generation_cost),
                                                       tot_open_cost = sum(tot_open_cost, na.rm = TRUE),
                                                       tot_balancing_cost = sum(tot_balancing_cost, na.rm = TRUE),
                                                       tot_uplift_cost = sum(tot_uplift_cost, na.rm = TRUE),
                                                       tot_OPEX_cost = sum(tot_OPEX_cost, na.rm = TRUE),
                                                       
                                                       # tot_revenues & Margin
                                                       tot_revenues = sum(tot_revenues, na.rm = TRUE),
                                                       tot_margin = sum(tot_margin, na.rm = TRUE)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c('tot_delivery_volume',
                                                              'tot_procured_volume',
                                                              'tot_hedged_volume',
                                                              'tot_generation_volume',
                                                              'tot_open_volume',
                                                              'tot_total_cost',
                                                              'tot_hedged_cost',
                                                              'tot_generation_cost',
                                                              'tot_open_cost',
                                                              'tot_balancing_cost',
                                                              'tot_uplift_cost',
                                                              'tot_OPEX_cost',
                                                              'tot_revenues',
                                                              'tot_margin')]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_cost = tot_total_cost / tot_procured_volume,
                                         unit_hedged_cost = tot_hedged_cost / tot_hedged_volume,
                                         unit_genertion_cost = tot_generation_cost / tot_generation_volume,
                                         unit_open_cost = tot_open_cost / tot_open_volume,
                                         unit_balancing_cost = tot_balancing_cost / tot_procured_volume,
                                         unit_uplift_cost = tot_uplift_cost / tot_procured_volume,
                                         unit_opex_cost = tot_OPEX_cost / tot_procured_volume,
                                         unit_revenues = tot_revenues / tot_procured_volume,
                                         unit_margins = tot_margin / tot_procured_volume
                                         )]
                        

                        dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'),
                                        
                                                  measure.vars = c('unit_total_cost',
                                                                   'unit_hedged_cost',
                                                                   'unit_genertion_cost',
                                                                   'unit_open_cost',
                                                                   'unit_balancing_cost',
                                                                   'unit_uplift_cost',
                                                                   'unit_opex_cost',
                                                                   'unit_revenues',
                                                                   'unit_margins'), variable.name = 'components', value.name = 'values')
                        
                        
                        
                        dts_ru_2 = dcast(
                            
                                    dts_ru_2,
                                    
                                         components ~ groupped,
                                         value.var = 'values'
                                    
                                     )
                        
                        
                        dts_ru_2[, components := c('Unit Total Cost',
                                                   'Unit Hedged Cost',
                                                   'Unit Generation Cost',
                                                   'Unit Open Cost',
                                                   'Unit Balancing Cost',
                                                   'Unit Uplift Cost',
                                                   'Unit OPEX Cost',
                                                   'Unit tot_revenues',
                                                   'Unit Margins')]
                        
                       
                        
                        
                        # UNIT VALUES ALL GROUP 
                        
                        dts_all = dts_ru_general()[, .(# Volumes
                                                       tot_delivery_volume = sum(tot_delivery_volume, na.rm = TRUE),
                                                       tot_procured_volume = sum(tot_procured_volume, na.rm = TRUE),
                                                       tot_hedged_volume = sum(tot_hedged_volume, na.rm = TRUE),
                                                       tot_generation_volume = sum(tot_generation_volume, na.rm = TRUE),
                                                       tot_open_volume = sum(tot_open_volume, na.rm = TRUE),
                                                       
                                                       # Costs
                                                       tot_total_cost = sum(tot_total_cost, na.rm = TRUE),
                                                       tot_hedged_cost = sum(tot_hedged_cost, na.rm = TRUE),
                                                       tot_generation_cost = sum(tot_generation_cost, na.rm = TRUE),
                                                       tot_open_cost = sum(tot_open_cost, na.rm = TRUE),
                                                       tot_balancing_cost = sum(tot_balancing_cost, na.rm = TRUE),
                                                       tot_uplift_cost = sum(tot_uplift_cost, na.rm = TRUE),
                                                       tot_OPEX_cost = sum(tot_OPEX_cost, na.rm = TRUE),
                                                       
                                                       # tot_revenues & Margin
                                                       tot_revenues = sum(tot_revenues, na.rm = TRUE),
                                                       tot_margin = sum(tot_margin, na.rm = TRUE))] 



                        nam_ru_2_all = names(dts_all)
                        nam2_ru_2_all = nam_ru_2_all[!nam_ru_2_all %in% c('tot_delivery_volume',
                                                              'tot_procured_volume',
                                                              'tot_hedged_volume',
                                                              'tot_generation_volume',
                                                              'tot_open_volume',
                                                              'tot_total_cost',
                                                              'tot_hedged_cost',
                                                              'tot_generation_cost',
                                                              'tot_open_cost',
                                                              'tot_balancing_cost',
                                                              'tot_uplift_cost',
                                                              'tot_OPEX_cost',
                                                              'tot_revenues',
                                                              'tot_margin')]
                
                        temp2_ru_2_all = dts_all[, ..nam2_ru_2_all]
                        
                        
                        dts_all = dts_all[,  .(unit_total_cost = tot_total_cost / tot_procured_volume,
                                         unit_hedged_cost = tot_hedged_cost / tot_hedged_volume,
                                         unit_genertion_cost = tot_generation_cost / tot_generation_volume,
                                         unit_open_cost = tot_open_cost / tot_open_volume,
                                         unit_balancing_cost = tot_balancing_cost / tot_procured_volume,
                                         unit_uplift_cost = tot_uplift_cost / tot_procured_volume,
                                         unit_opex_cost = tot_OPEX_cost / tot_procured_volume,
                                         unit_revenues = tot_revenues / tot_procured_volume,
                                         unit_margins = tot_margin / tot_procured_volume
                                         )]
                        
                        
                       dts_all = melt(dts_all,
                       
                               id.vars = c('unit_total_cost',
                                           'unit_hedged_cost',
                                           'unit_genertion_cost',
                                           'unit_open_cost',
                                           'unit_balancing_cost',
                                           'unit_uplift_cost',
                                           'unit_opex_cost',
                                           'unit_revenues',
                                           'unit_margins'), variable.name = 'components', value.name = 'values')
                                                
                       row_all = as.numeric(as.vector(dts_all[1,]))

                       dts_ru_2_all = data.frame(components = colnames(dts_all),
                                                 All = row_all)
                        
                        
                        dts_ru_2$All <- dts_ru_2_all$All
                        
                        setcolorder(dts_ru_2, c('components', 'All'))
            
                        
                        
                        
            ### Reactable             

            reactable(
                
                dts_ru_2, 
                
                defaultColDef = colDef(headerStyle = list(background = "#6593A6", color = '#fff'), format = colFormat(digits = 0)),
                
                  columns = list(
                    components = colDef(name = '', minWidth = 140, style = list(position = "sticky", left = 0, backgroundColor = '#eee', borderRight = "2px solid #eee", zIndex = 1)),
                    All = colDef(name = 'All Groups', style = list(borderRight = "1px solid #eee"))),
                
                bordered = TRUE,
                highlight = TRUE,
                defaultPageSize = 20,
                showPageSizeOptions = FALSE,
                
                  rowStyle = function(index) {
                     if (dts_ru_2[index, "components"] %in% c('Unit Total Cost', 'Unit tot_revenues')) {
                                        list(background = "rgba(0, 0, 0, 0.05)",
                                             fontWeight = 'bold')
                     } 
                    }
                
                    )
            
            
            
            
            
                    
            })
             
             
### Unit Costs Graph - Bar Period  
                
               output$plot_ru_unit_costs_period <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_total_cost = sum(total_cost)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_total_cost'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_cost = tot_total_cost / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'),
                                        
                                                  measure.vars = c('unit_total_cost'), variable.name = 'components', value.name = 'values')
                        
 


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~groupped,
                                y = ~values,
                                color = ~components,
                                opacity = 0.75,
                                colors = '#C05D78'
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Unit Costs by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
### Unit tot_revenues Graph - Bar Period  
                
               output$plot_ru_unit_revenues_period <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_revenues = sum(tot_revenues)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_revenues'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_revenues = tot_revenues / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'),
                                        
                                                  measure.vars = c('unit_total_revenues'), variable.name = 'components', value.name = 'values')
                        
 


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~groupped,
                                y = ~values,
                                color = ~components,
                                opacity = 0.75,
                                colors = '#6F9B91'
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Unit tot_revenues by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
### Unit Margins Graph - Bar Period  
                
               output$plot_ru_unit_margins_period <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_margins = sum(margin)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_margins'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_margin = tot_margins / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'),
                                        
                                                  measure.vars = c('unit_total_margin'), variable.name = 'components', value.name = 'values')
                        
 


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~groupped,
                                y = ~values,
                                color = ~components,
                                opacity = 0.75,
                                colors = "Paired"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Unit Margin by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })        
               
               
               
### Unit Costs Graph - Line  
                
               output$plot_ru_unit_costs <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru, 'yearmonth')
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_total_cost = sum(total_cost)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_total_cost',
                                                              'yearmonth'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_cost = tot_total_cost / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped', 'yearmonth'),
                                        
                                                  measure.vars = c('unit_total_cost'), variable.name = 'components', value.name = 'values')
                        
 


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~yearmonth,
                                y = ~values,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Paired",
                            
                            type = 'scatter',
                            mode = 'lines+markers'
                             
                             ) %>% 
                         
                         layout(
                             
                             title = list(text = 'Unit Costs by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
### Unit tot_revenues Graph - Line 
                
               output$plot_ru_unit_revenues <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru, 'yearmonth')
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_revenues = sum(tot_revenues)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_revenues',
                                                              'yearmonth'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_revenues = tot_revenues / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped', 'yearmonth'),
                                        
                                                  measure.vars = c('unit_total_revenues'), variable.name = 'components', value.name = 'values')
                        
 


                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~yearmonth,
                                y = ~values,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Paired",
                            
                            type = 'scatter',
                            mode = 'lines+markers'
                             
                             ) %>% 
                         
                         layout(
                             
                             title = list(text = 'Unit tot_revenues by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )                        

                                       
                  
                  })
               
               
               
               
### Unit Margins Graph - Line  
                
               output$plot_ru_unit_margins <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru, 'yearmonth')
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_margins = sum(margin)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_margins',
                                                              'yearmonth'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_margin = tot_margins / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped', 'yearmonth'),
                                        
                                                  measure.vars = c('unit_total_margin'), variable.name = 'components', value.name = 'values')
                        
 


                        
                         # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~yearmonth,
                                y = ~values,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Paired",
                            
                            type = 'scatter',
                            mode = 'lines+markers'
                             
                             ) %>% 
                         
                         layout(
                             
                             title = list(text = 'Unit Margins by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })                   
               
               
                          
                        
                
                
                
              #### Volumes Graphs ----------------------------------------------------
                
               output$plot_ru_volumes_procured <- 
                    
                   
                   renderPlotly({
                       
                       
                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_1 = c(input$category_aggregation_ru, 'yearmonth')
                       dts_ru_1 = dts_ru_general()[, .(tot_procured_volume = sum(tot_procured_volume)), keyby = aggregators_ru_1]  
                                
                       
                        
                        nam_ru_1 = names(dts_ru_1)
                        nam2_ru_1 = nam_ru_1[!nam_ru_1 %in% c('tot_procured_volume', 'yearmonth')]
                
                        temp2_ru_1 = dts_ru_1[, ..nam2_ru_1]

                        dts_ru_1$groupped <- apply(temp2_ru_1, 1, function(x) paste(x, collapse = "-"))
                        dts_ru_1[, groupped := as.factor(groupped)]
                     
                        
                        # Graph Making 
                        
                          
                  plot_ly(dts_ru_1, 
                            
                                x = ~yearmonth,
                                y = ~tot_procured_volume,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Set3"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Total Procured Volumes', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h"), 
                                 
                             margin = m_1,
                             
                             xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                             yaxis = list(title = 'MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                  
                  
                  })
               
               
               
               
              ### Volumes Graphs - Procured  
                
               output$plot_ru_volumes_procured_components <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(tot_hedged_volume = sum(tot_hedged_volume), tot_generation_volume = sum(tot_generation_volume), tot_open_volume = sum(tot_open_volume)), keyby = aggregators_ru_2] 
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c('tot_hedged_volume', 'tot_generation_volume', 'tot_open_volume')]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]

                        dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'), measure.vars = c('tot_hedged_volume', 'tot_generation_volume', 'tot_open_volume'), variable.name = 'components', value.name = 'volume')


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~volume,
                                y = ~groupped,
                                color = ~components,
                                opacity = 0.75,
                                colors = "Paired"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Procured Volumes Sources by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "MWh", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = '',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
               ### Volumes Graphs - Procured PIE 
                
               output$plot_ru_volumes_procured_components_perc <- 
                    
                   
                   renderPlotly({
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_3 = c(input$category_aggregation_ru)
                       dts_ru_3 = dts_ru_general()[, .(tot_hedged_volume = sum(tot_hedged_volume), tot_generation_volume = sum(tot_generation_volume), tot_open_volume = sum(tot_open_volume)), keyby = aggregators_ru_3] 
                       
                        
                        nam_ru_3 = names(dts_ru_3)
                        nam2_ru_3 = nam_ru_3[!nam_ru_3 %in% c('tot_hedged_volume', 'tot_generation_volume', 'tot_open_volume')]
                
                        temp2_ru_3 = dts_ru_3[, ..nam2_ru_3]

                        dts_ru_3$groupped <- apply(temp2_ru_3, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_3 = melt(dts_ru_3, id.vars = c('groupped'), measure.vars = c('tot_hedged_volume', 'tot_generation_volume', 'tot_open_volume'), variable.name = 'components', value.name = 'volume')
                        
                        dts_ru_3[, components := as.factor(components)]

                        
                    
                        
                        # Graph Making 
                        
                    plot_ly(
                        
                        dts_ru_3,
                        
                            labels = ~components,
                            values = ~volume,
                            type = 'pie',
                            opacity = 0.75,
                            colors = "Dark2"
                            
                        
                        ) %>% 
                     
                      layout(
                          title = list(text = 'Voltage Volumes by Source'),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          legend = list(orientation = "h", x = 0, y = 0, title = list(text = ''))) 

                        
                    })
               
               
               
              #### Costs Graphs ----------------------------------------------------
                
               output$plot_ru_costs_total <- 
                    
                   
                   renderPlotly({
                       
                       
                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_1 = c(input$category_aggregation_ru, 'yearmonth')
                       dts_ru_general()[, tot_cost := sum(tot_hedged_cost + tot_generation_cost + tot_open_cost + tot_balancing_cost + tot_uplift_cost + tot_OPEX_cost)]

                       dts_ru_1 = dts_ru_general()[, .(total_cost = sum(tot_cost)), keyby = aggregators_ru_1]  
                       dts_ru_1 
                       
                        nam_ru_1 = names(dts_ru_1)
                        nam2_ru_1 = nam_ru_1[!nam_ru_1 %in% c('total_cost', 'yearmonth')]
                
                        temp2_ru_1 = dts_ru_1[, ..nam2_ru_1]

                        dts_ru_1$groupped <- apply(temp2_ru_1, 1, function(x) paste(x, collapse = "-"))
                        dts_ru_1[, groupped := as.factor(groupped)]
                     
                        
                        # Graph Making 
                        
                          
                  plot_ly(dts_ru_1, 
                            
                                x = ~yearmonth,
                                y = ~total_cost,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Set3"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Total Volumes Costs', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 title = list(text = '')), 
                                 
                             margin = m_1,
                             
                             xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                             yaxis = list(title = 'euro',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                  
                  
                  }) 
               
               
               
             ### Costs Graphs - Total  
                
               output$plot_ru_costs_components <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(tot_hedged_cost = sum(tot_hedged_cost), tot_generation_cost = sum(tot_generation_cost), tot_open_cost = sum(tot_open_cost), tot_balancing_cost = sum(tot_balancing_cost), tot_OPEX_cost = sum(tot_OPEX_cost), tot_uplift_cost = sum(tot_uplift_cost)), keyby = aggregators_ru_2] 

                       
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c('tot_hedged_cost', 'tot_generation_cost', 'tot_open_cost', 'tot_balancing_cost', 'tot_OPEX_cost', 'tot_uplift_cost')]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]

                        dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'), measure.vars = c('tot_hedged_cost', 'tot_generation_cost', 'tot_open_cost', 'tot_open_cost', 'tot_balancing_cost', 'tot_OPEX_cost', 'tot_uplift_cost'), variable.name = 'components', value.name = 'costs')


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~costs,
                                y = ~groupped,
                                color = ~components,
                                opacity = 0.75,
                                colors = "Paired"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Volumes Costs Composition by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 title = list(text = ''),
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "euro", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = '',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
               ### Costs Graphs - PIE 
                
               output$plot_ru_costs_components_perc <- 
                    
                   
                   renderPlotly({
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_3 = c(input$category_aggregation_ru)
                       dts_ru_3 = dts_ru_general()[, .(tot_hedged_cost = sum(tot_hedged_cost), tot_generation_cost = sum(tot_generation_cost), tot_open_cost = sum(tot_open_cost), tot_balancing_cost = sum(tot_balancing_cost), tot_OPEX_cost = sum(tot_OPEX_cost), tot_uplift_cost = sum(tot_uplift_cost)), keyby = aggregators_ru_3] 
                       
                        
                        nam_ru_3 = names(dts_ru_3)
                        nam2_ru_3 = nam_ru_3[!nam_ru_3 %in% c('tot_hedged_cost', 'tot_generation_cost', 'tot_open_cost', 'tot_balancing_cost', 'tot_OPEX_cost', 'tot_uplift_cost')]
                
                        temp2_ru_3 = dts_ru_3[, ..nam2_ru_3]

                        dts_ru_3$groupped <- apply(temp2_ru_3, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_3 = melt(dts_ru_3, id.vars = c('groupped'), measure.vars = c('tot_hedged_cost', 'tot_generation_cost', 'tot_open_cost', 'tot_balancing_cost', 'tot_OPEX_cost', 'tot_uplift_cost'), variable.name = 'components', value.name = 'costs')
                        
                        dts_ru_3[, components := as.factor(components)]

                        
                    
                        
                        # Graph Making 
                        
                    plot_ly(
                        
                        dts_ru_3,
                        
                            labels = ~components,
                            values = ~costs,
                            type = 'pie',
                            opacity = 0.75,
                            colors = "Dark2"
                            
                        
                        ) %>% 
                     
                      layout(
                          title = list(text = 'Voltage Volumes Cost by Source', x = 0),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          legend = list(orientation = "h", x = 0, y = 0, title = list(text = ''))) 

                        
                    })   
               
               
               
              #### Revenues Graphs ----------------------------------------------------
                
               output$plot_ru_revenues_total <- 
                    
                   
                   renderPlotly({
                       
                       
                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_1 = c(input$category_aggregation_ru, 'yearmonth')

                       dts_ru_1 = dts_ru_general()[, .(total_revenues = sum(tot_revenues)), keyby = aggregators_ru_1]  
                       dts_ru_1 
                       
                        nam_ru_1 = names(dts_ru_1)
                        nam2_ru_1 = nam_ru_1[!nam_ru_1 %in% c('total_revenues', 'yearmonth')]
                
                        temp2_ru_1 = dts_ru_1[, ..nam2_ru_1]

                        dts_ru_1$groupped <- apply(temp2_ru_1, 1, function(x) paste(x, collapse = "-"))
                        dts_ru_1[, groupped := as.factor(groupped)]
                     
                        
                        # Graph Making 
                        
                          
                  plot_ly(dts_ru_1, 
                            
                                x = ~yearmonth,
                                y = ~total_revenues,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Set3"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Total Revenues', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 title = list(text = '')), 
                                 
                             margin = m_1,
                             
                             xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                             yaxis = list(title = 'euro',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                  
                  
                  })
               
               
               
               
              ### Revenues Graphs - Total  
                
               output$plot_ru_revenues_components <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(total_revenues = sum(tot_revenues)), keyby = aggregators_ru_2] 

                       
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c('total_revenues')]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]

                        dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'), measure.vars = c('total_revenues'), variable.name = 'components', value.name = 'revenues')


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~revenues,
                                y = ~groupped,
                                color = ~components,
                                opacity = 0.75,
                                colors = "Paired"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Total Revenues by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 title = list(text = ''),
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "euro", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = '',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
               ### Revenues Graphs - PIE 
                
               output$plot_ru_revenues_components_perc <- 
                    
                   
                   renderPlotly({
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_3 = c(input$category_aggregation_ru)
                        dts_ru_3 = dts_ru_general()[, .(total_revenues = sum(tot_revenues)), keyby = aggregators_ru_3] 
                       
                        
                        nam_ru_3 = names(dts_ru_3)
                        nam2_ru_3 = nam_ru_3[!nam_ru_3 %in% c('total_revenues')]
                
                        temp2_ru_3 = dts_ru_3[, ..nam2_ru_3]

                        dts_ru_3$groupped <- apply(temp2_ru_3, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_3 = melt(dts_ru_3, id.vars = c('groupped'), measure.vars = c('total_revenues'), variable.name = 'components', value.name = 'revenues')
                        
                        dts_ru_3[, components := as.factor(components)]

                        
                    
                        
                        # Graph Making 
                        
                    plot_ly(
                        
                        dts_ru_3,
                        
                            labels = ~groupped,
                            values = ~revenues,
                            type = 'pie',
                            opacity = 0.75,
                            colors = "Dark2"
                            
                        
                        ) %>% 
                     
                      layout(
                          title = list(text = 'Voltage Revenues by Group', x = 0),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          legend = list(orientation = "h", x = 0, y = 0, title = list(text = ''))) 

                        
                    })
               
               
              #### Margin Graphs ----------------------------------------------------
                
               output$plot_ru_margin_total <- 
                    
                   
                   renderPlotly({
                       
                       
                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_1 = c(input$category_aggregation_ru, 'yearmonth')

                       dts_ru_1 = dts_ru_general()[, .(total_margins = sum(tot_margin)), keyby = aggregators_ru_1]  
                       dts_ru_1 
                       
                        nam_ru_1 = names(dts_ru_1)
                        nam2_ru_1 = nam_ru_1[!nam_ru_1 %in% c('total_margins', 'yearmonth')]
                
                        temp2_ru_1 = dts_ru_1[, ..nam2_ru_1]

                        dts_ru_1$groupped <- apply(temp2_ru_1, 1, function(x) paste(x, collapse = "-"))
                        dts_ru_1[, groupped := as.factor(groupped)]
                     
                        
                        # Graph Making 
                        
                          
                  plot_ly(dts_ru_1, 
                            
                                x = ~yearmonth,
                                y = ~total_margins,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Set3"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Total Margins', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 title = list(text = '')), 
                                 
                             margin = m_1,
                             
                             xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                             yaxis = list(title = 'euro',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                  
                  
                  })
               
               
               
               
              ### Margin Graphs - Total  
                
               output$plot_ru_margin_components <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(total_margins = sum(tot_margin)), keyby = aggregators_ru_2] 

                       
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c('total_margins')]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]

                        dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'), measure.vars = c('total_margins'), variable.name = 'components', value.name = 'margins')


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~margins,
                                y = ~groupped,
                                color = ~components,
                                opacity = 0.75,
                                colors = "Paired"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Total Margins by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 title = list(text = ''),
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "euro", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = '',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
               ### Margin Graphs - PIE 
                
               output$plot_ru_margin_components_perc_1 <- 
                    
                   
                   renderPlotly({
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_3 = c(input$category_aggregation_ru)
                       dts_ru_3 = dts_ru_general()[, .(total_margins = sum(tot_margin)), keyby = aggregators_ru_3] 
                       
                       dts_ru_3 = dts_ru_3[total_margins < 0]
                       dts_ru_3 = dts_ru_3[, total_margins := -total_margins]

                        
                        nam_ru_3 = names(dts_ru_3)
                        nam2_ru_3 = nam_ru_3[!nam_ru_3 %in% c('total_margins')]
                
                        temp2_ru_3 = dts_ru_3[, ..nam2_ru_3]

                        dts_ru_3$groupped <- apply(temp2_ru_3, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_3 = melt(dts_ru_3, id.vars = c('groupped'), measure.vars = c('total_margins'), variable.name = 'components', value.name = 'margins')
                        
                        dts_ru_3[, components := as.factor(components)]

                        
                    
                        
                        # Graph Making 
                        
                    plot_ly(
                        
                        dts_ru_3,
                        
                            labels = ~groupped,
                            values = ~margins,
                            type = 'pie',
                            opacity = 0.75,
                            colors = "Dark2"
                            
                        
                        ) %>% 
                     
                      layout(
                          
                          title = 'Negative Margins',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          legend = list(orientation = "h", x = 0, y = 0, title = list(text = ''))) 

                        
                    }) 
               
               
               output$plot_ru_margin_components_perc_2 <- 
                    
                   
                   renderPlotly({
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_3 = c(input$category_aggregation_ru)
                       dts_ru_3 = dts_ru_general()[, .(total_margins = sum(tot_margin)), keyby = aggregators_ru_3] 
                       
                       dts_ru_3 = dts_ru_3[total_margins > 0]

                        
                        nam_ru_3 = names(dts_ru_3)
                        nam2_ru_3 = nam_ru_3[!nam_ru_3 %in% c('total_margins')]
                
                        temp2_ru_3 = dts_ru_3[, ..nam2_ru_3]

                        dts_ru_3$groupped <- apply(temp2_ru_3, 1, function(x) paste(x, collapse = "-"))
                        
                        dts_ru_3 = melt(dts_ru_3, id.vars = c('groupped'), measure.vars = c('total_margins'), variable.name = 'components', value.name = 'margins')
                        
                        dts_ru_3[, components := as.factor(components)]

                        
                    
                        
                        # Graph Making 
                        
                    plot_ly(
                        
                        dts_ru_3,
                        
                            labels = ~groupped,
                            values = ~margins,
                            type = 'pie',
                            opacity = 0.75,
                            colors = "Dark2"
                            
                        
                        ) %>% 
                     
                      layout(
                          
                          title = 'Positive Margins',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          legend = list(orientation = "h", x = 0, y = 0, title = list(text = ''))) 

                        
                    })      
               
               
               
### Unit Costs Graph - Bar Period  
                
               output$plot_ru_unit_costs_period <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_total_cost = sum(tot_total_cost)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_total_cost'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_cost = tot_total_cost / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'),
                                        
                                                  measure.vars = c('unit_total_cost'), variable.name = 'components', value.name = 'values')
                        
 


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~groupped,
                                y = ~values,
                                color = ~components,
                                opacity = 0.75,
                                colors = '#C05D78'
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Unit Costs by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
### Unit Revenues Graph - Bar Period  
                
               output$plot_ru_unit_revenues_period <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_revenues = sum(tot_revenues)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_revenues'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_revenues = tot_revenues / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'),
                                        
                                                  measure.vars = c('unit_total_revenues'), variable.name = 'components', value.name = 'values')
                        
 


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~groupped,
                                y = ~values,
                                color = ~components,
                                opacity = 0.75,
                                colors = '#6F9B91'
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Unit Revenues by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
### Unit Margins Graph - Bar Period  
                
               output$plot_ru_unit_margins_period <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru)
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_margins = sum(tot_margin)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_margins'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_margin = tot_margins / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped'),
                                        
                                                  measure.vars = c('unit_total_margin'), variable.name = 'components', value.name = 'values')
                        
 


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~groupped,
                                y = ~values,
                                color = ~components,
                                opacity = 0.75,
                                colors = "Paired"
                             
                             ) %>% 
                         
                         add_bars() %>%
                         
                         layout(
                             
                             title = list(text = 'Unit Margin by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })        
               
               
               
### Unit Costs Graph - Line  
                
               output$plot_ru_unit_costs <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru, 'yearmonth')
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_total_cost = sum(tot_total_cost)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_total_cost',
                                                              'yearmonth'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_cost = tot_total_cost / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped', 'yearmonth'),
                                        
                                                  measure.vars = c('unit_total_cost'), variable.name = 'components', value.name = 'values')
                        
 


                        
                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~yearmonth,
                                y = ~values,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Paired",
                            
                            type = 'scatter',
                            mode = 'lines+markers'
                             
                             ) %>% 
                         
                         layout(
                             
                             title = list(text = 'Unit Costs by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })
               
               
               
               
### Unit Revenues Graph - Line 
                
               output$plot_ru_unit_revenues <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru, 'yearmonth')
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_revenues = sum(tot_revenues)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_revenues',
                                                              'yearmonth'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_revenues = tot_revenues / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped', 'yearmonth'),
                                        
                                                  measure.vars = c('unit_total_revenues'), variable.name = 'components', value.name = 'values')
                        
 


                        # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~yearmonth,
                                y = ~values,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Paired",
                            
                            type = 'scatter',
                            mode = 'lines+markers'
                             
                             ) %>% 
                         
                         layout(
                             
                             title = list(text = 'Unit Revenues by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )                        

                                       
                  
                  })
               
               
               
               
### Unit Margins Graph - Line  
                
               output$plot_ru_unit_margins <- 
                    
                   
                   renderPlotly( {
                       

                       # Data Aggregation and Filtering 
                       
                       aggregators_ru_2 = c(input$category_aggregation_ru, 'yearmonth')
                       
                       ### to eliminate then
                   
                       dts_ru_2 = dts_ru_general()[, .(# Volumes
                                                       tot_procured_volume = sum(tot_procured_volume),
                                                       tot_margins = sum(tot_margin)),
                                                   
                                                   keyby = aggregators_ru_2] 
                       
                       
                       
                       
                        
                        nam_ru_2 = names(dts_ru_2)
                        nam2_ru_2 = nam_ru_2[!nam_ru_2 %in% c(
                                                              'tot_procured_volume',
                                                              'tot_margins',
                                                              'yearmonth'
                                                              )]
                
                        temp2_ru_2 = dts_ru_2[, ..nam2_ru_2]
                        
                        
                        dts_ru_2[, `:=` (unit_total_margin = tot_margins / tot_procured_volume)] 


						            dts_ru_2$groupped <- apply(temp2_ru_2, 1, function(x) paste(x, collapse = "-"))
                        
					            	dts_ru_2 = melt(dts_ru_2, id.vars = c('groupped', 'yearmonth'),
                                        
                                                  measure.vars = c('unit_total_margin'), variable.name = 'components', value.name = 'values')
                        
 


                        
                         # Graph Making 
                        
                    plot_ly(dts_ru_2, 
                            
                                x = ~yearmonth,
                                y = ~values,
                                color = ~groupped,
                                opacity = 0.75,
                                colors = "Paired",
                            
                            type = 'scatter',
                            mode = 'lines+markers'
                             
                             ) %>% 
                         
                         layout(
                             
                             title = list(text = 'Unit Margins by Group', x = 0),
                             
                             barmode = 'stack',
                             
                             legend = list(
                                 orientation = "h",
                                 y = -0.2), 
                                 
                             margin = m_1,
                             
                            xaxis = list(title = "", zerolinecolor = '#ffff', zerolinewidth = 2),
                            yaxis = list(title = 'eur / MWh',  zerolinecolor = '#ffff', zerolinewidth = 2)
                            
                        )
                                       
                  
                  })                      
               
               
               
 
                           
                       
                        
            output$download_output_ru <- 
                        
                        downloadHandler(
                            
                            filename = function() { paste(Sys.Date(), '-MainAnalysis-', input$export_name_ru_user, '-',  input$export_name_ru_file, '.csv', sep = '') },
                            content = function(file) {write.csv2(dts_ru_archive, file, row.names = F) } #fwrite(dts_ru_archive, file, sep = ';')
                            
                            )
                
        
            })
    


    
        ### b. Archive ---------------------------
    
            observeEvent(input$archive_bttn_ru, {
                
                
              showModal(modal_confirm)  
 
            })
                
                  observeEvent(input$ok, {
                        
               ### Export Results                                          
                
                      setwd('..')
                      
    archive = file.path(getwd(), input$selectpath, '6.Sales.Matrix','LV', 'Archive')       

   if (file.exists(paste(archive, paste(Sys.Date(), input$export_name_ru_user, sep = '_'), sep = "/", collapse = "/"))) {
     archive.date = paste(archive, paste(Sys.Date(), input$export_name_ru_user, sep = '_'), sep = "/", collapse = "/")
   } else {
     dir.create(paste(archive, paste(Sys.Date(), input$export_name_ru_user, sep = '_'), sep = "/", collapse = "/"))
     dir.create(paste(archive, paste(Sys.Date(), input$export_name_ru_user, sep = '_'), 'inputs', sep = "/", collapse = "/"))
     archive.date = paste(archive, paste(Sys.Date(), input$export_name_ru_user, sep = '_'), sep = "/", collapse = "/")
   }                      

                    ### Archiving file names
                    
                        archive_name = paste0(Sys.Date(), '-regular_run.rds')
                        archive_name_summary = paste0(Sys.Date(), '-regular_run_summary.csv')
                        
                        removeModal()
                        
                      
                      ### Save files into the Archive
                        
                        odir_rds = file.path(getwd(), 'regular_run', '6.Sales.Matrix', 'Group-SM', 'Archive', archive_name)

                        odir_csv = file.path(getwd(), 'regular_run', '6.Sales.Matrix', 'Group-SM', 'Archive', archive_name_summary)


                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                                                
                        saveRDS(data_ru_all, file = odir_rds)
                        
                        
                        
                        data_ru_all_csv = data_ru_all[, .(tot_procured_volume = sum(tot_procured_volume),
                                                           tot_delivery_volume = sum(tot_delivery_volume),
                                                           tot_hedged_volume = sum(tot_hedged_volume),
                                                           tot_open_volume = sum(tot_open_volume),
                                                           tot_revenues = sum(tot_revenues),
                                                           tot_hedged_cost = sum(tot_hedged_cost),
                                                           tot_open_cost = sum(tot_open_cost),
                                                           tot_balancing_cost = sum(tot_balancing_cost),
                                                           tot_uplift_cost = sum(tot_uplift_cost),
                                                           tot_OPEX_cost = sum(tot_OPEX_cost),
                                                           tot_total_cost = sum(tot_total_cost),
                                                           tot_margin = sum(tot_margin)),
                                                    keyby = c('yearmonth', 'segment')]
                        
                        
                        
                        write.csv2(data_ru_all_csv, file = odir_csv, row.names = F)
                      

                        
                        ### Show Notification

                            showNotification("Files Archived", type = "warning")
                            
                            
                            })
                  
                
                 observeEvent(input$cancel, {
                
                     removeModal()
                     
                     })               
  
                
                

                

        
                
        
     
         

# : ==================================================================================                  
  
    
    
    

                
        
          
         
         
         
         
    
# : ==================================================================================                  
  
    
    
    

         

    
    
        
    })     
  

  
  
  
  
}