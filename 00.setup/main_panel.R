

### MAIN PANELS ###    





# : ---------





# 0. INTRO ========================================


main_panel_intro <- 
    
  # Main Panel  --------------------------------

    mainPanel(
        
      ## Title Main Panel              
        
        h2('Introduction and Instructions'),
        h4('Here we will display instruction guide and general functionalities.'),
        
        br(),

         fluidRow(
             
            column(
                
                width = 11, offset = 0

            )
        )        
        )




# : ---------





# A. REGULAR UPDATE ========================================


  ## Upload --------------------------------

    main_panel_upload_ru <- 
    

      mainPanel(
        
      ## Title Main Panel              
        
         fluidRow(
             
            column(
                
                width = 12, offset = 0,
                
                tabsetPanel(
                    
                    tabPanel("Forecast by Cluster & Cluster information", 
                             
                             h4('Summary File 1 - Regular Update'),
                             
                             br(),
                             
                             reactableOutput(outputId = 'data_ru_1_table') 
                             
                             ),
                    
                    tabPanel("Supply Charges", 
                             
                             h4('Summary File 2 - Regular Update'),
                             
                             br(),
                             
                             reactableOutput(outputId = 'data_ru_2_table') 
                             
                             ),
                    
                    tabPanel("Transfer Prices", 
                             
                             h4('Summary File 3 - Regular Update'),
                             
                             br(),
                             
                             reactableOutput(outputId = 'data_ru_3_table') 
                             
                             ),
                    
                    tabPanel("Final Dataset", 
                             
                             h4('Merged Table - Regular Update'),
                             
                             br(),
                             
                             reactableOutput(outputId = 'data_ru_all_table') 
                             
                             ),
                    
                )

            )
        )        
        )

    
    
    
  ## Analysis --------------------------------

    main_panel_analysis_ru <- 
    

      mainPanel(
        
      ## Title Main Panel              
        
         fluidRow(
             
            column(
                
                width = 12, offset = 0.5,
                
                
                tabsetPanel(
                    
                    
                    tabPanel("Volumes", 
                            
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 12,
                                     
                                     br(),
                                     
                                     plotlyOutput('plot_ru_volumes_procured', height = '350px')
                                        
                                       ),
                                 
                             ), 
                             
                             hr(),
        
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 9,
                                     
                                     plotlyOutput('plot_ru_volumes_procured_components', height = '400px')
                                        
                                       ),
                                 
                                 column(
                                     
                                     width = 3,
                                     
                                     plotlyOutput('plot_ru_volumes_procured_components_perc', height = '400px')
                                 )
                                    )
                                    
                             ),
                    
                    tabPanel("Costs", 
                            
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 12,
                                     
                                     br(),
                                     
                                     plotlyOutput('plot_ru_costs_total', height = '350px')
                                        
                                       ),
                                 
                             ), 
                             
                             hr(),
        
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 9,
                                     
                                     plotlyOutput('plot_ru_costs_components', height = '400px')
                                        
                                       ),
                                 
                                 column(
                                     
                                     width = 3,
                                     
                                     plotlyOutput('plot_ru_costs_components_perc', height = '400px')
                                 )
                                    )
                                    
                             ),
                    
                    tabPanel("Revenues", 
                            
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 12,
                                     
                                     br(),
                                     
                                     plotlyOutput('plot_ru_revenues_total', height = '350px')
                                        
                                       ),
                                 
                             ), 
                             
                             hr(),
        
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 9,
                                     
                                     plotlyOutput('plot_ru_revenues_components', height = '400px')
                                        
                                       ),
                                 
                                 column(
                                     
                                     width = 3,
                                     
                                     plotlyOutput('plot_ru_revenues_components_perc', height = '400px')
                                 )
                                    )
                                    
                             ),
                    
                    tabPanel("Margins", 
                            
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 12,
                                     
                                     br(),
                                     
                                     plotlyOutput('plot_ru_margin_total', height = '350px')
                                        
                                       ),
                                 
                             ), 
                             
                             hr(),
        
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 6,
                                     
                                     plotlyOutput('plot_ru_margin_components', height = '400px')
                                        
                                       ),
                                 
                                 column(
                                     
                                     width = 3,
                                     
                                     plotlyOutput('plot_ru_margin_components_perc_1', height = '400px')
                                 ),
                                 
                                 column(
                                     
                                     width = 3,
                                     
                                     plotlyOutput('plot_ru_margin_components_perc_2', height = '400px')
                                 )
                                    )
                                    
                             ),
                    
                    
                        tabPanel("Results", 
                            
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 12,
                                     
                                     br(),
                                     
                                     reactableOutput('table_ru_summary')
                                        
                                       )
                                 
                                 )
                    
                            ),
                    
                        tabPanel("Unit Results", 
                            
                             fluidRow(
                                 
                                 column(
                                     
                                     width = 12,
                                     
                                     br(),
                                     
                                     reactableOutput('table_ru_summary_unit')
                                        
                                       )
                                 
                                 )
                    
                            ),
                    
                        tabPanel('Unit Graphs',
                                 
                                 fluidRow(
                                   
                                   column(
                                     
                                     width = 4,
                                     
                                     br(),
                                     
                                     plotlyOutput('plot_ru_unit_costs')
                                     
                                   ),
                                   
                                   column(
                                     
                                     width = 4,
                                     
                                     br(),
                                     
                                     plotlyOutput('plot_ru_unit_revenues')
                                     
                                   ),
                                   
                                   column(
                                     
                                     width = 4,
                                     
                                     br(),
                                     
                                     plotlyOutput('plot_ru_unit_margins')
                                     
                                   )
                                 ),
                                 
                                 fluidRow(
                                   
                                   column(
                                     
                                     width = 4,
                                     
                                     plotlyOutput('plot_ru_unit_costs_period')
                                   
                                   ),
                                   
                                   column(
                                     
                                     width = 4,
                                     
                                     plotlyOutput('plot_ru_unit_revenues_period')
                                     
                                   ),
                                   
                                   column(
                                     
                                     width = 4,
                                     
                                     plotlyOutput('plot_ru_unit_margins_period')
                                     
                                   )
                                   
                                )
                                
                            )
                      

            )
        )        
        ))    

    

    
 
 
    

 
           


# : ---------








# B. PROCESS ========================================


main_panel_process <- 
    
  # Main Panel  --------------------------------

    mainPanel(
        
      ## Title Main Panel              
        
        h2('Sample Title for Process'),
        h4('Here we will display some summary statistics about the resulting data uploaded.'),
        
        br(),

         fluidRow(
             
            column(
                
                width = 11, offset = 0,
                
                img(src = 'slide_appendix.png')

            )
        )        
        )



# C. APPENDIX ========================================


main_panel_apx <- 
    
  # Main Panel  --------------------------------

    mainPanel(
        
      ## Title Main Panel              
        
        h2('Sample Title for Appendix'),
        h4('Here we will display some summary statistics about the resulting data uploaded.'),
        
        br(),

         fluidRow(
             
            column(
                
                width = 11, offset = 0,
                
                img(src = 'slide_appendix.png')

            )
        )        
        )



