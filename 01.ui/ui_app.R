

### UI WRAPPER ###     





# : ============================================================================================================================




ui_app <- 
    
# HEADER ================================================================================

  navbarPage(
       
       title = 'Group Sales Matrix',
       windowTitle = 'PPC - Group Sales Matrix',
       fluid = TRUE,
       theme = default_th,
       useWaitress(color = '#3EB595'),
       
          
       
    ## Intro Panel -------------------------------------------------------
    
            tabPanel(
                
              title = 'Instructions', 
                     
                fluidRow(
                    
                    column(
                        
                        width = 11, offset = 1,
                        
                        main_panel_intro)
                        
                        )
                ),
       
     
# REGULAR UPDATE ====================================================================  
        
        navbarMenu(
            
            title = 'Analysis',
            

                             
        ## Upload Tab ---------------------------------
        
           tabPanel(
                
              title = 'Data Upload', 
              
                sidebarLayout(
      
                    fluid = FALSE,
                    
                    sidebarPanel = sidebar_upload_ru,
                    
                    mainPanel = main_panel_upload_ru)
                     
                ),
        
        
                      "----",
      
          
        ## Analysis Tab ---------------------------
        
           tabPanel(
                
              title = 'Main Analysis', 
              
              useWaiter(),
              
                sidebarLayout(
      
                    fluid = FALSE,
                    
                    sidebarPanel = sidebar_analysis_ru,
                    
                    mainPanel = main_panel_analysis_ru)
                     

                )
        
),
  
    
    
        
    ## Process Tab -------------------------------------------------------
    
            tabPanel(
                
              title = 'Process', 
                     
                fluidRow(
                    
                    column(
                        
                        width = 11, offset = 1,
                        
                        main_panel_process)
                        
                        )
              
                ),

    ## Appendix Panel -------------------------------------------------------
    
            tabPanel(
                
              title = 'Appendix', 
                     
                fluidRow(
                    
                    column(
                        
                        width = 11, offset = 1,
                        
                        main_panel_apx)
                        
                        )
              
                ),

    tags$head(tags$style(HTML('.navbar-brand {width: 200px; font-weight:500; text-align:center;}')))               

    
)

