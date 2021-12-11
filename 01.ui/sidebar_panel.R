

### SIDEBAR PANELS ###    




# : ---------




# A. UPLOAD DATA ========================================


## REGULAR UPDATE -------------------------------

sidebar_upload_ru <- 

  ### Selectors -------------------------------
     
    sidebarPanel(
        
        tags$style(".well {background-color: #fff;}"),
         
         width = 3,

        h4('Upload Data'),
    
        hr(),

        h6('Type of Analysis'),
        input_upload_rusa,
        
        hr(),
        br(),
        
      ### Load Inputs Files      
          
          input_upload_1,
      

          input_upload_2, 
      

          input_upload_3,
      

          input_upload_4,
      


      hr(),
      
      br(),
      
      export_ru_user,
      

      hr(),
      

      

      
   ### Action Buttons --------------------------
      
      button_upload_1_ru,
   
   br()
   
    )

     




# : ---------







# B. ANALYSIS ================================================================


## REGULAR UPDATE ================================================

sidebar_analysis_ru <- 

  ## Selectors -------------------------------
     
    sidebarPanel(
         
         width = 3,

         h4('Main Analysis'),
         
         hr(),
         

   ### Aggregation Level -----------------------
   
     ### Cateogry for Aggregation   
        
         category_aggregation_ru,
   
         time_granularity_ru,

   
     ### Data Range    
   
         datarange_ru,
   
         hr(),
   
   
        ### Type of View   

         type_view_ru,
   
         hr(),
   

     ### Export File name
   
        export_ru_file,
   
        hr(),

      
      
   ## Action Buttons --------------------------
      

      button_analysis_run_ru,
      
      br(),
      br(),
   

      button_analysis_download_ru,
   
      br(),
      br()
   
   
    )









