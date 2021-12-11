

### BUTTONS ###    




# : =============================================================



# A. REGULAR UPDATE =====================================



## Upload -----------------------------

button_upload_1_ru <- 
    
    actionButton(
          
          inputId = 'upload_files_ru',
          label = 'Upload Files',
          icon = icon('upload'),
          width = side_bar_width,
          class = "btn-success")





## Analysis ---------------------------


button_analysis_run_ru <- 
    
    actionButton(
          
          inputId = 'running_bttn_ru',
          label = 'Execute & Visualize',
          icon = icon('running'),
          width = side_bar_width,
          class = "btn-danger")



button_analysis_archive_ru <- 
    
    actionButton(
          
          inputId = 'archive_bttn_ru',
          label = 'Archive',
          icon = icon('download'),
          width = side_bar_width,
          class = "btn-warning")



button_analysis_download_ru <- 
    
    downloadButton(
          
          outputId = 'download_output_ru',
          label = 'Export Results',
          icon = icon('file-export'),
          style = "width:100%;",
          class = "btn-primary")



