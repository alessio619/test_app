

# : ========================================================================================================================================================



## Packages ----------------------------------------------------------------


### Shiny  
suppressMessages(library(shiny))
suppressMessages(library(bslib))
suppressMessages(library(waiter))
suppressMessages(library(shinyWidgets))




### Utilities
suppressMessages(library(data.table))
suppressMessages(library(magrittr))
suppressMessages(library(here))
suppressMessages(library(lubridate))
suppressMessages(library(arrow))
suppressMessages(library(openxlsx))



### Visualization
suppressMessages(library(plotly))
suppressMessages(library(reactable))



### Additional Options

options(shiny.maxRequestSize = 200*1024^2)

cat('Group - SALES MATRIX UPDATE LOADING: Please wait while app is getting ready in your default browser...')




