

# : ========================================================================================================================================================



## Sourcer ----------------------------------------------------------------

# Set up ===============================================================

source(file.path('00.setup', "01.packages.R"))
source(file.path('00.setup', "02.theme.R"))
source(file.path('00.setup', "03.preparation.R"))



# UI ===============================================================
source(file.path('01.ui', "main_panel.R"))
source(file.path('01.ui', "A.sliders.R"))
source(file.path('01.ui', "B.buttons.R"))
source(file.path('01.ui', "sidebar_panel.R"))
source(file.path('01.ui', "ui_app.R"))


# Source SERVER ===============================================================
source(file.path('03.server', "server_app.R"))


   


# Body ===============================================================




