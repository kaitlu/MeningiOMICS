shinyUI(
    navbarPage(title = strong("MeningiOMICS"), 
               windowTitle = "MeningiOMICS: Meningioma Visualization and Analysis Hub", 
               fluid = TRUE, 
               theme = shinytheme("flatly"),
               id = "navbar",
               
               ## referencing seperate .R files for each panel of UI for cleanliness of code
               
               source("tabs/home_ui.R", local = TRUE)$value,     
               source("tabs/singlegene_ui.R", local = TRUE)$value,
               source("tabs/multigene_ui.R", local = TRUE)$value,
               # source("tabs/upload_ui.R", local = TRUE)$value,  # to be built
               source("tabs/about_ui.R", local = TRUE)$value
               )
)