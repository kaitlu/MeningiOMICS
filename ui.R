shinyUI(
    navbarPage(title = strong("Meningioma Visualization Tool"), 
               windowTitle = "Meningioma Visualization and Analysis Hub", 
               fluid = TRUE, 
               theme = shinytheme("flatly"),
               id = "navbar",
               
               ## referencing seperate .R files for each panel of UI for cleanliness of code
               
               source("tabs/landing.R", local = TRUE)$value,     
               source("tabs/singlegene.R", local = TRUE)$value,
               source("tabs/multigene.R", local = TRUE)$value,
               source("tabs/upload.R", local = TRUE)$value,
               source("tabs/about.R", local = TRUE)$value
               )
)