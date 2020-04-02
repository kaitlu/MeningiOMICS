tabPanel(title = "Home",
         
         titlePanel("Welcome to the Meningioma Visualization Tool"),
        
         tags$iframe(src = 'home_ui.html', # put .html in /www
                     width = '100%', height = '800px',
                     frameborder = 0, scrolling = 'auto'
         )
)


