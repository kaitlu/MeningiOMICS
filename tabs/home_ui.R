tabPanel(title = "Home",
         
         titlePanel("Welcome to the Meningioma Visualization Tool"),
        
         tags$iframe(src = 'about_ui.html', # put myMarkdown.html to /www
                     width = '100%', height = '800px',
                     frameborder = 0, scrolling = 'auto'
         )
)


