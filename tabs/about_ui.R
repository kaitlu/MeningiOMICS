tabPanel("About",
         
         titlePanel("Information on Data Sources and Details for Use"),

         tags$iframe(src = 'about_ui.html', # put .html in /www
                     width = '100%', height = '800px',
                     frameborder = 0, scrolling = 'auto')
         
         
) # end of tabPanel