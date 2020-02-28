server <- function(input, output, session) {
              
         source("server_files/singlegene_server.R", local = TRUE)$value    
         source("server_files/multigene_server.R", local = TRUE)$value
    
 }