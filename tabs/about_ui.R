tabPanel("About",
         titlePanel("Information on Data Sources and Details for Use"),
         
         h5("Thank you for using the Meningioma Visualization Tool."),
         
         ## horizonal line
         hr(),
         
         h3("Data Sources"),
         
         "This table describes the data available the datasets included in the Meningioma Visualization Tool.",
         
         ## insert table
         fluidRow(tags$iframe(style="height:450px; width:98%; scrolling=yes", 
                              src="tableone.html")
                  ),
         
         ## horizontal line
         hr(),
         
         "The table below contains links to the GEO Accession page for each dataset.",
         
         ## insert table
         fluidRow(tags$iframe(style="height:400px; width:50%; scrolling=yes", 
                              src="references.html")
         ),
         
         ## horizontal line
         hr(),
         
         h3("Directions for Use"),
         
         "All analyses are built to react to user selection and are dynamically generated based on user selection. In the analyses tabs, make selections for variables of interest in order from the top down. Start by selecting a gene or genes of interest. The application will find all of the clinical variables which exist in datasets which contain the gene(s) of interest. Next, select a dataset; the application will only display datasets which have the selected clinical variable. Users can change the selected gene or genes of interest at anytime; the available clincal variables and datasets will be recalculated for selection.",
         
         ## horizontal line
         hr(),
         
         h3("Future Features"),
         
         "Currently there are no datasets included in the Meningioma Visualization tool available for robust survival analysis. This feature will be added once sufficient data becomes available.",
         
         ## horizontal line
         hr(),
         
         h3("A Note on Harmonization and Collective Analysis"),
         
         "Great care was taken to preserve the integrity of each dataset included in the Meningioma Visualization Tool. Variable names and levels of factor variables were altered for some datasets and expression values were log2 transformed during the harmonization stage. Expression values were also centered and scaled for each gene in each experiment to facilitate ease of collective analysis. Because the datasets originated from a variety of experimental designs and were processed on differing platforms, combined analysis is not recommended; as such, the tool does not allow 'pooled' or 'overall' analyses."
         
) # end of tabPanel