## user interface
fluidPage(
    titlePanel("Meningioma Visualization Tool"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Analyis and visualization of RNA expression over clinical outcomes"),
    
            ## select gene to group by
            selectInput("gene",
                        label = "Select a gene",
                        choices = gene,
                        selected = "PTTG1"
            ),
              
             ## select variable to group by
              selectInput("grouping",
                          label = "Select variable to analyze",
                          choices = variables,        # list of variables created in global.R
                          selected = "grade"
                          ),
            
            ## select dataset to analyze
            selectInput("dataset",
                        label = "Select a dataset", 
                        choices = datasets,         # list of datasets created in global.R
                        selected = "Schmidt_M"
            ),
    ),
        
    mainPanel( 
        
    ## create space for graph
    plotOutput("graph"),
    
    ## horizonal line
    hr(),
        
    ## out the following on the same row
    fluidRow(
    
        ## create space for table
        column(4,
               "Summary of Expression",
                tableOutput("summary")
                
               ),
    
        ## create space for anova
        column(4,
               "Anova",
                   tableOutput("anova")
               
               ),
    
        # create space for tukey
        column(4,
               "Tukey HSD",
                   tableOutput("tukey")
                
                )
        )
    )
)
)
