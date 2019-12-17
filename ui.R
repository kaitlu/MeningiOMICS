## user interface
fluidPage(
    titlePanel("Mengingioma Visualization Tool"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Analyis and visualization of RNA expression over clinical outcomes"),
    
              ## select dataset to analyze
              selectInput("dataset",
                          label = "Select a dataset", 
                          choices = datasets,         # list of datasets created in global.R
                          selected = "Schmidt_M"
                          ),
              ## select variable to group by
              selectInput("grouping",
                          label = "Select variable to analyze",
                          choices = variables,        # list of variables created in global.R
                          selected = "grade"
                          ),
              ## select gene to group by
              selectInput("gene",
                          label = "Select a gene",
                          choices = gene,
                          selected = "PTTG1"
                          ),
        
    ),
        
    mainPanel( 
    
    ## create space for table
    tableOutput("summary"),
    
    ## create space for graph
    plotOutput("graph"),
    
    ## create space for anova
    tableOutput("anova"),
    
    # create space for tukey
    tableOutput("tukey")
    )
)
)