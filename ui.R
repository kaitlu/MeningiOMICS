## user interface
fluidPage(
    titlePanel("Meningioma Visualization Tool"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Analyis and visualization of RNA expression over clinical outcomes"),
    
            ## select gene to group by
            selectizeInput("gene",
                        label = "Gene",
                        choices = gene,
                        options = list(
                                       placeholder = 'Please select an option below',
                                       onInitialize = I('function() { this.setValue(""); }')
                        )
            ),
              
            ## select variable to group by
            selectizeInput("grouping",
                          label = "Select variable to analyze",
                          # choices = variables,        # list of variables available for selected gene
                          # selected = "grade"
                          choices = NULL,
                          selected = NULL,
                          options = list(
                              placeholder = 'Please select an option below',
                              onInitialize = I('function() { this.setValue(""); }')
                          )
                          ),
            
            ## select dataset to analyze
            selectizeInput("dataset",
                        label = "Select a dataset", 
                        # choices = datasets,         # list of datasets with both the gene and clinical selection available
                        # selected = "Schmidt_M"
                        choices = NULL,
                        selected = NULL,
                        options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }')
                        )
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
