tabPanel(title = "Single Gene Analysis",
        
    sidebarLayout(
        
        sidebarPanel(
            
            helpText("Analysis and visualization of RNA expression"),
            
            ## select gene to group by
            selectizeInput(inputId = "gene",
                           label = "Select a Gene",
                           choices = NULL,
                           options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')
                           )
            ),
            
            conditionalPanel(condition="input.tabselected==1",   # sidebar panel for when anova tab is selected
                             
                             ## select variable to group by
                             selectizeInput(inputId = "grouping",
                                            label = "Select variable to analyze",
                                            choices = NULL,
                                            options = list(
                                                placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                            )
                             ),
                             
                             ## select dataset to analyze
                             selectizeInput("dataset",
                                            label = "Select a dataset", 
                                            choices = NULL,
                                            options = list(
                                                placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                            )
                             ),
                             
                             ## download button for correlation data
                             downloadButton("downloadGenePhenoDataset", "Download Selected Data"),
                             
            ),
            
            conditionalPanel(condition="input.tabselected==2",   # sidebar panel for when correlation tab is selected
                             
                             ## select a gene for pairwise correlation
                             selectizeInput(inputId = "gene2",
                                            label = "Gene",
                                            choices = NULL,
                                            options = list(
                                                placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                            )
                             ),
                             
                             ## select dataset to analyze
                             selectizeInput(inputId = "dataset_correlation",
                                            label = "Select a dataset", 
                                            choices = NULL,
                                            options = list(
                                                placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                            )
                             ),
                             
                             ## download button for correlation data
                             downloadButton("downloadPairwiseCorrelationDataset", "Download Selected Data"),
                             
            )
        ),
        
        ## main panel for display
        mainPanel(
            
            ## tabs to display
            tabsetPanel(type = "tabs",
                        
                        ## Single Gene - Phenotype Tab
                        tabPanel(title = "Gene-Phenotype Analysis",           
                                 value = 1,
                                 plotlyOutput("graph"), 
                                 
                                 ## horizonal line
                                 hr(),
                                 
                                 ## put the following on the same row
                                 fluidRow(
                                     
                                     ## create space for summary table
                                     column(5,
                                            h5(textOutput("summary_title")),
                                            tableOutput("summary")
                                            
                                     ),
                                     
                                     ## create space for anova results
                                     column(4,
                                            h5(textOutput("center_title")),
                                            tableOutput("anova")
                                            
                                     ),
                                     
                                     # create space for tukey results
                                     column(3,
                                            h5(textOutput("tukey_title")),
                                            tableOutput("tukey")
                                            
                                     )
                                 ),
                                 
                                 ## download gene-pheno type results button
                                 downloadButton('downloadGenePhenoResults', 'Download These Results'),
                                 
                                 ## horizontal line
                                 hr(),
                                 
                                 ## footnote for ANOVA v Welch per homogeneity of variance
                                 fluidRow(h6(textOutput("anova_footnote")))
                                 
                            ),
                        
                        ## Pairwise Correlation tab
                        tabPanel(title = "Pairwise Gene Expression Correlation", 
                                 value = 2,
                                 plotlyOutput("correlation_plot"),
                                 
                                 ## horizonal line
                                 hr(),
                                 
                                 fluidRow(
                                     column(4,
                                         "Spearman's Correlation Coefficient",
                                         tableOutput("correlation_table")
                                     )
                                 ),
                                 
                                 ## download button for correlation data
                                 downloadButton("downloadPairwiseCorrelationResult", "Download This Table"),
                                 
                        ),
                        
                        id = "tabselected"                   ## id for these tabpanel values
            )
            
        ),
        position = "left",
        fluid = TRUE
    )
) 
