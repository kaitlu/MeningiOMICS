tabPanel(title = "Multigene Analysis",
         
         sidebarLayout(
             
             sidebarPanel(
                 
                 helpText("Multigene Analyis and visualization of RNA expression"),
                 
                 conditionalPanel(condition="input.tabselected==1",   # sidebar panel for when anova tab is selected
                                  
                                  
                                  ## feild for users to paste in selections 
                                  textInput(inputId = "gene_user_input",
                                            label = "Genes",
                                            value = NULL,
                                            placeholder = 'Please paste a comma seperated list of genes of interest'
                                  ),
                                  
                                 
                                  
                                  ## select clinical variable to group by
                                  selectizeInput(inputId = "multi_grouping",
                                                 label = "Select variable to analyze",
                                                 choices = NULL,
                                                 selected = NULL,
                                                 options = list(
                                                     placeholder = 'Please select an option below',
                                                     onInitialize = I('function() { this.setValue(""); }')
                                                 )
                                  ),
                                  
                                  ## select dataset to analyze
                                  selectizeInput("multi_dataset",
                                                 label = "Select a dataset", 
                                                 choices = NULL,
                                                 selected = NULL,
                                                 options = list(
                                                     placeholder = 'Please select an option below',
                                                     onInitialize = I('function() { this.setValue(""); }')
                                                 )
                                  )
                                 
                 )
                 
                 # (inputId = "gene_user_input",
                 #                label = "Gene",
                 #                choices = gene, #needs to be updated with a reactive
                 #                selected = NULL,
                 #                multiple = TRUE,
                 #                options = list(
                 #                    placeholder = 'Please paste a comma seperated list of genes of interest',
                 #                    delimiter = ',',
                 #                    onInitialize = I('function(input, callback) {
                 #                                        return {value: input,
                 #                                                text: input
                 #                                                };}')
                 #                )
                 # ),
                 
             ),
                 
    mainPanel(
        
        ## tabs to display
        tabsetPanel(type = "tabs",
                    
                    tabPanel(title = "Results of Multigene Anova",    ## create tab for multiple anovas table out put
                             value = 1,                               ## multi anova tab value = 3
                             dataTableOutput("multianova_results")     ## output results of multiple
                    ),
                    
                    tabPanel(title = "Heatmap and Hierarchial Clustering of Gene Expression by Clinical Variables",
                             value = 2
                    ),
                    
                    id = "tabselected" 
                    )
    ),
             
              
            position = "left",
            fluid = TRUE
)
)
