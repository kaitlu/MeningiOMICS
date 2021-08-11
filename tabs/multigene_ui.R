tabPanel(title = "Multigene Analysis",
         
         sidebarLayout(
             
             sidebarPanel(
                 
                 helpText("Multigene Analysis and Visualization"),
                 
                 ## select expression type
                 selectizeInput(inputId =  "omics_multi",
                                label = "Select an Omics Data Type",
                                choices = NULL,
                                options = list(
                                    placeholder = 'Please select an option below',
                                    onInitialize = I('function() {this.setValue(""); }')
                                )
                 ),
                 
                 conditionalPanel(condition="input.panelselected==1",   # sidebar panel for when anova tab is selected
                                  
                                  
                                  ## feild for users to paste in selections 
                                  textInput(inputId = "gene_user_input",
                                            label = "Genes of interest",
                                            value = NULL,
                                            placeholder = 'Comma, tab, bar, or semicolon separated list of genes of interest'
                                  )
                                  
                                  
                 ), 
                 
                 conditionalPanel(condition="input.panelselected==2",   # sidebar panel for when heatmap tab is selected
                                  
                                  ## feild for users to paste in selections 
                                  textInput(inputId = "significant_gene_user_input",
                                            label = "Paste significant genes from Multigene ANOVA here",
                                            value = NULL,
                                            placeholder = 'Comma, tab, bar, or semicolon separated list of genes of interest'
                                  )
                                 
                 ),
                                  
             
                 ## select clinical variable to group by
                 selectizeInput(inputId = "multi_grouping",
                                label = "Select clinical variable to analyze",
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
                 ),
                 
                 conditionalPanel(condition="input.panelselected==1",   # conditional panel for when anova tab is selected
                                  
                                  ## select significance level of interest 
                                  selectizeInput("sig_level",
                                                 label = "Choose an alpha value (level of significance)", 
                                                 choices = c(.01, 
                                                             .05, 
                                                             .1
                                                             ),
                                                 selected = .05
                                                 )#,
                                  
                 ),
                 
                 conditionalPanel(condition="input.panelselected==1",   # sidebar panel for when multigeneAVOVA tab is selected
                                  
                                  ## download Heatmap dataset button
                                  downloadButton('downloadMultigeneAnovaDataset', "Download Selected Data")
                 ),
                 
                 
                 conditionalPanel(condition="input.panelselected==2",   # sidebar panel for when heatmap tab is selected
                                  
                                  ## download Heatmap dataset button
                                  downloadButton('downloadHeatmapDataset', "Download Selected Data")
                 )
                 
             ),
                 
    mainPanel(
        
        ## tabs to display
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Multigene ANOVA",            ## create tab for multiple anovas table out put
                             value = 1,                                       ## multi anova tab value = 3
                             
                             fluidRow(
                                 h3(textOutput("multianova_results_title")),
                                 DT::dataTableOutput("multianova_results",    ## output results of multiple
                                                     height = "31em")         ## hack for space for valid message - return sometime
                                 ),                                               
                             
                             ## horizonal line
                             hr(),
                             
                             fluidRow(
                                 ## put the following on the same row
                                 downloadButton("downloadMultigeneAnovaResults", "Download Multigene Anova Results Table")
                                 ),
                             
                             ## horizonal line
                             hr(),
                             
                             ## put the following on the same row
                             fluidRow(
                                 
                                 ## create space for summary table
                                 column(12,
                                        h3("Significantly Differentially Expressed/Methylated Genes"),
                                        "Copy Genes Below For Use in the Heatmap Tab",
                                        pre(textOutput("significant_list")),
                                        
                                        ## button to copy to clipboard
                                        rclipboardSetup(),
                                        uiOutput("clipboard")
                                 )
                             )
                    ),
                    
                    tabPanel(title = "Heatmap and Hierarchical Clustering",
                             value = 2,
                             plotlyOutput("heatmap"),
                             
                             
                    ),
                    
                    id = "panelselected" 
                    )
    ),
             
              
            position = "left",
            fluid = TRUE
)
)
