      ## find the available datasets for all selected genes 
      datasets_with_all_genes <- reactive({
         available_datasets_multi <- character()
         for (i in datasets) {
            if(all(c(strsplit(input$gene_user_input, split =",")[[1]]) %in% colnames(eval(as.symbol(i))[["data"]])
            )
            ) {
               available_datasets_multi <- c(available_datasets_multi,
                                             i
               )
               
               
            }
         }
         return(available_datasets_multi)
      })
      
      
      # find available clinical variables in available datasets
      multi_cv <- reactive({
         clinical_variables <- character()
         for (i in  datasets_with_all_genes()) {
            clinical_variables <- sort(unique(
               c(clinical_variables,
                 colnames(eval(
                    as.symbol(i))[["clinical_data"]]
                 )
               )
            )
            )
            
         }
         return(clinical_variables)
      }) 
      
      ## populate the available clinical variables for selected gene in UI
      observe({
         updateSelectizeInput(session = session,
                              inputId = "multi_grouping",
                              label = NULL,
                              choices = multi_cv(),
                              server = TRUE
         )
      })
      
      ## find the available datasets for selected gene and clinical data in ANOVA UI
      multi_ad <- reactive({
         genes_clinical_datasets <- character()
         for (i in datasets_with_all_genes()) {
            if(all(c(c(strsplit(input$gene_user_input,split =",")[[1]]),input$multi_grouping) %in% colnames(eval(as.symbol(i))[["data"]]
            ))
            ) {
               genes_clinical_datasets <- c(genes_clinical_datasets,
                                            i)
               
            }
         }
         return( genes_clinical_datasets)
      })
      
      ## populate the available datasets for selected gene and clinical data
      observe({
         updateSelectizeInput(session,
                              inputId = "multi_dataset",
                              label = NULL,
                              choices = multi_ad()
         )
      })
      
      
      ## function for anova over multiple genes
      multi_anova <- function(gene, clinical_variable, dataset) {
         aov <- summary(aov(data = dataset,                           # aov, call the dataset
                            # need the quoted backhashes around gene to read "irregular" column/gene names
                            formula = as.formula(paste0("`",(gene),"`",  
                                                        "~",
                                                        clinical_variable # over clinical variables
                            )
                            )  
         )
         )
         anova_pvalue <- data.frame(gene_name = as.character(gene), # gene name
                                    `p adj` = aov[[1]]$'Pr(>F)'[1]  # extracted p-value
         ) 
         anova_pvalue
      }
      
      ## create reactive variable for user input list of genes
      gui <- reactive(lapply(                                      # input comes as a string
         as.list(                             # need to feed multi_anova a list
            strsplit(input$gene_user_input,  # parse user input
                     split =","              # by comma
            )[[1]]),                # index into list because EVERYTHING NEEDS TO BE INDEXED INTO         
         as.symbol)                           # need the gene name unquoted
      )
      
      ## output results from user input gene list to an object
      multianova_out <-reactive(invisible                          # prevent lapply from printing
                                (lapply(                           # use user list and apply function
                                   gui(),                         # created reactive variable list 
                                   FUN = multi_anova,             # function is the multi_anova
                                   clinical_variable = "grade",   # over selected clincal variable of interest
                                   dataset = Schmidt_M[["data"]]  # in selected dataset
                                )
                                )
      )
      
      
      ## bind results into a table to use in the datatable - reactive to user input
      multianova_table <- reactive(
         do.call(rbind,            # lists are fun - do call allows rbind to work 
                 multianova_out()  # bind the gene and anova results
         )
      )
      
      # datatable out put
      output$multianova_results <- renderDataTable({
         validate(
            need(input$gene_user_input != '', message = "Please enter comma seperated genes of interest"),
            need(input$multi_grouping != '', message = FALSE),
            need(input$multi_dataset != '', message = FALSE)
         )
         datatable(multianova_table())
      })
      
       