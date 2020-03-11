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
                              choices = multi_cv()
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
         aov <- summary(aov(data = dataset,                               # aov, call the dataset
                            formula = as.formula(paste0("`",(gene),"`",   # need the quoted backhashes around gene to read "irregular" column/gene names
                                                        "~",
                                                        clinical_variable # over clinical variables
                            )
                            )  
         )
         )
         anova_pvalue <- data.frame(gene_name = as.character(gene),       # gene name
                                    `p adj` = aov[[1]]$'Pr(>F)'[1]        # extracted p-value
         ) 
         anova_pvalue
      }
      
      ## create reactive variable for user input list of genes
     
       gui <- reactive(lapply(                # input comes as a string
         as.list(                            # need to feed multi_anova a list
            strsplit(input$gene_user_input,  # parse user input
                     split =","              # by comma
            )[[1]]),                         # index into list         
         as.symbol)                          # need the gene name unquoted
      )
      
      ## create reactive variable for clinical variable
      cvui <- reactive({as.symbol(input$multi_grouping)})
      
      ## create reactive variable for dataset
      dui <- reactive({eval(as.symbol(input$multi_dataset))})
      
      ## output results from user input gene list to an object
      multianova_out <-reactive(invisible(                          # prevent lapply from printing
                                lapply(                             # use user list and apply function
                                   gui(),                           # created reactive variable list 
                                   FUN = multi_anova,                          # function is the multi_anova
                                   clinical_variable = input$multi_grouping,   # over selected clincal variable of interest
                                   dataset = dui()[["data"]]        # in selected dataset
                                )
                                )
      )
      
      
      ## bind results into a table to use in the datatable - reactive to user input
      multianova_table <- reactive(
         do.call(rbind,            # lists are fun - do call allows rbind to work 
                 multianova_out()  # bind the gene and anova results
         )
      )
      
      # title for multigene anova
      output$multianova_results_title <- renderText({
         paste0("Results of ANOVA")
      })
      
      # datatable output of all multiple anova results
      output$multianova_results <- renderDataTable({
         validate(
            need(input$gene_user_input != '', message = "Please enter comma seperated genes of interest"),
            need(input$multi_grouping != '', message = FALSE),
            need(input$multi_dataset != '', message = FALSE)
         )
         datatable(multianova_table())
      })
      
      
      # copy and pastable text of genes significant at the selected alpha level
      filtered_results <- reactive(multianova_table()  %>% filter(p.adj<.05)) # edit this to have selectable alpha
      significant_genes <- reactive(as.character(filtered_results()$gene_name))
      
      output$signficant_list_title <- renderText({
         validate(
            need(input$gene_user_input != '', message = FALSE),
            need(input$multi_grouping != '', message = FALSE),
            need(input$multi_dataset != '', message = FALSE)
         )
         paste0("Significantly Differentially Expressed Genes")
      })
      
      output$significant_list <- renderPrint({
         validate(
            need(input$gene_user_input != '', message = FALSE),
            need(input$multi_grouping != '', message = FALSE),
            need(input$multi_dataset != '', message = FALSE)
         )
         cat(paste(paste(significant_genes()), collapse=","))
      })
      
      
      #### heatmap
  
      ## find the available datasets for all selected signifiacnt genes 
      s_datasets_with_all_genes <- reactive({
         s_available_datasets_multi <- character()
         for (i in datasets) {
            if(all(c(strsplit(input$significant_gene_user_input, split =",")[[1]]) %in% colnames(eval(as.symbol(i))[["data"]])
            )
            ) {
               s_available_datasets_multi <- c(s_available_datasets_multi,
                                                i
               )
               
               
            }
         }
         return(s_available_datasets_multi)
      })
      
      
      # find available clinical variables in available datasets
      s_multi_cv <- reactive({
         s_clinical_variables <- character()
         for (i in  s_datasets_with_all_genes()) {
            s_clinical_variables <- sort(unique(
               c(s_clinical_variables,
                 colnames(eval(
                    as.symbol(i))[["clinical_data"]]
                 )
               )
            )
            )
            
         }
         return(s_clinical_variables)
      }) 
      
      ## populate the available clinical variables for selected gene in UI
      observe({
         updateSelectizeInput(session = session,
                              inputId = "significant_multi_grouping",
                              label = NULL,
                              choices = s_multi_cv()
         )
      })
      
      ## find the available datasets for selected gene and clinical data in ANOVA UI
      s_multi_ad <- reactive({
         s_genes_clinical_datasets <- character()
         for (i in s_datasets_with_all_genes()) {
            if(all(c(c(strsplit(input$significant_gene_user_input,split =",")[[1]]),input$significant_multi_grouping) %in% colnames(eval(as.symbol(i))[["data"]]
            ))
            ) {
               s_genes_clinical_datasets <- c(s_genes_clinical_datasets,
                                            i)
               
            }
         }
         return(s_genes_clinical_datasets)
      })
      
      ## populate the available datasets for selected gene and clinical data
      observe({
         updateSelectizeInput(session,
                              inputId = "significant_multi_dataset",
                              label = NULL,
                              choices = s_multi_ad()
         )
      })
      
      ## create reactive variable for clinical variable
      s_cvui <- reactive({as.symbol(input$significant_multi_grouping)})
      
      
      ## create reactive variable for dataset
      s_dui <- reactive({eval(as.symbol(input$significant_multi_dataset))})
      
      
      output$heatmap <- renderPlotly({
         validate(
            need(input$significant_gene_user_input != '', message = FALSE),
            need(input$significant_multi_grouping != '', message = FALSE),
            need(input$significant_multi_dataset != '', message = FALSE)
         )
          heatmaply(x = s_dui()[["data"]] %>% select(as.character(strsplit(input$significant_gene_user_input, split =",")[[1]])),
                    RowSideColors = s_dui()[["data"]] %>% select(input$significant_multi_grouping),
                    xlab = "Gene",
                    main = "Gene Expression Heatmap",
                    key.title = "Expression",
                    showticklabels = c(T,F)
                                                )
      })
