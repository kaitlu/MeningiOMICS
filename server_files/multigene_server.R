      ## find the available datasets for all selected genes 
      datasets_with_all_genes <- reactive({
         req(input$gene_user_input)
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
         req(input$gene_user_input)
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
         req(input$gene_user_input)
         updateSelectizeInput(session = session,
                              inputId = "multi_grouping",
                              label = NULL,
                              choices = multi_cv()
         )
      })
      
      ## find the available datasets for selected gene and clinical data in ANOVA UI
      multi_ad <- reactive({
         req(input$gene_user_input)
         req(input$multi_grouping)
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
         req(input$gene_user_input)
         req(input$multi_grouping)
         updateSelectizeInput(session,
                              inputId = "multi_dataset",
                              label = NULL,
                              choices = multi_ad()
         )
      })
      
      
      ## function for anova over multiple genes
      multi_anova <- function(gene, clinical_variable, dataset) {
         aov <- summary(aov(data = dataset,                                 # aov, call the dataset
                            formula = as.formula(paste0("`",(gene),"`",     # need the quoted backhashes around gene to read "irregular" column/gene names
                                                        "~",
                                                        clinical_variable)) # over clinical variables
                        ))
         anova_pvalue <- data.frame(gene_name = as.character(gene),         # gene name
                                    `pvalue` = aov[[1]]$'Pr(>F)'[1]          # extracted p-value
         ) 
         anova_pvalue
      }
      
      ## create reactive variable for user input list of genes
     
       gui <- reactive({
          req(input$gene_user_input)
          lapply(                # input comes as a string
         as.list(                            # need to feed multi_anova a list
            strsplit(input$gene_user_input,  # parse user input
                     split =","              # by comma
            )[[1]]),                         # index into list         
         as.symbol)                          # need the gene name unquoted
          })
      
      ## create reactive variable for clinical variable
      cvui <- reactive({
         req(input$multi_grouping)
         as.symbol(input$multi_grouping)
         })
      
      ## create reactive variable for dataset
      dui <- reactive({
         req(input$gene_user_input)
         req(input$multi_grouping)
         eval(as.symbol(input$multi_dataset))})
      
      ## create reactive variable for dataset
      sig <- reactive({input$sig_level})
      
      ## output results from user input gene list to an object
      multianova_out <-reactive({
         req(input$gene_user_input)
         req(input$multi_grouping)
         req(input$multi_dataset)
                                invisible(                          # prevent lapply from printing
                                lapply(                             # use user list and apply function
                                   gui(),                           # created reactive variable list 
                                   FUN = multi_anova,                          # function is the multi_anova
                                   clinical_variable = input$multi_grouping,   # over selected clincal variable of interest
                                   dataset = dui()[["data"]]        # in selected dataset
                                )
                                )
      })
      
      
      ## bind results into a table to use in the datatable - reactive to user input
      multianova_table <- reactive({
         req(input$gene_user_input)
         req(input$multi_grouping)
         req(input$multi_dataset)
         do.call(rbind,            # lists are fun - do call allows rbind to work 
                 multianova_out()  # bind the gene and anova results
         )
      })
      
      # title for multigene anova
      output$multianova_results_title <- renderText({validate(
         need(input$gene_user_input != '', message = FALSE),
         need(input$multi_grouping != '', message = FALSE),
         need(input$multi_dataset != '', message = FALSE)
      )
         paste0("Results of ANOVA")
      })
      
      # datatable output of all multianova results
      output$multianova_results <- renderDataTable({
         validate(
            need(input$gene_user_input != '', message = "Please enter comma seperated genes of interest"),
            need(input$multi_grouping != '', message = "Please choose a clinical variable."),
            need(input$multi_dataset != '', message = "Please choose a dataset")
         )
         datatable(multianova_table())
      })
      
      # downloadable output of all multianova results
      output$downloadMultigeneAnovaResults <- downloadHandler(
         filename = function() {
            paste(input$multi_grouping, "_", input$multi_dataset, "_multigeneANOVAResults.csv", sep = "")
         },
         content = function(file) {
            write.csv(multianova_table(), file, row.names = FALSE)
         }
      )
      
      ## downloadable dataset for the genes in the multigene anova dataset, plus clinical
      multigeneANOVA_dataset <- reactive({
         req(input$gene_user_input)
         req(input$multi_grouping)
         req(input$multi_dataset)
         
         dui()[["data"]] %>% select(as.character(strsplit(input$gene_user_input, split =",")[[1]]),
                                    input$multi_grouping)
      })
      
      output$downloadMultigeneAnovaDataset <- downloadHandler(
         filename = function() {
            paste(input$multi_grouping, "_", input$multi_dataset,  "_multigeneANOVADataset.csv", sep = "")
         },
         content = function(file) {
            write.csv(multigeneANOVA_dataset(), file, row.names = FALSE)
         }
      )
      
      
      # copy and pastable text of genes significant at the selected alpha level
      filtered_results <- reactive({
         req(input$gene_user_input)
         req(input$multi_grouping)
         req(input$multi_dataset)
         multianova_table()  %>% filter(pvalue<sig())
         }) 
      
      significant_genes <- reactive({
         req(input$gene_user_input)
         req(input$multi_grouping)
         req(input$multi_dataset)
         as.character(filtered_results()$gene_name)})
      
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
      heatmap <- reactive({ 
         
         heatmaply(x = dui()[["data"]] %>% select(as.character(strsplit(input$significant_gene_user_input, split =",")[[1]])),
                                      RowSideColors = dui()[["data"]] %>% select(input$multi_grouping),
                                      xlab = "Gene",
                                      main = "Gene Expression Heatmap",
                                      key.title = "Expression",
                                      showticklabels = c(T,F),
                                      plot_method = "plotly",
                                      side_color_colorbar_len = .3,
                                      colorbar_len = .4,
                                      colorbar_xpos = 1.02,
                                      colorbar_ypos = 0,
                                      row_side_palette = Spectral
                                      )
         })
      
      output$heatmap <- renderPlotly({
         validate(
            need(input$significant_gene_user_input != '',  message = "Please enter comma seperated genes of interest"),
            need(input$multi_grouping != '', message =   "Please choose a clinical variable."),
            need(input$multi_dataset != '', message = "Please choose a dataset")
            )
          heatmap()
                                                
      })
      
      ## downloadable dataset for the genes in the heatmap dataset, plus clinical
      heatmap_dataset <- reactive({
         req(input$significant_gene_user_input)
         req(input$multi_grouping)
         req(input$multi_dataset)
         
         dui()[["data"]] %>% select(as.character(strsplit(input$significant_gene_user_input, split =",")[[1]]),
                                                 input$multi_grouping)
      })
      
      output$downloadHeatmapDataset <- downloadHandler(
         filename = function() {
            paste(input$multi_grouping, "_", input$multi_dataset,  "_", "HeatmapDataset.csv", sep = "")
         },
         content = function(file) {
            write.csv(heatmap_dataset(), file, row.names = FALSE)
         }
      )
