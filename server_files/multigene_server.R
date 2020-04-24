      # make a reactive for the gene user input
      multigene_anova_genes <- reactive({
         gene_list <- toupper(input$gene_user_input)
         gene_list2 <- str_replace_all(gene_list,  pattern = " ", "")
         str_replace_all(gene_list2, pattern = "[|;\t]", ",")
         })


      ## find the available datasets for all selected genes 
      datasets_with_all_genes <- reactive({
         req(input$gene_user_input)
         available_datasets_multi <- character()
         for (i in datasets) {
            if(all(c(strsplit(multigene_anova_genes(), split =",")[[1]]) %in% colnames(eval(as.symbol(i))[["data"]])
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
                 names(dplyr::select_if(eval(as.symbol(i))[["clinical_data"]], not_numeric))
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
            if(all(c(c(strsplit(multigene_anova_genes(), split =",")[[1]]),
                     input$multi_grouping) %in% colnames(eval(as.symbol(i))[["data"]]
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
         as.list(                             # need to feed multi_anova a list
            strsplit(multigene_anova_genes(), # parse user input
                     split =","               # by comma
            )[[1]]),                          # index into list         
         as.symbol)                           # need the gene name unquoted
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
         
         ### test if more than one level for comparison
         if (length(unique(dui()[["data"]] %>% pull(cvui())) %>% na.omit) > 1) {
            
            multiANOVA <- do.call(rbind,            # lists are fun - do call allows rbind to work 
                               multianova_out()  # bind the gene and anova results
            )
            multiANOVA$pvalue <- format(round(x =multiANOVA$pvalue, digits = 2), nsmall = 2)
            multiANOVA
         
         } else {
         
         ### for fewer than 2 levels of comparison
         warning <- data.frame(paste0("The selected dataset, ",input$multi_dataset,", does not have multiple levels of the selected clinical variable, ",input$multi_grouping,", which is required for ANOVA analysis of gene expression."))
         names(warning) <- c("Warning")
         rownames(warning) <- c("**")
         warning
         
         }
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
         
         dui()[["data"]] %>% select(as.character(strsplit(multigene_anova_genes(), split =",")[[1]]),
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
      significant_genes <- reactive({
         req(input$gene_user_input)
         req(input$multi_grouping)
         req(input$multi_dataset)
         
         ## check for mutliple levels of clinical variable
         if (length(unique(dui()[["data"]] %>% pull(cvui())) %>% na.omit) > 1) {
         
         filtered_results <- multianova_table()  %>% dplyr::filter(pvalue<sig())
         as.character(filtered_results$gene_name)
         } else {}
         }) 
      
      
      output$significant_list <- renderPrint({
         validate(
            need(input$gene_user_input != '', message = FALSE),
            need(input$multi_grouping != '', message = FALSE),
            need(input$multi_dataset != '', message = FALSE)
         )
         cat(paste(paste(significant_genes()), collapse=","))
      })
      
      output$clipboard <- renderUI({
         validate(
            need(input$gene_user_input != '', message = FALSE),
            need(input$multi_grouping != '', message = FALSE),
            need(input$multi_dataset != '', message = FALSE)
         )
         rclipButton("copybtm",
                     "Copy to Clipboard",
                     paste(paste(significant_genes()), collapse=","),
                     icon("clipboard")
         )
      })
      
      output$significant_list <- renderPrint({
         validate(
            need(input$gene_user_input != '', message = FALSE),
            need(input$multi_grouping != '', message = FALSE),
            need(input$multi_dataset != '', message = FALSE)
         )
         cat(paste(paste(significant_genes()), collapse=","))
      })
      
      
      # make a reactive for the gene user input
      multigene_heatmap_genes <- reactive({
         h_gene_list <- toupper(input$significant_gene_user_input)
         h_gene_list2 <- str_replace_all(h_gene_list,  pattern = " ", "")
         str_replace_all(h_gene_list2, pattern = "[|;\t]", ",")
      })
      
      
      #### heatmap
      heatmap <- reactive({ 
         
         heatmaply(x = dui()[["data"]] %>% select(as.character(strsplit(multigene_heatmap_genes(), split =",")[[1]])),
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
         
         dui()[["data"]] %>% select(as.character(strsplit(multigene_heatmap_genes(), split =",")[[1]]),
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
