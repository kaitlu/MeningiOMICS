## this file contains all server functions for the single gene analysis tab
    
## anova and plots

    ## server side load of all gene options for input
    updateSelectizeInput(session = session,
                         inputId = "gene",
                         choices = gene,
                         selected = FALSE,
                         server = TRUE)
    
    ## find available clinical variables for selected gene
    cv <- reactive({
      req(input$gene)
              clinical_variables <- character()
              for (i in datasets) {
                if(input$gene %in% colnames(eval(as.symbol(i))[["expression_data"]])) {
                    clinical_variables <- c(clinical_variables,
                                        colnames(eval(as.symbol(i))[["clinical_data"]])
                    )
                 }
              }
              return(sort(unique(clinical_variables)))
    })
    
    ## populate the available clinical variables for selected gene in UI
    observe({
      req(input$gene)  
      updateSelectizeInput(session = session,
                      inputId = "grouping",
                      label = NULL,
                      choices = cv(),
                      selected = FALSE,
                      server = TRUE
                      )
    })
        
    ## find the available datasets for selected gene and clinical data in ANOVA UI
    ad <- reactive({
      req(input$gene)
      req(input$grouping)
        available_datasets <- character()
        for (i in datasets) {
            if(all(c(input$gene, 
                     input$grouping) %in% colnames(eval(as.symbol(i))[["data"]])
                    )
            ) {
                available_datasets <- sort(unique(
                                                  c(available_datasets,
                                                    i
                                                    )
                                                )
                )}
        }
        return(available_datasets)
    })
    
    ## populate the available datasets for selected gene and clinical data
    observe({
      req(input$gene)
      req(input$grouping)
      updateSelectizeInput(session,
                          inputId = "dataset",
                          label = NULL,
                          choices = ad(),
                          selected = FALSE,
                          server = TRUE
        )
    })
    
    
    ## reactive to pull out gene
    gv <- reactive({
      req(input$gene)
      as.symbol(input$gene)
    })
    
    ## reactive to pull out dataset
    datasetInput <- reactive({
      req(input$dataset)  
      eval(as.symbol(input$dataset))
      })

    ## reactive to pull out grouping
    grp <- reactive({
      req(input$grouping)
      as.symbol(input$grouping)
      })


    ## Visualize expression values across selected clinical variables
    output$graph <- renderPlotly({
        validate(
            need(input$gene != '', 'Please choose a gene.'),
            need(input$grouping != '', 'Please choose a clinical variable.'),
            need(input$dataset != '', "Please choose a dataset")
        )

       if (is.double(datasetInput()[["data"]] %>% pull(grp())) == TRUE) {
        
       ## continuous data
       plot_ly(data = datasetInput()[["data"]],
               x = ~eval(grp()),
               y = ~eval(gv()),
               type = "scatter",
               mode = "markers",
               name = "Samples"
               ) %>% 
          add_lines(y = ~fitted(loess(eval(gv()) ~ eval(grp()), na.action = na.exclude)),
                   line = list(color = "#56B4E9"),
                   name = "Local Smoother"
           ) %>%
         
          add_lines(y = ~fitted(lm(eval(gv()) ~ eval(grp()), na.action = na.exclude)),
                   line = list(color = "#E69F00"),
                   name = "Simple Linear Regression"
           ) %>% 
           layout(margin = list(t = 85),
                  title = list(text = paste(input$gene,"RNA Expression by",input$grouping,"in",input$dataset)),
                  xaxis = list(title = input$grouping), 
                  yaxis = list(title = paste(input$gene,"Expression (log2)"))
           )
       
       } else {
        
       ## using plotly
       plot_ly(data = datasetInput()[["data"]],
                x = ~eval(grp()),
                y = ~eval(gv()),
                type = "box",
                boxpoints = "all",
                jitter = .7,
                pointpos = 0,
                boxmean = TRUE,
                line = list(color = "#0072B2")
        ) %>% 
            layout(margin = list(t = 85),
                   title = list(text = paste(input$gene,"RNA Expression by",input$grouping,"in",input$dataset)),
                   xaxis = list(title = input$grouping), 
                   yaxis = list(title = paste(input$gene,"Expression (log2)"))
                   )
       }
          })

    
    ## summary title 
    output$summary_title <- renderText({
      validate(
        need(input$gene != '', message = FALSE),
        need(input$grouping != '', message = FALSE),
        need(input$dataset != '', message = FALSE)
      )
      
      if (is.double(datasetInput()[["data"]] %>% pull(grp())) == TRUE) {
        #### continuous variables
        print(paste0("Summary of ",input$gene," and ",input$grouping))
      } else {
        #### categorical variables
        print(paste0("Summary of ",input$gene," Expression by ",input$grouping))
      }
      
    })
    
    variable_summary <- reactive({
      if (is.double(datasetInput()[["data"]] %>% pull(grp())) == TRUE) {
        
        #### continuous variables
        summ_exp <- datasetInput()[["data"]] %>%
          dplyr::summarize(mean = mean(!!gv()),
                           median = median(!!gv()),
                           sd = sd(!!gv()),
                           total = n()
          )
        
        summ_var <- datasetInput()[["data"]] %>%
          dplyr::summarize(mean = mean(!!grp(), na.rm = TRUE),
                           median = median(!!grp(), na.rm = TRUE),
                           sd = sd(!!grp(), na.rm = TRUE),
                           total = n()
          )
        
        summ_cont <- data.frame(rbind(summ_exp, summ_var))
        variable <- c(input$gene, input$grouping)
        cbind(variable,summ_cont)
        
      } else {    
        
        #### categorical variables
        summ <- datasetInput()[["data"]] %>%
          dplyr::group_by(!!grp()) %>%
          dplyr::summarize(mean = mean(!!gv()),
                           median = median(!!gv()),
                           sd = sd(!!gv()),
                           total = n()
          )
        summ
      }
      
    })
    
    ## provide summary statistics over expression
    output$summary <- renderTable({
        validate(
            need(input$gene != '', message = FALSE),
            need(input$grouping != '', message = FALSE),
            need(input$dataset != '', message = FALSE)
        )
        
      variable_summary()
        
     },                # end table function
     digits = 2,       # sign digits
     rownames = FALSE)  # include row names
    
    
    ## reactive aov object for use in cateegorical calculations
    variance <- reactive({
                          req(input$gene)
                          req(input$grouping)
                          req(input$dataset)
                            aov(data = datasetInput()[["data"]],                   # aov, call the dataset
                                 formula = as.formula(paste0("`",input$gene,"`", # formula to take gene input
                                                             "~",                
                                                             input$grouping
                                 )))
                    })
    
    
    ## center panel (linear fit or aov) title
    output$center_title <- renderText({
      validate(
        need(input$gene != '', message = FALSE),
        need(input$grouping != '', message = FALSE),
        need(input$dataset != '', message = FALSE)
      )
      
      if (is.double(datasetInput()[["data"]] %>% pull(grp())) == TRUE) {
        #### continuous variables
        print("Strength of Association")
      } else {
        #### categorical variables
        print("Analysis of Variance")
      }
      
    })
    
    
    ## center panel (linear fit or aov) results
    centertable <- reactive({
      req(input$gene)
      req(input$grouping)
      req(input$dataset)
      
      if (is.double(datasetInput()[["data"]] %>% pull(grp())) == TRUE) {
        
        #### continuous variables
        ## spearman correlation coefficient
        cont_rho <- cor.test(formula = as.formula(paste0("~",
                                                         "`",input$gene,"`",     # formula to take gene input
                                                         "+",
                                                         "`",input$grouping,"`") # by clinical variable
        ),
        data = datasetInput()[["data"]], 
        method = "spearman")  
        
        rho_cont <- as.data.frame(cbind(cont_rho$estimate[[1]], cont_rho$p.value))
        names(rho_cont) <- c("Estimate", "pvalue")
        row.names(rho_cont) <- c("Spearman's Correlation Coefficient")
        
        rho_cont
        
      } else {
        
        #### categorical variables
        
        ### test if more than one level for comparison
        if (length(unique(datasetInput()[["data"]] %>% pull(grp())) %>% na.omit) > 1) {
        
        ## leveneTest
        homogen <- leveneTest(variance())
        homogen_pvalue <- data.frame(homogen$'Pr(>F)'[1])
        names(homogen_pvalue) <- c("p-value")
        row.names(homogen_pvalue) <- "Homogeneity of Variance"
        
        ## anova results
        anova <- summary(object = variance())
        anova_pvalue <- data.frame(anova[[1]]$'Pr(>F)'[1])
        names(anova_pvalue) <- c("p-value")
        row.names(anova_pvalue) <- "ANOVA Results"
        
        ## welch one-way results
        welch <- oneway.test(formula = as.formula(paste0("`",input$gene,"`", # formula to take gene input
                                                         "~",                
                                                         input$grouping)
        ),
        data = datasetInput()[["data"]]
        
        )
        welch_pvalue <- data.frame(welch$p.value)
        names(welch_pvalue) <- c("p-value")
        row.names(welch_pvalue) <- "Welch's Results*"
        
        ## create table
        rbind(homogen_pvalue, anova_pvalue, welch_pvalue)
        
        } else {
          
        ### for fewer than 2 levels of comparison
        warning <- data.frame(paste0("The selected dataset, ",input$dataset,", does not have multiple levels of the selected clinical variable, ",input$grouping,", which is required for ANOVA and pairwise analysis of ",input$gene," expression."))
        names(warning) <- c("Warning")
        rownames(warning) <- c("**")
        warning
        
        
        }
      }
    })
    
    ## output center panel results
    output$anova <- renderTable({
        validate(
            need(input$gene != '', message = FALSE),
            need(input$grouping != '', message = FALSE),
            need(input$dataset != '', message = FALSE)
        )
        
        centertable()
        
         },           # end table function
    digits = 2,       # sign digits
    rownames = TRUE)  # include row names
    
    
    
    ## tukey title (blank for cont)
    output$tukey_title <- renderText({
      validate(
        need(input$gene != '', message = FALSE),
        need(input$grouping != '', message = FALSE),
        need(input$dataset != '', message = FALSE)
      )
      
      if (is.double(datasetInput()[["data"]] %>% pull(grp())) == TRUE) {
        #### continuous variables
        ## do nothing
        
      } else {
        #### categorical variables
        print("Tukey HSD (Pairwise)")
      }
    })
    
    
    ## tukey (pairwise) results for categorical 
    
    tukey <- reactive({
      if (is.double(datasetInput()[["data"]] %>% pull(grp())) == TRUE) {
        
        #### continuous variables    
        ## do nothing
        
      } else {
        
        #### categorical variables
        
        ### test if more than one level for comparison
        if (length(unique(datasetInput()[["data"]] %>% pull(grp())) %>% na.omit) == 2) {
        
          tukey_warning <- data.frame(paste0("The selected dataset, ",input$dataset,", does not have enough levels of the selected clinical variable, ",input$grouping,", to report pairwise differnce analysis of ",input$gene," expression."))
          names(tukey_warning) <- c("Warning")
          rownames(tukey_warning) <- c("**")
          tukey_warning
          
          
        } else {
          
          ## don't display another warning if the ANOVA warning is present
          if (length(unique(datasetInput()[["data"]] %>% pull(grp())) %>% na.omit) == 1) {} ## do nothing
        
        else { 
          
          ## return tukey results if more than 2 levels
          tukey <- TukeyHSD(variance())
          as.data.frame(tukey[[input$grouping]])[, c(1,4)]
        }
        }
      }
    })
    
    ## output tukey results
    output$tukey <- renderTable({
        validate(
            need(input$gene != '', message = FALSE),
            need(input$grouping != '', message = FALSE),
            need(input$dataset != '', message = FALSE)
        )
    
    tukey()
    
    },
    rownames = TRUE)
    
    ## footnote to accompany categorical results
    output$anova_footnote <- renderText({
      validate(
        need(input$gene != '', message = FALSE),
        need(input$grouping != '', message = FALSE),
        need(input$dataset != '', message = FALSE)
      )
      
      if (is.double(datasetInput()[["data"]] %>% pull(grp())) == TRUE) {
        #### continuous variables
        ## do nothing
      } else {
        #### categorical variables
        print("*An assumption of the ANOVA test is that there is homogeneity of variance in the response variable across groups. Results obtained from an ANOVA are valid when the p-value obtained from the test of homogeneity of variance indicates failure to reject the null (i.e. > .05). Welch's unequal variances t-test, should be used when there is not homogeneity of variance.")
      }
      
    })
    
  
    ### downloadable output of all results, zipped
    output$downloadGenePhenoResults <- downloadHandler(
      filename = function() {
        paste(input$gene, "_", input$grouping, "_", input$dataset, "_", "GenePhenotypeResults.zip", sep = "")
      },
      content = function(fname) {
        tmpdir <- tempdir()
        setwd(tempdir())
        fs <- c("SummaryResults.csv", "BivariableResults.csv", "TukeyResults.csv")
        write.csv(variable_summary(), file = "SummaryResults.csv", row.names = FALSE)
        write.csv(centertable(), file = "BivariableResults.csv", row.names = TRUE)
        write.csv(tukey(), file = "TukeyResults.csv", row.names = TRUE)
        
        zip(zipfile=fname, files=fs)
        if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}},
        contentType = "application/zip"
      
    )
    
    ## downloadable dataset for the content
    GenePheno_dataset <- reactive({
      req(input$gene)
      req(input$grouping)
      req(input$dataset)
      
      datasetInput()[["data"]] %>% select(input$gene, input$grouping)
    })
    
    output$downloadGenePhenoDataset <- downloadHandler(
      filename = function() {
        paste(input$gene,"_", input$grouping, "_", input$dataset ,  "_", "Dataset.csv", sep = "")
      },
      content = function(file) {
        write.csv(GenePheno_dataset(), file, row.names = FALSE)
      }
    )
    
    
## correlation plots
    
    ## find genes available based on initially selected gene
    g2 <- reactive({
      req(input$gene)
            second_gene <- character()
            for (i in datasets) { colnames_gene1_corr <- colnames(eval(as.symbol(i))[["expression_data"]])
                                 if(input$gene %in% colnames_gene1_corr) {
                                       second_gene <- colnames_gene1_corr }
            }
        return(sort(unique(second_gene)))
    })
    
    ## populate the available second genes for initially selected gene in UI
    observe({
      req(input$gene)
        updateSelectizeInput(session,
                             inputId = "gene2",
                             label = NULL,
                             choices = g2(),
                             selected = FALSE,
                             server = TRUE
        )
    })
    
    
    ## find the available datasets for selected pairwise genes in correlation UI
    cd <- reactive({
      req(input$gene)
      req(input$gene2)
        available_datasets_correlation <- character()
        for (i in datasets) {
                            if(all(c(input$gene, input$gene2) %in% colnames(eval(as.symbol(i))[["data"]])
                                    )
                                ) {
                                    available_datasets_correlation <- sort(unique(
                                                                                  c(available_datasets_correlation,
                                                                                    i
                                                                                    )
                                                                                )
                                    )
            }
        }
        return(available_datasets_correlation)
    })
    
    ## populate the available datasets for selected gene pair
    observe({
      req(input$gene)
      req(input$gene2)
        updateSelectizeInput(session,
                             inputId = "dataset_correlation",
                             label = NULL,
                             selected = FALSE,
                             choices = cd()
        )
    })
    
    ## pull out gene2
    gv2 <- reactive({
      req(input$gene2)
      as.symbol(input$gene2)
    })
    
    ## pull out dataset
    dataset_correlationInput <- reactive({
      req(input$dataset_correlation)
        eval(as.symbol(input$dataset_correlation))
    })
    
    ## make the correlation plots
    output$correlation_plot <- renderPlotly({
        validate(
            need(input$gene != '', 'Please choose a gene.'),
            need(input$gene2 != '', 'Please choose a gene to compare.'),
            need(input$dataset_correlation != '', "Please choose a dataset")
        )
                                      
        ## with plotly
        plot_ly(data = dataset_correlationInput()[["data"]],
                x = ~eval(gv()),
                y = ~eval(gv2()),
                type = "scatter",
                mode = "markers",
                name = "Samples"
        )  %>%
            add_lines(y = ~fitted(loess(eval(gv2()) ~ eval(gv()), na.action = na.exclude)),
                      line = list(color = "#56B4E9"),
                      name = "Local Smoother"
            ) %>%
            
            add_lines(y = ~fitted(lm(eval(gv2()) ~ eval(gv()), na.action = na.exclude)),
                      line = list(color = "#E69F00"),
                      name = "Simple Linear Regression"
            ) %>% 
        
            layout(margin = list(t = 85),
                   title =  list(text = paste("RNA Expression of",input$gene,"vs.",input$gene2,"in",input$dataset_correlation)),
                   xaxis =  list(title = paste(input$gene,"Expression (log2)")), 
                   yaxis =  list(title = paste(input$gene2,"Expression (log2)"))
            )
        
        })
    

    ## add correlation coef and association test
    pairwisecorrelation <- reactive({
      req(input$gene)
      req(input$gene2)
      req(input$dataset_correlation)
      cor_test <- cor.test(formula = as.formula(paste0("~",
                                                       "`",input$gene,"`",  # formula to take gene input
                                                       "+",
                                                       "`",input$gene2,"`")
                                                                     ),
                                                data = dataset_correlationInput()[["data"]],
                                                method = "spearman")
      cor_table <- as.data.frame(cbind(cor_test$estimate[[1]], cor_test$p.value))
      names(cor_table) <- c("rho", "p-value")
      cor_table
    })
    
    ## output correlation results
    output$correlation_table <- renderTable({
        validate(
            need(input$gene != '', message = F),
            need(input$gene2 != '', message = F),
            need(input$dataset_correlation != '', message = F)
        )     
      
      pairwisecorrelation()  
      
    })
    
    ## downloadable output of correlation
    output$downloadPairwiseCorrelationResult <- downloadHandler(
      filename = function() {
        paste(input$gene,"_", input$gene2, "_", input$dataset_correlation, "_", "SpearmanCorrelationResults.csv", sep = "")
      },
      content = function(file) {
        write.csv(pairwisecorrelation(), file, row.names = FALSE)
      }
    )
    
    ## downloadable dataset for the content
    correlation_dataset <- reactive({
      req(input$gene)
      req(input$gene2)
      req(input$dataset_correlation)
      
      dataset_correlationInput()[["data"]] %>% select(input$gene, input$gene2)
    })
    
    output$downloadPairwiseCorrelationDataset <- downloadHandler(
      filename = function() {
        paste(input$gene,"_", input$gene2, "_", input$dataset_correlation,  "_", "Dataset.csv", sep = "")
      },
      content = function(file) {
        write.csv(correlation_dataset(), file, row.names = FALSE)
      }
    )
