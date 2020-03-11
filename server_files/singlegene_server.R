## this file contains all server functions for the single gene analysis tab
    
## anova and plots
    
    ## find available clinical variables for selected gene
    cv <- reactive({
              clinical_variables <- character()
              for (i in datasets) {
              if(input$gene %in% colnames(eval(as.symbol(i))[["expression_data"]])) {
                clinical_variables <- 
                                                  c(clinical_variables,
                                                    colnames(eval(as.symbol(i))[["clinical_data"]])
                                      
                                                
                 )}
              }
              return(sort(unique(clinical_variables)))
    })
    
    ## populate the available clinical variables for selected gene in UI
    observe({
        updateSelectizeInput(session = session,
                      inputId = "grouping",
                      label = NULL,
                      choices = cv()
                      )
    })
        
    ## find the available datasets for selected gene and clinical data in ANOVA UI
    ad <- reactive({
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
        updateSelectizeInput(session,
                          inputId = "dataset",
                          label = NULL,
                          choices = ad()
        )
    })
    
    
    ## pull out gene
    gv <- reactive({as.symbol(input$gene)
    })
    
    ## pull out dataset
    datasetInput <- reactive({
        eval(as.symbol(input$dataset))
    })

    ## pull out grouping
    grp <- reactive({as.symbol(input$grouping)
    })


    ## provide summary statistics over gene
    output$summary <- renderTable({
        validate(
            need(input$gene != '', message = FALSE),
            need(input$grouping != '', message = FALSE),
            need(input$dataset != '', message = FALSE)
        )
            summ <- datasetInput()[["data"]] %>%
                        dplyr::group_by(!!grp()) %>%
                        dplyr::summarize(mean = mean(!!gv()),
                                  median = median(!!gv()),
                                  sd = sd(!!gv()),
                                  total = n()
                                )
        summ
    })

    output$graph <- renderPlotly({
        validate(
            need(input$gene != '', 'Please choose a gene.'),
            need(input$grouping != '', 'Please choose a clinical variable.'),
            need(input$dataset != '', "Please choose a dataset")
        )
        ## Visualize expression values across grade
         ggplotly(
             ggplot(data = datasetInput()[["data"]],
                    aes(x = !!grp(),
                        y = !!gv())
                    ) +
             geom_boxplot() +
             geom_jitter(width = .10, 
                        color = "#0072B2") +                                  # accessible blue
             theme_bw() +
             ggtitle(paste(input$gene,"RNA Expression by",input$grouping)) +   # label plot to what clinical variable is shown 
             xlab(input$grouping) +                                            # x axis grouping variable
             ylab(paste(input$gene,"Expression (log2)")) +                     # y axis gene 
             stat_summary(fun.y=mean,                                          # add in a blue dashed line at the mean
                          geom = "errorbar",
                          aes(ymax = ..y.., ymin = ..y..),
                          width = 1.5,
                          linetype = "dashed",
                          color = "#E69F00")
            )
          })

    variance <- reactive({
                          aov(data = datasetInput()[["data"]],               # aov, call the dataset
                                 formula = as.formula(paste0(input$gene,     # formula to take gene input
                                                             "~",            # interpret grade as variable
                                                             input$grouping
                                 )))
                    })

    output$anova <- renderTable({
        validate(
            need(input$gene != '', message = FALSE),
            need(input$grouping != '', message = FALSE),
            need(input$dataset != '', message = FALSE)
        )
                                    anova <- summary(variance())
                                    anova_pvalue <- data.frame(anova[[1]]$'Pr(>F)'[1])
                                    names(anova_pvalue) <- c("p-value")
                                    row.names(anova_pvalue) <- "overall"
                                    anova_pvalue
                                 },
                                digits = -2,
                                rownames = TRUE)

    output$tukey <- renderTable({
        validate(
            need(input$gene != '', message = FALSE),
            need(input$grouping != '', message = FALSE),
            need(input$dataset != '', message = FALSE)
        )
                                    tukey <- TukeyHSD(variance())
                                    as.data.frame(tukey[[input$grouping]])
                                },
                                rownames = TRUE)

## correlation plots
    
    ## find genes available based on initially selected gene
    g2 <- reactive({
            second_gene <- character()
            for (i in datasets) {
                                 if(input$gene %in% colnames(eval(as.symbol(i))[["expression_data"]])) {
                                       second_gene <- sort(unique(
                                                                  c(second_gene,
                                                                    colnames(eval(as.symbol(i))[["expression_data"]])
                                                                    )
                                                                )
                                       )
            }
        }
        return(second_gene)
    })
    
    ## populate the available second genes for initially selected gene in UI
    observe({
        updateSelectizeInput(session,
                             inputId = "gene2",
                             label = NULL,
                             choices = g2()
        )
    })
    
    
    ## find the available datasets for selected pairwise genes in correlation UI
    cd <- reactive({
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
        updateSelectizeInput(session,
                             inputId = "dataset_correlation",
                             label = NULL,
                             choices = cd()
        )
    })
    
    ## pull out gene2
    gv2 <- reactive({as.symbol(input$gene2)
    })
    
    ## pull out dataset
    dataset_correlationInput <- reactive({
        eval(as.symbol(input$dataset_correlation))
    })
    
    ## make the correlation plots
    output$correlation_plot <- renderPlotly({
        validate(
            need(input$gene != '', 'Please choose a gene.'),
            need(input$gene2 != '', 'Please choose a gene to compare.'),
            need(input$dataset_correlation != '', "Please choose a dataset")
        )
                                        ggplotly(
                                             ggplot(data = dataset_correlationInput()[["data"]],
                                                    aes(x = !!gv(),
                                                        y = !!gv2()
                                                        )
                                                     )+
                                              geom_point(color = "#0072B2") + 
                                              geom_smooth(method= loess, se=FALSE, color = "#56B4E9") +          #add a smoother
                                              geom_smooth(method = lm, se = FALSE, color = "#E69F00") +          #add a straight line
                                              theme_bw() +
                                              ggtitle(paste("RNA Expression of",input$gene,"vs.",input$gene2)) + #title pairwise
                                              xlab(paste(input$gene,"Expression (log2)")) +                      #x axis gene
                                              ylab(paste(input$gene2,"Expression (log2)"))                       #y axis gene
                                             )
        })
    

    ## add correlation coef and association test
    output$correlation_table <- renderTable({
        validate(
            need(input$gene != '', message = F),
            need(input$gene2 != '', message = F),
            need(input$dataset_correlation != '', message = F)
        )     
        cor_test <- cor.test(formula = as.formula(paste0("~",
                                                          input$gene,                   # formula to take gene input
                                                          "+",
                                                          input$gene2)
                                                   ),
                              data = dataset_correlationInput()[["data"]], 
                              method = "spearman")    
        
        cor_table <- as.data.frame(cbind(cor_test$estimate[[1]], cor_test$p.value))
        names(cor_table) <- c("rho", "p-value")
        cor_table
    })
