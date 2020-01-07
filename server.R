## server logic
server <- function(input, output, session) {
    
    ## find available clinical variables for selected gene
    cv <- reactive({
              clinical_variables <- character()
              for (i in datasets) {
              if(input$gene %in% colnames(eval(as.symbol(i))[["expression_data"]])) {
                 clinical_variables <- unique(
                                              c(clinical_variables,
                                                colnames(eval(as.symbol(i))[["clinical_data"]])
                                                )
                                              )      
                 }
              }
              return(clinical_variables)
    })
    
    ## populate the available clinical variables for selected gene in UI
    observe({
        updateSelectizeInput(session,
                      inputId = "grouping",
                      label = NULL,
                      choices = cv()
                      )
    })
        
    ## find the available clinical variables for selected gene and clinical data in UI
    ad <- reactive({
        available_datasets <- character()
        for (i in datasets) {
            if(all(c(input$gene, 
                     input$grouping) %in% colnames(eval(as.symbol(i))[["data"]])
                    )
            ) {
                available_datasets <- unique(
                    c(available_datasets,
                      i
                    )
                )
            }
        }
        return(available_datasets)
    })
    
    ## populate the available clinical variables for selected gene and clinical data
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
                        group_by(!!grp()) %>%
                        summarize(mean = mean(!!gv()),
                                  median = median(!!gv()),
                                  sd = sd(!!gv())
                                )
        summ
    })

    output$graph <- renderPlot({
        validate(
            need(input$gene != '', 'Please choose a gene.'),
            need(input$grouping != '', 'Please choose a clinical variable.'),
            need(input$dataset != '', "Please choose a dataset")
        )
        ## Visualize expression values across grade
        ggplot(data = datasetInput()[["data"]],
               aes(x = !!grp(),
                   y = !!gv())
               ) +
            geom_boxplot() +
            geom_jitter(width = .25,
                        color = "#0072B2") +             # accessible blue
            theme_bw() +
            ggtitle("Expression by WHO Grade") +
            xlab("WHO Grade") +
            ylab("Expression") +
            stat_summary(fun.y=mean,                      # add in a blue dashed line at the mean
                         geom = "errorbar",
                         aes(ymax = ..y.., ymin = ..y..),
                         width = .75,
                         linetype = "dashed",
                         color = "#0072B2")
          })

    variance <- reactive({
                          aov(data = datasetInput()[["data"]],            # aov, call the dataset
                                 formula = as.formula(paste0(input$gene,  # formula to take gene input
                                                             "~",         # interpret grade as variable
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
                                    names(anova_pvalue) <- c("p adj")
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

    }

