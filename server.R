## server logic
server <- function(input, output) {
    ## pull out dataset
    datasetInput <- reactive({
        eval(as.symbol(input$dataset))
    })
    
    ## pull out grouping
    grp <- reactive({as.symbol(input$grouping)
    })
        
    ## pull out gene
    gv <- reactive({as.symbol(input$gene)
    })
    
 
    ## provide summary statistics over gene
    output$summary <- renderTable({
            summ <- datasetInput()[["data"]] %>% 
                        group_by(!!grp()) %>%
                        summarize(mean = mean(!!gv()),
                                  median = median(!!gv()),
                                  sd = sd(!!gv())
                                )
        summ
    })
    
    output$graph <- renderPlot({
        ## Visualize expression values across grade
        ggplot(data = datasetInput()[["data"]], 
               aes(x = !!grp(), 
                   y = !!gv())
               ) +
            geom_boxplot() +
            theme_bw() +
            ggtitle("Expression by WHO Grade") +
            xlab("WHO Grade") +
            ylab("Expression") +
            stat_summary(fun.y=mean,                      # add in a blue dashed line at the mean
                         geom = "errorbar", 
                         aes(ymax = ..y.., ymin = ..y..),
                         width = .75, 
                         linetype = "dashed",
                         color = "blue")
          })
    
    variance <- reactive({
                          aov(data = datasetInput()[["data"]],            # aov, call the dataset
                                 formula = as.formula(paste0(input$gene,  # formula to take gene input
                                                             "~",         # interpret grade as variable
                                                             input$grouping
                                 )))
                    })
    
    output$anova <- renderTable({
                                    anova <- summary(variance())
                                    anova_pvalue <- data.frame(anova[[1]]$'Pr(>F)'[1])
                                    names(anova_pvalue) <- c("p adj")
                                    row.names(anova_pvalue) <- "overall"
                                    anova_pvalue
                                 }, 
                                digits = -2,
                                rownames = TRUE)
    
    output$tukey <- renderTable({
                                    tukey <- TukeyHSD(variance())
                                    as.data.frame(tukey[[input$grouping]])
                                },
                                rownames = TRUE)
    
    }

