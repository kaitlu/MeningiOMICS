test_data <- gse108089_rna %>%
    filter(gene == 'RFC2') 
    

mean_value <- mean(test_data$logvalue)
sd_value <- sd(test_data$logvalue)

test_data$standardized <- (test_data$logvalue-mean_value)/sd_value
test_data$standardized
