## load packages
library(ggplot2)
library(dplyr)
library(car)
library(shiny)
library(stringr)
library(plotly)
library(shinythemes)

## load data
#### call datasets
datasets_load <- c( "datasets/GSE85135_Clark_V_KO.Rda", # find all files in this directory to load rather than hard 
                    "datasets/GSE58037_Clark_V_KO.Rda", # coding in the future
                    "datasets/GSE77259_Schulten_H_KO.Rda",
                    "datasets/GSE74385_Schmidt_M_KO.Rda",
                    "datasets/GSE54934_Torres-Martin_M_KO.Rda",
                    "datasets/GSE55609_Yeh_T_KO.Rda",
                    "datasets/GSE16156_DUrso_O_KO.Rda",
                    "datasets/GSE16581_Lee_Y_KO.Rda",
                    "datasets/GSE12530_Keller_A_KO.Rda",
                    "datasets/GSE9438_Claus_E_KO.Rda",
                    "datasets/GSE4780_Scheck_A_KO.Rda"
                   ) # load only some versus the others - add toggle (look into)

#### load datasets
lapply(datasets_load, 
       load, 
       .GlobalEnv)

#### master list - idea being create a master list to access rather than hardcoded thing
# master_list <- list()
# for (k in 1:length(datasets_load)){
#     print(datasets_load[k])
#     master_list[[k]] <- eval(as.symbol(load(datasets_load[k])))
# }


## create choices

### datasets
datasets <- c("Clark_V_2016", 
              "Clark_V",
              "Schulten_H",
              "Schmidt_M",
              "TorresMartin_M",
              "Yeh_T",
              "DUrso_O",
              "Lee_Y",
              "Keller_A",
              "Claus_E",
              "Scheck_A"
)

#### variables across datasets  ** depreicated **
# variables <- c("age",       
#                "sex",          
#                "grade",        
#                "histology",    
#                "type",        
#                "location",     
#                "mib1_index",   
#                "recurrence",   
#                "recurrence_yrs", 
#                "recurrence_freq", 
#                "followup_days",
#                "survival_yrs",
#                "tissue",       
#                "progression",  
#                "radiation",   
#                "rna_integrity"
# )

#### genes by which to analyze
gene <- character()                      # create an empty character vector
for (p in datasets) {                    # loop to look through datasets for all available genes
    gene <- sort(unique(                 # assign unique gene    names to the character vector
        c(gene,                       
          colnames(eval(as.symbol(p))[["expression_data"]]) # look through the expression sets for the available genes in each
        )
    )
    )
}

