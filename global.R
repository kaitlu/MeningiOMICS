## load packages
library(ggplot2)
library(dplyr)
library(car)
library(shiny)
library(stringr)
library(plotly)
library(shinythemes)
library(DT)
library(heatmaply)
library(rclipboard)


## list data

### list array datasets
array_load <- paste0("datasets/array/", 
                     list.files(path = "datasets/array", pattern = "*.Rda"))

### list methylation datasets
methyl_load <- paste0("datasets/methylation/", 
                     list.files(path = "datasets/methylation/", pattern = "*.Rda"))


#### load datasets
lapply(c(array_load, methyl_load), 
       load, 
       .GlobalEnv)

######################################################

## create baseline choices

### omics data types
omics <- c("Microarray", "Methylation")

### datasets by types
array_datasets <- c("Clark_V_2016", 
                    "Clark_V_2013",
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

methyl_datasets <- c("Agnihotri_S_2013",
                     "Harmanci_a_2013")

######################################################

## create gene options 

### genes by which to analyze - array
array_genes <- character()              # create an empty character vector
for (i in array_datasets) {             # loop to look through datasets for all available genes
    array_genes <- c(array_genes,
                     colnames(eval(as.symbol(i))[["expression_data"]]) # look through the expression sets for the available genes in each
              )
    }
array_genes <- sort(unique(array_genes))

### genes by which to analyze - methyl
methyl_genes <- character()              # create an empty character vector
for (i in methyl_datasets) {             # loop to look through datasets for all available genes
    methyl_genes <- c(methyl_genes,
                     colnames(eval(as.symbol(i))[["expression_data"]]) # look through the expression sets for the available genes in each
    )
}
methyl_genes <- sort(unique(methyl_genes))

######################################################

### global functions
## formula to find columns which are not continuous variables for multigene ANOVA (and potentially future) purposes
not_numeric <- function(x){!is.numeric(x)}

