## load packages
library(ggplot2)
library(dplyr)
library(car)
library(shiny)
library(stringr)

## load data
#### call datasets
datasets_load <- c( "~/Documents/Thesis/mgm_eda/datasets/GSE85135_Clark_V_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE58037_Clark_V_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE77259_Schulten_H_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE74385_Schmidt_M_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE54934_Torres-Martin_M_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE55609_Yeh_T_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE16156_DUrso_O_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE16581_Lee_Y_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE12530_Keller_A_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE9438_Claus_E_KO.Rda",
                    "~/Documents/Thesis/mgm_eda/datasets/GSE4780_Scheck_A_KO.Rda"
                   )

#### load datasets
lapply(datasets_load, 
       load, 
       .GlobalEnv)

## create choices

#### datasets
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

#### variables across datasets
variables <- c("age",       
               "sex",          
               "grade",        
               "histology",    
               "type",        
               "location",     
               "mib1_index",   
               "recurrence",   
               "recurrence_yrs", 
               "recurrence_freq", 
               "followup_days",
               "survival_yrs",
               "tissue",       
               "progression",  
               "radiation",   
               "rna_integrity"
)

#### genes by which to analyze
gene <- c("NEUROD1",
           "PAX4",
           "INSM1",
           "NKX2-2",
           "NEUROG3",
           "PTTG1",
           "F3"
)