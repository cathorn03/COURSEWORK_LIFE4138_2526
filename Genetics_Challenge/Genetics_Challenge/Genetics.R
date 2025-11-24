
#Clearing enviroment
rm(list = ls())

#Loading required modules
install.packages('tidyverse')
library(tidyverse)

#Reading in tsv files
AvsD <- read_tsv('A_vs_D.deseq2.results.tsv')
AvsE <- read_tsv('A_vs_E.deseq2.results.tsv')

########################
## Summary Satitstics ##
########################



padj_threshold <- 0.05 #Sets threshold for padj
log2fc_threshold <- 1.0 #Sets threshold for log2FC


sum_stats <- function(df, comp_name){
  df$expression <- ifelse((df$padj < padj_threshold & df$log2FoldChange > log2fc_threshold), #Determines if gene is upregulated
                             "upregulated", #Assigns "upregulated" if true
                             ifelse((df$padj < padj_threshold & df$log2FoldChange < -log2fc_threshold), #Determines if gene is downregulated
                                        "downregulated", #Assigns "downregulated" if true
                                        "not significant")) #If neither, asigend NA
  upregulated <- nrow(df[df$expression == "upregulated", ])
  downregulated <- nrow(df[df$expression == "downregulated", ])
  
  cat("###### Expression changes in", comp_name, "######\n")
  cat("No. of upregulated genes:", upregulated, "\n")
  cat("No. of downregulated genes:", downregulated, "\n")
  
  padj_summ <- summary(df$padj)
  log2fc_summ <- summary(df$log2FoldChange)
  
  cat("Summary of adjusted p-values:")
  print(padj_summ)
  cat("Summary of log2 Fold Change:")
  print(log2fc_summ)
  return(df) # Returns df with expression column
}
  
AvsD <- sum_stats(AvsD, "AvsD") #Runs sum_stast() and adds expression column to AvsD
AvsE <- sum_stats(AvsE, "AvsE") #Runs sum_stats() and adds expression column to AvsE



