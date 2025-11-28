
#Clearing enviroment
rm(list = ls())

#Loading required modules
install.packages('tidyverse')
install.packages("ggpubr")
install.packages("pheatmap")
install.packages("plotly")
install.packages("reshape2")
library(ggpubr)
library(tidyverse)
library(pheatmap)
library(ggrepel)
library(reshape2)
library(plotly)


#Reading in tsv files
AvsD <- read_tsv('A_vs_D.deseq2.results.tsv')
AvsE <- read_tsv('A_vs_E.deseq2.results.tsv')

########################
## Summary Satitstics ##
########################

padj_threshold <- 0.05 #Sets threshold for padj
log2fc_threshold <- 1.0 #Sets threshold for log2FC

sum_stats <- function(df, comp_name){
  
  df$expression <- ifelse(is.na(df$padj) == TRUE, #Checks for NA values 
                          "MISSING VALUE(s)", #Assingns gives this to rows with NA padj
                          ifelse((df$padj < padj_threshold & df$log2FoldChange > log2fc_threshold), #Checks for upregulated
                                 "Upregulated", #Gives this
                                 ifelse((df$padj < padj_threshold & df$log2FoldChange < -log2fc_threshold), #Checks for downregulated
                                        "Downregulated", #Gives this
                                        "Not significant"))) #Everything else get this
                          
  upregulated <- nrow(df[df$expression == "Upregulated", ]) #Counts rows that are upregulated
  downregulated <- nrow(df[df$expression == "Downregulated", ]) #Counts rows that are downregulated 
  
  cat("###### Expression changes in", comp_name, "######\n") #Prints name of data set
  cat("No. of upregulated genes:", upregulated, "\n") #prints upregulated
  cat("No. of downregulated genes:", downregulated, "\n") #prints downregulated
  
  padj_summ <- summary(df$padj) #Summarise padj
  log2fc_summ <- summary(df$log2FoldChange) #summarises log 2 fold change
  
  cat("Summary of adjusted p-values:\n")
  print(padj_summ) 
  cat("Summary of log2 Fold Change:\n")
  print(log2fc_summ)
  return(df) # Returns df with expression column
}

AvsD <- sum_stats(AvsD, "AvsD") #Runs sum_stast() and adds expression column to AvsD
AvsE <- sum_stats(AvsE, "AvsE") #Runs sum_stats() and adds expression column to AvsE

###########
## Plots ##
###########

make_volcano <- function(df, comp_name, xcrop = c(-10,10), ycrop = c(0,50)){
  
  plot_title <- paste("Volcano plot of", comp_name)
  
  plot <- ggplot(data = subset(df,!is.na(padj)),
         aes(x = log2FoldChange, y = -log10(padj),
         color = expression, label = gene_id)) +
    geom_point(alpha = 0.5) +
    labs(title = plot_title,
         x = "log2(Fold Change)",
         y = "-log10(adjusted p-value)",
         color = "Significance") +
    coord_cartesian(xlim = xcrop, ylim= ycrop) +
    scale_color_manual(values = c("blue", "azure4","red"))
  
  return(plot)
}

make_ma <- function(df, comp_name){
  
  plot_title <- paste("MA plot of", comp_name)
  
  plot <- ggmaplot(data = df) +
    labs(title = plot_title,
         x = "log2(Mean Expression)",
         y = "log2(Fold Change)",
         color = "Significance")
  
}

make_hist <- function(df, comp_name, bin = 0.05){
  
  plot_title <- paste("Histogram of", comp_name, "adjusted p-values")
  
  plot <- ggplot(aes(padj), data = df) + 
    geom_histogram(binwidth = bin) + 
    labs(title = plot_title)
  
  return(plot)
}


volcano_AvsD <- make_volcano(AvsD, "A vs D", c(-10,10), c(0,25))
volcano_AvsE <- make_volcano(AvsE, "A vs E")
volcano_AvsD
volcano_AvsE


ma_AvsD <- make_ma(AvsD, "A vs D")
ma_AvsE <- make_ma(AvsE, "A vs E")
ma_AvsD
ma_AvsE


hist_AvsD <- make_hist(AvsD, "A vs D")
hist_AvsE <- make_hist(AvsE, "A vs E")
hist_AvsD
hist_AvsE


AvsD$set <- "A vs D" #sets all of the column "set" to A vs D
AvsE$set <- "A vs E" #sets all of the column "set" to A vs E

AvsD_top <- AvsD %>% arrange(padj) %>% slice(1:100) #Gets 100 lowest padj values from each data set
AvsE_top <- AvsE %>% arrange(padj) %>% slice(1:100)

combined <- rbind(AvsD_top, AvsE_top) #combines both with lowest padj

combined_sub <- subset(combined,duplicated(gene_id) | duplicated(gene_id, fromLast=TRUE)) #removes any genes which are not in both sets

p <- ggplot(combined, aes(set, gene_id)) +
  geom_tile(aes(fill=log2FoldChange)) +
ggplotly(p)

###########################
## Signifcant Genes List ##
###########################

sig_AvsD <- AvsD %>% filter(expression == "Upregulated" | expression == "Downregulated")
sig_AvsE <- AvsE %>% filter(expression == "Upregulated" | expression == "Downregulated")





