
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

make_volcano <- function(df, comp_name, xcrop = c(-10,10), ycrop = c(0,50)){ #Creates a function to make a volcano
  
  plot_title <- paste("Volcano plot of", comp_name) #Creates object with what the plot title is
  
  plot <- ggplot(data = subset(df,!is.na(padj)), #Creates the plot. Uses data with non na padj values
         aes(x = log2FoldChange, y = -log10(padj), #X axis is log2Fold change, y axis is -log10(padj)
         color = expression, label = gene_id)) + # Colours data points by expression coloumn. Labels them via gene_id
    geom_point(alpha = 0.5) + #Sets opacity of the points
    labs(title = plot_title, ç
         x = "log2(Fold Change)", #Labels x axis
         y = "-log10(adjusted p-value)", #Labels y axis
         color = "Significance") + #Lables the legend
    coord_cartesian(xlim = xcrop, ylim= ycrop) + #Crops axis based on provided parameters
    scale_color_manual(values = c("blue", "azure4","red")) #Sets colour of the points
  
  return(ggplotly(plot)) #Returns the plot as a ggplotly plot
}

make_ma <- function(df, comp_name){ #Function to create an MA plot
  
  plot_title <- paste("MA plot of", comp_name) #Creates object with what the plot title is
  
  plot <- ggmaplot(data = df) + #Makes the plot using data from provided data set
    labs(title = plot_title, #Labels title
         x = "log2(Mean Expression)", #Labels x axis
         y = "log2(Fold Change)", #Labels y axis
         color = "Significance") #Labels legend
  
  return(ggplotly(plot)) #Returns the plot as a ggplotly plot
}

make_hist <- function(df, comp_name, bin = 0.05){#Function to create a histogram
  
  plot_title <- paste("Histogram of", comp_name, "adjusted p-values") #Creates object with what the plot title is
  
  plot <- ggplot(aes(padj), data = df) + #Makes the plot with data from provided data set, builds it on padj
    geom_histogram(binwidth = bin) + #Creates histogram. Bin width is specified in the function 
    labs(title = plot_title) #Gives the plot a title
  
  return(plot) #Returns the plot as a ggplotly plot
}


volcano_AvsD <- make_volcano(AvsD, "A vs D", c(-10,10), c(0,25)) #Creates plot for AvsD
volcano_AvsE <- make_volcano(AvsE, "A vs E") #Creates plot for AvsE
volcano_AvsD #Shows plot
volcano_AvsE #Shows plot


ma_AvsD <- make_ma(AvsD, "A vs D") #Creates plot for AvsD
ma_AvsE <- make_ma(AvsE, "A vs E") #Creates plot for AvsE
ma_AvsD #Shows plot
ma_AvsE #Shows plot


hist_AvsD <- make_hist(AvsD, "A vs D") #Creates plot for AvsD
hist_AvsE <- make_hist(AvsE, "A vs E") #Creates plot for AvsE
hist_AvsD #Shows plot
hist_AvsE #Shows plot


AvsD$set <- "A vs D" #sets all of the column "set" to A vs D
AvsE$set <- "A vs E" #sets all of the column "set" to A vs E

AvsD_top <- AvsD %>% arrange(padj) %>% slice(1:100) #Gets 100 lowest padj values from AvsD
AvsE_top <- AvsE %>% arrange(padj) %>% slice(1:100) #Gets 100 lowest padj values from AvsE

combined <- rbind(AvsD_top, AvsE_top) #Combines both dfs containg lowest padj

combined_sub <- subset(combined,duplicated(gene_id) | duplicated(gene_id, fromLast=TRUE)) #removes any genes which are not in both sets

p <- ggplot(combined_sub, aes(set, gene_id)) + #Makes the plot. Data is from combined_sub, x axis is the data set y axis is gene_id
  geom_tile(aes(fill=log2FoldChange)) #Makes plot a heatmap
ggplotly(p) #Makes it a plotly plot

###########################
## Signifcant Genes List ##
###########################

sig_AvsD <- AvsD %>% filter(expression == "Upregulated" | expression == "Downregulated") #Creats df with only upregulated and downregulated genes
sig_AvsE <- AvsE %>% filter(expression == "Upregulated" | expression == "Downregulated") #Creats df with only upregulated and downregulated genes





