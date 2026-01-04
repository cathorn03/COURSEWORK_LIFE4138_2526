#############################
## Module and Data Loading ##
#############################

#Clearing enviroment
rm(list = ls())

#Loading required modules
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("plotly")) install.packages("plotly")
if (!require("readr")) install.packages("readr")
if (!require("stringr")) install.packages("stringr")

library(dplyr)
library(ggplot2)
library(ggpubr)
library(plotly)
library(readr)
library(stringr)


#Reading in tsv files
AvsD <- read_tsv('A_vs_D.deseq2.results.tsv')
AvsE <- read_tsv('A_vs_E.deseq2.results.tsv')

#Creating output document
sink(file = "output.txt")

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
  cat("\n")
  return(df) # Returns df with expression column
}

AvsD <- sum_stats(AvsD, "AvsD") #Runs sum_stast() and adds expression column to AvsD
AvsE <- sum_stats(AvsE, "AvsE") #Runs sum_stats() and adds expression column to AvsE


###########
## Plots ##
###########

###Volcano plot###
make_volcano <- function(df, comp_name, xcrop = c(-10,10), ycrop = c(0,50)){ #Creates a function to make a volcano
  
  plot_title <- paste("Volcano plot of", comp_name) #Creates object with what the plot title is
  
  plot <- ggplot(data = subset(df,!is.na(padj)), #Creates the plot. Uses data with non na padj values
         aes(x = log2FoldChange, y = -log10(padj), #X axis is log2Fold change, y axis is -log10(padj)
         color = expression, label = gene_id)) + # Colours data points by expression coloumn. Labels them via gene_id
    geom_point(alpha = 0.5) + #Sets opacity of the points
    labs(title = plot_title,
         x = "log2(Fold Change)", #Labels x axis
         y = "-log10(adjusted p-value)", #Labels y axis
         color = "Significance") + #Lables the legend
    coord_cartesian(xlim = xcrop, ylim= ycrop) + #Crops axis based on provided parameters
    scale_color_manual(values = c("blue", "azure4","red")) #Sets colour of the points
  
  return(ggplotly(plot)) #Returns the plot as a ggplotly plot
}

###MA plot###
make_ma <- function(df, comp_name){ #Function to create an MA plot
  
  plot_title <- paste("MA plot of", comp_name) #Creates object with what the plot title is
  
  plot <- ggmaplot(data = df,  #Makes the plot using data from provided data set
                   top = 0 ) + #Removes point labels
    labs(title = plot_title, #Labels title
         x = "log2(Mean Expression)", #Labels x axis
         y = "log2(Fold Change)", #Labels y axis
         color = "Significance") #Labels legend
  
  return(ggplotly(plot)) #Returns the plot as a ggplotly plot
}

###Histogram###
make_hist <- function(df, comp_name, bin = 0.05){#Function to create a histogram
  
  plot_title <- paste("Histogram of", comp_name, "adjusted p-values") #Creates object with what the plot title is
  
  plot <- ggplot(aes(padj), data = df) + #Makes the plot with data from provided data set, builds it on padj
    geom_histogram(binwidth = bin,  #Creates histogram. Bin width is specified in the function 
                   colour = "black", fill = "azure4") + #Sets colour of the histogram
    labs(title = plot_title,
         x = "Adjusted p-value",
         y = "Frequency") #Gives the plot a title
  
  return(plot) #Returns the plot as a ggplotly plot
}

###Displaying plots###
volcano_AvsD <- make_volcano(AvsD, "A vs D", c(-10,10), c(0,25)) #Creates plot for AvsD
volcano_AvsE <- make_volcano(AvsE, "A vs E") #Creates plot for AvsE
volcano_AvsD #Shows plot
ggsave("volcano_AvsD.png", width=8, height=5) #Saves plot with specific dimensions
volcano_AvsE #Shows plot
ggsave("volcano_AvsE.png", width=8, height=5) #Saves plot with specific dimensions


ma_AvsD <- make_ma(AvsD, "A vs D") #Creates plot for AvsD
ma_AvsE <- make_ma(AvsE, "A vs E") #Creates plot for AvsE
ma_AvsD #Shows plot
ggsave("ma_AvsD.png", width=8, height=5) #Saves plot with specific dimensions
ma_AvsE #Shows plot
ggsave("ma_AvsE.png", width=8, height=5) #Saves plot with specific dimensions


hist_AvsD <- make_hist(AvsD, "A vs D") #Creates plot for AvsD
hist_AvsE <- make_hist(AvsE, "A vs E") #Creates plot for AvsE
hist_AvsD #Shows plot
ggsave("histogram_AvsD.png", width=5, height=4) #Saves plot with specific dimensions
hist_AvsE #Shows plot
ggsave("histogram_AvsE.png", width=5, height=4) #Saves plot with specific dimensions


###Heatmap###
AvsD$set <- "A vs D" #sets all of the column "set" to A vs D
AvsE$set <- "A vs E" #sets all of the column "set" to A vs E

AvsD_top <- AvsD %>% filter(expression == "Upregulated" | expression == "Downregulated") %>% #Selects only differentially expressed genes
  arrange(padj) %>% #Sorts from lowest to highest padj
  slice(1:20) #Gets the first 20 values
AvsE_top <- AvsE %>% filter(expression == "Upregulated" | expression == "Downregulated") %>% #Selects only differentially expressed genes
  arrange(padj) %>% #Sorts from lowest to highest padj
  slice(1:20) #Gets the first 20 values

combined_list <- rbind(AvsD_top, AvsE_top) #Combines both dfs containg lowest padj

combined_unique <- combined_list %>% count(gene_id) %>% #Produces a table of the counts of every gene_id
  filter(n == 1) %>% #Gets only gene_ids which are mentioned once
  inner_join(combined_list, by = 'gene_id') #Adds this gene_id full rows from combined_list to the new df combined_unique

unique_AvsD <- combined_unique %>% filter(combined_unique$set == "A vs D") #Separates out rows from AvsD
unique_AvsE <- combined_unique %>% filter(combined_unique$set == "A vs E") #Separates out rows from AvsE

inverse_unique_AvsD <- AvsE[AvsE$gene_id %in% unique_AvsD$gene_id,] #Gets the values from unique_AvsD and finds the same gene in AvsE
inverse_unique_AvsE <- AvsD[AvsD$gene_id %in% unique_AvsE$gene_id,] #Gets the values from unique_AvsE and finds the same gene in AvsD

combined_list_filled <- rbind(combined_list, inverse_unique_AvsD, inverse_unique_AvsE) #Adds all needed values into one df

hmap <- ggplot(combined_list_filled, aes(set, gene_id)) + #Makes the plot. Data is from ccombined_list_filled, x axis is the data set y axis is gene_id
  geom_tile(aes(fill=log2FoldChange)) +  #Makes plot a heatmap and fills cells acording to their log2FoldChange Value
  scale_fill_distiller(palette = "RdBu", direction = -1) + #Sets the colour palette to be red and blue. direction = -1 inverts thwe direction so blue is under expressed
  ggtitle(str_wrap("A heatmap of the log 2 fold change of the most significant genes in AvsD and AvsE", width = 45))+ #Adds the title and makes the text wrap every 45 characters
  labs(x = "Comparison Data Set", #Labels x-axis
       y = "Gene ID", #Labels y-axis
       fill = "log2(Fold Change)") #Labels legend

ggplotly(hmap) #Makes it a plotly plot
ggsave(file = "heatmap.png", width=6, height=8) #Saves plot with specific dimensions


###########################
## Signifcant Genes List ##
###########################


sig_AvsD <- AvsD %>% filter(expression == "Upregulated" | expression == "Downregulated") %>% #Creats df with only upregulated and downregulated genes
  select(gene_id, pvalue, padj, log2FoldChange) #Selects only gene_id, pvalue, padj, and log2FoldChange columns
sig_AvsE <- AvsE %>% filter(expression == "Upregulated" | expression == "Downregulated") %>% #Creats df with only upregulated and downregulated genes
  select(gene_id, pvalue, padj, log2FoldChange) #Selects only gene_id, pvalue, padj, and log2FoldChange columns

write_tsv(sig_AvsD, "significant_AvsD.tsv.gz")
write_tsv(sig_AvsE, "significant_AvsE.tsv.gz")

