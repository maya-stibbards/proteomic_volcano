install.packages(c("plyr","tidyverse","gplots","pheatmap",
                   "gridExtra","VennDiagram","ggseqlogo", "readxl"))

library(plyr)
library(tidyverse)
library(gplots)
library(pheatmap)
library(gridExtra)
library(VennDiagram)
library(ggseqlogo)
library(readxl)

df <- read_excel("Anshul and Maia results.xlsx", sheet = 1, na = "NA")
dfdrop <- df[-c(1,2,4,5,7:10,12:14)]
# dfvar <- dfdrop %>% as.numeric(as.character(unlist(dfdrop[[1]]))) %>% drop_na() 
# dfnumeric <- as.numeric(as.character(dfdrop$log2FC))
# df_tidy <- dfvar %>% drop_na()
numericdf <- as.numeric(as.character(unlist(dfdrop$log2FC)))
df_tidy <- na.omit(dfdrop, cols=c("log2FC", "adj.pvalue"))
dfdrop %>% summarise(Number_of_proteins = n())
df_tidy %>% summarise(Number_of_proteins = n())




quantile_normalisation <- function(df){
  
  # Find rank of values in each column
  df_rank <- map_df(df,rank,ties.method="average")
  # Sort observations in each column from lowest to highest 
  df_sorted <- map_df(df,sort)
  # Find row mean on sorted columns
  df_mean <- rowMeans(df_sorted)
  
  # Function for substiting mean values according to rank 
  index_to_mean <- function(my_index, my_mean){
    return(my_mean[my_index])
  }
  
  # Replace value in each column with mean according to rank 
  df_final <- map_df(df_rank,index_to_mean, my_mean=df_mean)
  
  return(df_final)
}

df_norm <- df_tidy %>% select(-c('Gene.Name')) %>% 
  quantile_normalisation() %>% 
  bind_cols(df_tidy[,1],.)