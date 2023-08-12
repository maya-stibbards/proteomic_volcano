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
dfdrop$adj.pvalue_log <- (log10(dfdrop$adj.pvalue)*-1)
df_tidy <- na.omit(dfdrop, cols=c("log2FC", "−log10 p-value"))
dfdrop %>% summarise(Number_of_proteins = n())
df_tidy %>% summarise(Number_of_proteins = n())


df_tidy %>% ggplot(aes(log2FC,adj.pvalue_log)) + geom_point()

df_tidy %>%
  # significant observations
  mutate(threshold = if_else(log2FC >= 2 & adj.pvalue_log >= 1.3 |
                               log2FC <= -2 & adj.pvalue_log >= 1.3,"A", "B")) %>%
  # coloured according to the threshold
  ggplot(aes(log2FC,adj.pvalue_log, colour = threshold)) +
  geom_point(alpha = 0.5) +
  # dotted lines to indicate the threshold
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5) + 
  geom_vline(xintercept = 2, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = -2, linetype = 2, alpha = 0.5) +
  # colour of the points
  scale_colour_manual(values = c("A"= "red", "B"= "black")) +
  xlab("log2 fold change") + ylab("-log10 p-value") + # Relabel the axes
  theme_minimal() + # Set the theme
  theme(legend.position="none") # Hide the legend