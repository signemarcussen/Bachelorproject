# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("ggplot2")
library("ggthemes")

# Load data ---------------------------------------------------------------
#data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")
data_clean_all <- read_tsv(file = "data/02_data_clean_all.tsv.gz")
data_clean_matched <- read_tsv(file = "data/03_data_clean_matched.tsv.gz")

# Wrangling data  ------------------------------------------------------------


# Creating plots ------------------------------------------------------------
## HLA distribution 

# 10 most abundant alleles 
top_alelle <- data_clean_matched %>% 
      group_by(Allele) %>% 
      summarise(Count = n()) %>%  
      top_n(15) %>% 
      select(Allele)

inner_join(data_clean_matched, 
           top_alelle, by = "Allele") %>% 
      ggplot(aes(x = Allele)) +
      geom_bar(fill="#3366CC")+
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 35, 
                                       hjust = 1),
            plot.title = element_text(face = "bold", 
                                      size = 16),
            plot.subtitle = element_text(face = "italic")) +
      labs(x = " ", 
           y = "Number of cases",
           title = "Distribution of the HLA alleles",
           subtitle = "Barplot of the 15 most abundant alleles, after peptide:HLA matching by NetMHCpan-4.1")

## Distribution of the peptide lenght 
data_clean_all %>% ggplot(aes(x = pep_lenght))+ 
      geom_bar(fill="#3366CC")+
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", 
                                      size = 16),
            plot.subtitle = element_text(face = "italic")) +
      labs(x = "Peptide lenght", 
           y = "Number of cases",
           title = "Distribution of the peptide lenght",
           subtitle = "Barplot xxxx")


# Save plots --------------------------------------------------------------
