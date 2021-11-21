# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("ggplot2")
library("ggthemes")
library("scales")

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
      top_n(10) %>% 
      select(Allele)

inner_join(data_clean_matched, 
           top_alelle, by = "Allele") %>% 
      ggplot(aes(x = Allele)) +
      geom_text(aes(label = ..count..), 
             stat = "count", vjust = -0.5, 
             colour = "black", size = 3) +
      geom_bar(fill="#3366CC") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 35, 
                                       hjust = 1)) +
      labs(x = "HLA allele", 
           y = "Allele count") +
      scale_y_continuous(labels = comma_format(big.mark = "."))


## Distribution of the peptide lenght 
data_clean_all %>% 
   mutate(pep_lenght = as.factor(pep_lenght)) %>% 
   ggplot(aes(x = pep_lenght)) + 
      geom_bar(fill="#3366CC") + 
      geom_text(aes(label = ..count..), 
                stat = "count", vjust = -0.5, 
                colour = "black", size = 3) +
      annotate(geom="text", 
               x= as.factor(14), y=280000, 
               label="n = 579.880",
               color="black") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", 
                                      size = 16),
            plot.subtitle = element_text(face = "italic")) +
      labs(x = "Peptide lenght", 
           y = "Peptide count") +
      scale_y_continuous(labels = comma_format(big.mark = "."))


# Save plots --------------------------------------------------------------
