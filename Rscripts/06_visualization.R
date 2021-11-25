# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("ggplot2")
library("ggthemes")
library("scales")
library("patchwork")
library("keras")

# Load data ---------------------------------------------------------------
#data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")
data_clean_all <- read_tsv(file = "data/02_data_clean_all.tsv.gz")
data_clean_matched <- read_tsv(file = "data/03_data_clean_matched.tsv.gz")
data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")

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
      ggplot(aes(x = Allele),
             fill = Allele) +
      geom_text(aes(label = ..count..), 
             stat = "count", vjust = -0.5, 
             colour = "black", size = 4) +
      geom_bar(fill="#006633", 
               alpha = 0.7) +
      theme_hc() + 
      theme(axis.title = element_text(size = 12),
            axis.text.x = element_text(angle = 35, 
                                    hjust = 1,
                                    size = 12),
            axis.text.y = element_text(size = 12)) +
      labs(x = "HLA allele", 
           y = "Frequency") +
      scale_y_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))        


## Distribution of the peptide lenght 
# n = 579.880
data_clean_all %>% 
   mutate(pep_lenght = as.factor(pep_lenght)) %>% 
   ggplot(aes(x = pep_lenght, 
              fill= pep_lenght)) + 
      geom_bar(alpha = 0.7,
               fill="#006633") + 
      geom_text(aes(label = ..count..),
                stat = "count", 
                vjust = -0.5,
                colour = "black", 
                size = 4) +
      theme_hc() + 
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 12)) +
      labs(x = "Epitope length", 
           y = "Frequency") +
     scale_y_continuous(labels = comma_format(big.mark = ".",
     decimal.mark = ","))

# Distribution of peptide frequency in HLA*02:01 restricted data 
# Only positive cases
data_A0201 %>% 
   filter(Binding == 1) %>% 
   group_by(Peptide) %>% 
   summarise(count = n()) %>% 
   mutate(Peptide = case_when(count < 600 ~ "Other", 
                              TRUE ~ Peptide)) %>% 
   ggplot(aes(x = reorder(Peptide, -count), y = count)) + 
   geom_bar(stat = "identity",
            fill="#006633", 
            alpha = 0.7) +
   theme_hc() + 
   theme(axis.title = element_text(size = 12),
         axis.text.x = element_text(angle = 35, 
                                    hjust = 1,
                                    size = 12),
         axis.text.y = element_text(size = 12)) +
   labs(x = "Epitope", 
        y = "Frequency") +
   scale_y_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ",")) 

# Save plots --------------------------------------------------------------

