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
data_clean_all <- read_tsv(file = "data/02_data_clean_all.tsv.gz")
data_clean_matched <- read_tsv(file = "data/03_data_clean_matched.tsv.gz")
data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")

# Creating plots ------------------------------------------------------------

## HLA distribution ---------------------------------------------------------
# 10 most abundant alleles 
top_alelle <- data_clean_matched %>% 
      group_by(Allele) %>% 
      summarise(Count = n()) %>%  
      top_n(10) %>% 
      select(Allele)

p_HLA_distribution <- inner_join(data_clean_matched,
                               top_alelle, by = "Allele") %>%
   ggplot(aes(x = Allele),
             fill = Allele) +
      geom_text(aes(label =  format(..count.. , big.mark = " ", decimal.mark = ".")), 
             stat = "count", vjust = -0.5, 
             colour = "black", size = 4) +
      geom_bar(fill="#006633", 
               alpha = 0.7) +
      theme_hc() + 
      theme(axis.title = element_text(size = 14),
            axis.text.x = element_text(angle = 35, 
                                    hjust = 1,
                                    size = 12),
            axis.text.y = element_text(size = 12)) +
      labs(x = "HLA allele", 
           y = "Frequency") +
      scale_y_continuous(labels = comma_format(big.mark = " ",
                                            decimal.mark = ". "))        


## Distribution of the peptide length -------------------------------------------
p_peptide_distribution <- data_clean_all %>% 
      mutate(pep_length = as.factor(pep_length)) %>% 
      ggplot(aes(x = pep_length, 
                 fill= pep_length)) + 
         geom_bar(alpha = 0.7,
                  fill="#006633") + 
         geom_text(aes(label = format(..count.. , big.mark = " ", decimal.mark = ".")),
                   stat = "count", 
                   vjust = -0.5,
                   colour = "black", 
                   size = 4) +
         theme_hc() + 
         theme(axis.text = element_text(size = 12),
               axis.title = element_text(size = 14)) +
         labs(x = "Epitope length", 
              y = "Frequency") +
        scale_y_continuous(labels = comma_format(big.mark = " ",
        decimal.mark = "."))


## Distribution of Epitope frequency in HLA*02:01 restricted data ----------------
# Only positive cases
p_epitope_distribution <- data_A0201 %>% 
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
   theme(axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 35, 
                                    hjust = 1,
                                    size = 12),
         axis.text.y = element_text(size = 12)) +
   labs(x = "Epitope", 
        y = "Frequency") +
   scale_y_continuous(labels = comma_format(big.mark = " ",
                                            decimal.mark = ". ")) 

# Save plots --------------------------------------------------------------

ggsave(filename = "results/HLA_distribution.png",
       plot = p_HLA_distribution,
       width = 9.5,
       height = 4.5, 
       units = "in")


ggsave(filename = "results/peptide_distribution.png",
       plot = p_peptide_distribution,
       width = 9.5,
       height = 4.5, 
       units = "in")

ggsave(filename = "results/epitope_distribution.png",
       plot = p_epitope_distribution,
       width = 9.5,
       height = 4.5, 
       units = "in")

