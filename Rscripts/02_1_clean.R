# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("janitor")


# Load data ---------------------------------------------------------------
data_raw_combined <- read_tsv(file = "data/01_data_raw_combined.tsv.gz")


# Wrangle data ------------------------------------------------------------
data_clean <- data_raw_combined %>% 
   select("Experiment",
          "TCR BioIdentity", 
          "Amino Acids", 
          matches("HLA")) %>% 
   rename(Peptide = `Amino Acids`) %>% 
   mutate(Peptide = strsplit(Peptide, ",")) %>% 
   unnest(Peptide) %>%    
   filter(str_length(Peptide) == 9) %>% 
   separate(col = `TCR BioIdentity`,
            into = "CDR3b",
            extra = "drop") %>% 
   mutate(CDR3b = na_if(CDR3b, "")) %>% 
   filter_at(.vars = vars(CDR3b, Peptide),
             .vars_predicate = all_vars(str_detect(.,
                                                   "[^LITSFANMPGKQYVHWDERC]", 
                                                   negate = TRUE))) %>% 
   drop_na()
   
# Renaming HLA columns 
HLA_X <- c("HLA-A","HLA-A_1","HLA-B","HLA-B_1","HLA-C","HLA-C_1")
data_clean[HLA_X] <- data_clean[HLA_X] %>% 
   map(~str_sub(.x, 
                start = 1, 
                end = 7)) %>% 
   as_tibble()

# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_data_clean.tsv.gz")