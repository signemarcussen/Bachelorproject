# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("janitor")


# Load data ---------------------------------------------------------------
data_raw_combined <- read_tsv(file = "data/01_data_raw_combined.tsv.gz")


# Wrangle data ------------------------------------------------------------
data_clean_all <- data_raw_combined %>% 
   select("Experiment",
          "TCR BioIdentity", 
          "Amino Acids", 
          matches("HLA")) %>% 
   rename(Peptide = `Amino Acids`) %>% 
   mutate(Peptide = strsplit(Peptide, ",")) %>% 
   unnest(Peptide) %>% 
   separate(col = `TCR BioIdentity`,
            into = "CDR3b",
            extra = "drop") %>% 
   mutate(CDR3b = na_if(CDR3b, ""),
          pep_length = str_length(Peptide)) %>% 
   filter_at(.vars = vars(CDR3b, Peptide),
             .vars_predicate = all_vars(str_detect(.,
                                                   "[^LITSFANMPGKQYVHWDERC]", 
                                                   negate = TRUE))) %>% 
   drop_na()

# Subset by peptides of length = 9
data_clean <- data_clean_all %>% 
   filter(pep_length == 9) %>% 
   select(-pep_length)
   
# Renaming HLA columns 
HLA_X <- c("HLA-A","HLA-A_1","HLA-B","HLA-B_1","HLA-C","HLA-C_1")
data_clean[HLA_X] <- data_clean[HLA_X] %>% 
   map(~str_sub(.x, 
                start = 1, 
                end = 7)) %>% 
   as_tibble()

# Write data --------------------------------------------------------------
write_tsv(x = data_clean_all,
          file = "data/02_data_clean_all.tsv.gz")

write_tsv(x = data_clean,
          file = "data/02_data_clean.tsv.gz")