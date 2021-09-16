# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("janitor")


# Load data ---------------------------------------------------------------
data_raw_combined <- read_tsv(file = "data/01_data_raw_combined.tsv.gz")
pMHC_raw <- read_tsv(file = "data/01_pMHC_raw.tsv.gz")


# Wrangle data ------------------------------------------------------------
data_clean <- data_raw_combined %>% 
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
   drop_na()

# Work with subset
set.seed(1234)
data_clean <- data_clean %>% sample_n(50)

### LUCILLE
#abcde %>% str_detect("[^abcd], negate = TRUE")
#df %>% filter(str_detect(Peptide, "[^abcd], negate = TRUE"))

HLA_X <- c("HLA-A","HLA-A_1","HLA-B","HLA-B_1","HLA-C","HLA-C_1")
data_clean[HLA_X] <- data_clean[HLA_X] %>% 
   map(~str_sub(.x, 
                start = 1, 
                end = 7)) %>% 
   as_tibble()


## Wrangle pMHC -----------------------------------------------------------
col_names <- colnames(pMHC_raw) %>% 
   str_subset(., "HLA") %>% 
   str_replace("HLA.", "") %>%
   str_replace("\\.", "\\:") %>% 
   gsub(pattern = "^(.{1})(.*)$",        
        replacement = "\\1*\\2",
        x = .) %>% 
   append("Peptide", 
          after = 0) 

pMHC_clean <- pMHC_raw %>% 
   row_to_names(row_number = 1) %>%
   clean_names() %>% 
   select("peptide",
          matches("el_rank")) %>% 
   rename_at(vars(everything()), 
             function(x) col_names)



# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_data_clean.tsv.gz")
write_tsv(x = pMHC_clean,
          file = "data/02_pMHC_clean.tsv.gz")