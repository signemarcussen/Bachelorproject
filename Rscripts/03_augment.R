# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("janitor")


# Load data ---------------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")

## You must run "create_files_for_netMHCpan.R" to obtain these files:
pMHC_raw_1 <- read.table(file = "data/_raw/pMHC_predictions_1.xls", 
                         sep = "\t",
                         header = TRUE)
pMHC_raw_2 <- read.table(file = "data/_raw/pMHC_predictions_2.xls", 
                         sep = "\t",
                         header = TRUE)

# Wrangle data ------------------------------------------------------------

# Add ID column and join pMHC files
pMHC_raw_1 <- pMHC_raw_1 %>%
   as_tibble %>% 
   mutate(ID = 1:nrow(pMHC_raw_1))
pMHC_raw_2 <- pMHC_raw_2 %>%
   as_tibble() %>% 
   mutate(ID = 1:nrow(pMHC_raw_2))
pMHC_raw_combined <- inner_join(x = pMHC_raw_1,
                                y = pMHC_raw_2,
                                by = "ID")


## Extract desired column names
col_names <- colnames(pMHC_raw_combined) %>% 
   str_subset(., "HLA") %>% 
   str_replace("HLA.", "") %>%
   str_replace("\\.", "\\:") %>% 
   gsub(pattern = "^(.{1})(.*)$",        
        replacement = "\\1*\\2",
        x = .) %>% 
   append("Peptide", 
          after = 0) 


## Keep only peptides, EL_Rank and alleles - and make long format
colnames(pMHC_raw_combined) <- sapply(pMHC_raw_combined[1,], 
                                      as.character) %>% 
   make_clean_names()

pMHC_clean <- pMHC_raw_combined[-1,] %>% 
   select("peptide",
          matches("el_rank")) %>% 
   rename_at(vars(everything()), 
             function(x) col_names) %>% 
   pivot_longer(cols = -Peptide, 
                names_to = "Allele", 
                values_to = "EL_Rank") %>% 
   mutate(EL_Rank = as.numeric(as.character(EL_Rank)))


## Match peptide and alleles in the two files and keep only binders (EL_Rank < 2)
data_clean_matched <- data_clean %>% 
   pivot_longer(cols = matches("HLA"),
                values_to = "Allele") %>% 
   select(-name) %>% 
   left_join(x = .,
             y = pMHC_clean, 
             by = c("Peptide", "Allele")) %>% 
   filter(EL_Rank <= 2) %>% 
   distinct() %>% 
   select(-Experiment, -EL_Rank) %>% 
   mutate(Binding = 1)


## Create non-binders by mismatching CDR3b with peptide and corresponding allele
set.seed(99)
non_binders <- data_clean_matched %>% 
   select(CDR3b, 
          Peptide, 
          Allele) %>% 
   mutate(CDR3b = sample(CDR3b))

data_complete <- bind_rows(data_clean_matched, 
                           non_binders) %>% 
   distinct(., 
            across(-Binding), 
            .keep_all = TRUE) %>% 
   mutate(CDR3b_size = nchar(CDR3b)) %>% 
   replace_na(list(Binding = 0))

# Subset only HLA-A*02:01
data_complete_A0201 <- data_complete %>% 
   filter(Allele == "A*02:01") %>% 
   select(-Allele)

## View number of unique peptides and CDR3b sequences
data_complete_A0201 %>% distinct(Peptide)
data_complete_A0201 %>% distinct(CDR3b)

# Write data --------------------------------------------------------------
write_tsv(x = data_complete_A0201,
          file = "data/03_data_complete_A0201.tsv.gz")
