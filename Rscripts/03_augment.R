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
   select(-Experiment, -EL_Rank)

# Remove duplicates?
data_clean_matched <- data_clean_matched %>% 
   distinct()

## Subset only HLA-A*02:01
data_A0201 <- data_clean_matched %>% 
   filter(Allele == "A*02:01") %>% 
   select(-Allele)

## Create non-binders by mismatching CDR3b with peptide and corresponding allele
all_CDR3b <- data_clean_matched %>% pull(CDR3b)

set.seed(100)
non_binders <- data_A0201 %>%
   mutate(CDR3b = sample(all_CDR3b, 
                         size = nrow(data_A0201)))

duplicates <- inner_join(data_A0201,
                         non_binders,
                         by = c("CDR3b", "Peptide"))
# Keep only correct mismatches
non_binders <- anti_join(non_binders,
                         data_A0201)

# Make new and unique combinations of all duplicates
all_unique <- bind_rows(data_A0201,
                        non_binders)
set.seed(100)
for (i in 1:nrow(duplicates)) {
   
   duplicates$CDR3b[i] <- sample(all_CDR3b, 1)
   
   while (do.call(paste0, slice(duplicates, i)) %in% do.call(paste0, all_unique)) {
      
      duplicates$CDR3b[i] <- sample(all_CDR3b, 1)
      
   } 
   
   print(paste0(i, "/", nrow(duplicates)))
   
}


## Complete data
data_A0201 <- data_A0201 %>% 
   mutate(Binding = 1)

data_A0201_complete <- bind_rows(data_A0201, 
                                 non_binders,
                                 duplicates) %>% 
   mutate(CDR3b_size = nchar(CDR3b)) %>% 
   replace_na(list(Binding = 0))


## View number of unique peptides and CDR3b sequences
data_A0201_complete %>% distinct(Peptide)
data_A0201_complete %>% distinct(CDR3b)

data_A0201_complete %>%  distinct(., across(- c(Binding,CDR3b_size)))
#222216-206693
#[1] 15523 duplicates 

# Write data --------------------------------------------------------------
write_tsv(x = data_A0201_complete,
          file = "data/03_data_A0201_complete.tsv.gz")
