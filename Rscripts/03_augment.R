# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))


# Load data ---------------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")

## You must run "create_files_for_netMHCpan.R" to obtain this file:
pMHC_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/_raw/pMHC_predictions.xls", 
                       sep = "\t",
                       header = TRUE)

# Wrangle data ------------------------------------------------------------

## Clean pMHC data 
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


## Match alleles in the two files and choose the strongest binder for each peptide
HLA_correct <- c()
for (row in 1:nrow(data_clean)) {
   alleles <- as.character(data_clean[row, 4:9])
   alleles_score <- subset(x = pMHC_clean, 
                           select = alleles)[row, ]
   HLA_correct <- append(HLA_correct, 
                         colnames(alleles_score)[max.col(alleles_score)])
   
}

data_complete <- data_clean %>% 
   select(CDR3b, Peptide) %>% 
   mutate(HLA = HLA_correct,
          Binding = 1) %>%    
   filter(str_length(Peptide) == 9)


## Create non-binders by mismatching CDR3b with peptide and corresponding HLA
set.seed(99)
non_binders <- data_complete %>% 
   select(CDR3b, 
          Peptide, 
          HLA) %>% 
   mutate(CDR3b = sample(CDR3b))

data_complete <- bind_rows(data_complete, 
                           non_binders) %>% 
   distinct(., 
            across(-Binding), 
            .keep_all = TRUE) %>% 
   mutate(CDR3b_size = nchar(CDR3b)) %>% 
   replace_na(list(Binding = 0))


# Write data --------------------------------------------------------------
write_tsv(x = data_complete,
          file = "data/03_data_complete.tsv.gz")