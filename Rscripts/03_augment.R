# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))


# Load data ---------------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")
pMHC_clean <- read_tsv(file = "data/02_pMHC_clean.tsv.gz")

# Wrangle data ------------------------------------------------------------
# Match alleles in the two files and choose the strongest binder for each peptide
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
   mutate(HLA = HLA_correct) %>% 
   mutate(Binding = 1) %>% 
   mutate(CDR3b_size = nchar(CDR3b)) %>%    
   filter(str_length(Peptide) == 9)

# Write data --------------------------------------------------------------
write_tsv(x = data_complete,
          file = "data/03_data_complete.tsv.gz")