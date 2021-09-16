# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------


# Load data ---------------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------
## Run netMHcpan.R script before this:
data_complete <- data_clean %>% 
   select(CDR3b, Peptide) %>% 
   mutate(HLA = HLA_correct)
   
   #mutate(Binding = 1) %>% 
   #mutate(CDR3b_size = nchar(CDR3b))

# Write data --------------------------------------------------------------
write_tsv(x = data_complete,
          file = "data/03_data_complete.tsv.gz")