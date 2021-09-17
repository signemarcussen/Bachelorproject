# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
blosum62_raw <- read.table(file = "data/_raw/BLOSUM62.txt", 
                           skip = 6)


# Wrangle data ------------------------------------------------------------
blosum62 <- blosum62_raw %>% 
   select(-c("B", "Z", "X", "X.")) %>% 
   slice(1:(n() - 4))

peptides <- data_complete %>% 
   pull(Peptide)


# Model data --------------------------------------------------------------
blosum_encoding(x = peptides,
                m = blosum62)



# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
