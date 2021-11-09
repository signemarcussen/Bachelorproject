# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("dplyr")
library("keras")
suppressWarnings(library("PepTools"))


# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")

## Subset
set.seed(2005)
data_A0201 <- data_A0201 %>% # SUBSET
      sample_n(10)

# Wrangle data ------------------------------------------------------------
# One-hot encoding 
X_pep <- data_A0201 %>%  
      pull(Peptide)

aa <- c('L','I','T','S','F','A','N','M','P','G','K','Q','Y','V','H','W','D','E','R','C')
# Model data --------------------------------------------------------------


# Write data --------------------------------------------------------------