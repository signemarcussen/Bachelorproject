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
      sample_n(174)

# Wrangle data ------------------------------------------------------------

# One-hot encoding 

amino_acids = "LITSFANMPGKQYVHWDERC"
amino_acids_x = "LITSFANMPGKQYVHWDERCX"

## Pad short CDR3b sequences with "X" to same length
max_CDR3b <- data_A0201 %>% 
   select(CDR3b_size) %>% 
   max(.)

data_A0201 <- data_A0201 %>% 
   mutate(CDR3b = str_pad(string = CDR3b, 
                          width = max_CDR3b, 
                          side = "right", 
                          pad = "X"))

one_hot_pep <- data_A0201 %>% 
   pull(Peptide) %>% 
   one_hot_encoding(., amino_acids) %>%
   array_reshape(., c(nrow(data_A0201), 9, 20, 1))


one_hot_CDR3b <- data_A0201 %>% 
   pull(CDR3b) %>% 
   one_hot_encoding(., amino_acids_x) %>%
   array_reshape(., c(nrow(data_A0201), max_CDR3b, 21, 1))
   


# Model data --------------------------------------------------------------

# Write data --------------------------------------------------------------











