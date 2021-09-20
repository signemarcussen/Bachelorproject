# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))


# Load data ---------------------------------------------------------------
metadata_raw <- read_csv(file = "data/_raw/subject-metadata.csv")
peptidedata_raw <- read_csv(file = "data/_raw/peptide-detail-ci.csv")


# Wrangle data ------------------------------------------------------------
data_raw_combined <- peptidedata_raw %>% 
   full_join(x = .,
             y = metadata_raw,
             by = "Experiment")


# Write data --------------------------------------------------------------
write_tsv(x = data_raw_combined,
          file = "data/01_data_raw_combined.tsv.gz")