# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))


# Load data ---------------------------------------------------------------
subject_metadata <- read_csv(file = "data/_raw/subject-metadata.csv")
peptide_detail_ci <- read_csv(file = "data/_raw/peptide-detail-ci.csv")
#peptide_detail_cii <- read_csv(file = "data/_raw/peptide-detail-cii.csv")
#minigene_detail <- read_csv(file = "data/_raw/minigene-detail.csv")
#orfs <- read_csv(file = "data/_raw/orfs.csv")
#peptide_hits_ci <- read_csv(file = "data/_raw/peptide-hits-ci.csv")
#peptide_hits_cii <- read_csv(file = "data/_raw/peptide-hits-cii.csv")
#minigene_hits <- read_csv(file = "data/_raw/minigene-hits.csv")


# Wrangle data ------------------------------------------------------------
## Join the two data files
data_raw_combined <- peptide_detail_ci %>% 
   full_join(x = .,
             y = subject_metadata,
             by = "Experiment")


# Write data --------------------------------------------------------------
write_tsv(x = data_raw_combined,
          file = "data/01_data_raw_combined.tsv.gz")