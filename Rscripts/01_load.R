# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))


# Load data ---------------------------------------------------------------
metadata_raw <- read_csv(file = "data/_raw/subject-metadata.csv")
peptidedata_raw <- read_csv(file = "data/_raw/peptide-detail-ci.csv")
pMHC_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/_raw/pMHC_predictions.xls", 
                       sep = "\t",
                       header = TRUE)

# Wrangle data ------------------------------------------------------------
## Join metadata and peptidedata
data_raw_combined <- peptidedata_raw %>% 
   full_join(x = .,
             y = metadata_raw,
             by = "Experiment")

# Write data --------------------------------------------------------------
write_tsv(x = data_raw_combined,
          file = "data/01_data_raw_combined.tsv.gz")
write_tsv(x = pMHC_raw,
          file = "data/01_pMHC_raw.tsv.gz")
