# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------


# Load data ---------------------------------------------------------------
subject_metadata <- read_csv(file = "data/_raw/subject-metadata.csv")
peptide_detail_ci <- read_csv(file = "data/_raw/peptide-detail-ci.csv")
peptide_detail_cii <- read_csv(file = "data/_raw/peptide-detail-cii.csv")
minigene_detail <- read_csv(file = "data/_raw/minigene-detail.csv")
orfs <- read_csv(file = "data/_raw/orfs.csv")
peptide_hits_ci <- read_csv(file = "data/_raw/peptide-hits-ci.csv")
peptide_hits_cii <- read_csv(file = "data/_raw/peptide-hits-cii.csv")
minigene_hits <- read_csv(file = "data/_raw/minigene-hits.csv")


# Wrangle data ------------------------------------------------------------

# Combine peptide C1 and C2 experiments, and join subject_metadata
dat <- rbind(peptide_detail_ci, 
             peptide_detail_cii) %>% 
      full_join(x = ., 
                y = subject_metadata, 
                by = "Experiment")



my_data_subset <- my_data %>% 
      filter(...) %>% 
      select(...) %>% 
      mutate(...) %>% 
      arrange(...)


# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
ggsave(filename = ".png",
       plot = ,
       width = ,
       height = )
write_tsv(x = my_data_subset,
          file = "path/to/my/data_subset.tsv.gz")