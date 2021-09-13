# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


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

## Combine peptide C1 and C2 experiments, and join subject_metadata
data_raw <- peptide_detail_ci %>% 
   full_join(x = .,
             y = subject_metadata,
             by = "Experiment") %>% 
   select("Experiment",
          "TCR BioIdentity", 
          "Amino Acids", 
          matches("HLA"))


HLA_X <- c("HLA-A","HLA-A_1","HLA-B","HLA-B_1","HLA-C","HLA-C_1")
data_clean <- data_raw %>% 
   drop_na() %>% 
   rename(Peptide = `Amino Acids`) %>% 
   mutate(Peptide = strsplit(Peptide, ",")) %>% 
   unnest(Peptide) %>%  
   separate(col = `TCR BioIdentity`,
            into = "CDR3b",
            extra = "drop") #%>%
   #str_sub(data_raw[HLA_X], start = 1, end = 7)
   #mutate(Binding = 1) %>% 
   #mutate(CDR3b_size = nchar(CDR3b))

## Create subset
set.seed(1234)   
data_subset <- data_clean %>% sample_n(50)


## Make list of peptide sequences for NetMHCpan
peptides_netMHCpan <- data_subset %>% 
   select(Peptide) %>% 
   write_tsv(.,
             file = "~/Bachelor/Bachelorproject/data/peptides.pep",
             col_names = FALSE)


## Make HLA file for NetMHCpan
HLA_netMHCpan <- data_subset %>% 
   select(matches("HLA")) %>%
   pivot_longer(.,
                cols = everything(),
                values_to = "Allele",
                values_drop_na = TRUE) %>% 
   mutate(Allele = str_sub(Allele, 
                           start = 1, 
                           end = 7)) %>%
   distinct(Allele) %>% 
   mutate(Allele = str_replace_all(Allele, "\\*", "") %>% 
                   paste("HLA", ., sep = "-")) %>% 
   t() %>% 
   write.table(x = .,
               file = "~/Bachelor/Bachelorproject/data/HLA.csv",
               sep = ",",
               col.names = FALSE,
               row.names = FALSE,
               quote = FALSE)



# Write data --------------------------------------------------------------

ggsave(filename = ".png",
       plot = ,
       width = ,
       height = )
write_tsv(x = my_data_subset,
          file = "path/to/my/data_subset.tsv.gz")