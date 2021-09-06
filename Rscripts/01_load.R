# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("seqinr")

# Define functions --------------------------------------------------------


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

# Combine peptide C1 and C2 experiments, and join subject_metadata
raw_data <- peptide_detail_ci %>% 
   full_join(x = .,
             y = subject_metadata,
             by = "Experiment") %>% 
   select("Experiment",
          "TCR BioIdentity", 
          "Amino Acids", 
          matches("HLA"))

data_clean <- raw_data %>% 
   rename(Peptide = `Amino Acids`) %>% 
   mutate(Peptide = strsplit(Peptide, ",")) %>% 
   unnest(Peptide) %>%  
   separate(col = `TCR BioIdentity`,
            into = "CDR3b",
            extra = "drop") %>% 
   mutate(Binding = 1) %>% 
   mutate(CDR3b_size = nchar(CDR3b))

## Create subset
set.seed(1234)   
data_subset <- data_clean %>% sample_n(50)


## Make fasta file of peptide sequences
peptides_fasta <- as.list(data_subset$Peptide) %>% 
   map(~strsplit(.x, "")) %>% 
   map(~unlist(.x)) %>% 
   map(~as.vector(.x)) %>% 
   write.fasta(., 
               names = paste("seq", c(1:length(peptides_fasta)), sep = "_"),
               file.out = "peptides.fa")



# Write data --------------------------------------------------------------

ggsave(filename = ".png",
       plot = ,
       width = ,
       height = )
write_tsv(x = my_data_subset,
          file = "path/to/my/data_subset.tsv.gz")