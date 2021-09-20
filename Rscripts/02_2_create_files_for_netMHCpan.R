########################################################################
# Create files for netMHCpan software -------------------------------- #
########################################################################

## NetMHCpan requires a .pep file of peptides, and a .csv file of HLA alleles.
## 1) Install software NetMHCpan - 4.1 in Ubuntu.
## 2) Create the two inputfiles with this script
## 3) Place yourself and the files in netMHCpan-4.1/<own_project> directory
## 4) Run the following command in Ubuntu/Unix:
## ../netMHCpan -p peptides.pep -xls -a `cat HLA.csv` -xlsfile pMHC_predictions.xls

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries -----------------------------------------------------
library("tidyverse")
library(readxl)


# Load data ----------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")


# Wrangle data -------------------------------------------------------
## Make peptide file
peptides_netMHCpan <- data_clean %>% 
   select(Peptide)


## Make HLA file
HLA_netMHCpan <- data_clean %>% 
   select(matches("HLA")) %>%
   pivot_longer(.,
                cols = everything(),
                values_to = "Allele",
                values_drop_na = TRUE) %>% 
   mutate(Allele = str_sub(Allele, 
                           start = 1, 
                           end = 7)) %>%
   distinct(Allele) %>% 
   #slice(1:93) %>% dette er maks størrelse
   mutate(Allele = str_replace_all(Allele, "\\*", "") %>% 
             paste("HLA", ., sep = "-")) %>% 
   t()
   

# Write data --------------------------------------------------------
write_tsv(peptides_netMHCpan,
          file = "~/Bachelor/Bachelorproject/data/peptides.pep",
          col_names = FALSE)
write.table(x = HLA_netMHCpan,
            file = "~/Bachelor/Bachelorproject/data/HLA.csv",
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)
