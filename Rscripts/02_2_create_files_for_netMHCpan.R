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
   select(Peptide) %>% 
   distinct()


## Make HLA file - Split in two files since netMHCpan can't handle
## too many alleles at one time
HLA_netMHCpan <- data_clean %>% 
   select(matches("HLA")) %>%
   pivot_longer(.,
                cols = everything(),
                values_to = "Allele",
                values_drop_na = TRUE) %>% 
   distinct(Allele) %>% 
   mutate(Allele = str_replace_all(Allele, "\\*", "") %>% 
             paste("HLA", ., sep = "-")) %>%
   # Remove alleles that netMHCpan don't recognize
   filter(str_detect(Allele,
                     "HLA-C04:09",
                     negate = TRUE))


n <- nrow(HLA_netMHCpan)

HLA_netMHCpan_1 <- HLA_netMHCpan %>% 
   slice(1 : round((n / 2))) %>% 
   t()
HLA_netMHCpan_2 <- HLA_netMHCpan %>% 
   slice(round((n / 2)) + 1 : n) %>% 
   t()


# Write data --------------------------------------------------------
write_tsv(peptides_netMHCpan,
          file = "data/peptides.pep",
          col_names = FALSE)
write.table(x = HLA_netMHCpan_1,
            file = "data/HLA_1.csv",
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)
write.table(x = HLA_netMHCpan_2,
            file = "data/HLA_2.csv",
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)
