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
one_hot_encoding <- function(sequence) {
   
   amino_acids = "LITSFANMPGKQYVHWDERC"
   
   # Define encoding matrix
   m = nchar(amino_acids)
   n = nchar(amino_acids)
   enc = diag(1, m, n)
   
   #enc1 = array(diag(1, m, n), dim = c(m,n))
   dimnames(enc) = c(strsplit(amino_acids, ""),
                     strsplit(amino_acids, ""))
   
   # Encode the sequence 
   x_enc = enc[unlist(strsplit(sequence, "")),]
   # x_enc = array(x_enc) %>% 
   #    array_reshape(., c(9, 20,1))
   return(x_enc)
   
}

encode <- data_A0201 %>% 
   pull(Peptide) %>% 
   one_hot_encoding(.)
encode

bl_encode <- data_A0201 %>% pull(Peptide) %>% 
   blosum_encoding(peptide = ., 
                   blosum_matrix = blosum62) %>% 
   array_reshape(., c(nrow(.), 9, 20, 1))

%>% 
   array_reshape(., c(nrow(.), 9, 20, 1))
#nrow = antal peptider. ?

# Model data --------------------------------------------------------------

# Write data --------------------------------------------------------------











