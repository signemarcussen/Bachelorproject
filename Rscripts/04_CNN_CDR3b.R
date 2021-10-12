# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library(keras)
library(PepTools)

# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_A0201 <- read_tsv(file = "data/03_data_complete_A0201.tsv.gz")
blosum62_raw <- read.table(file = "data/_raw/BLOSUM62.txt", 
                           skip = 6)


# Wrangle data ------------------------------------------------------------
blosum62 <- blosum62_raw %>% 
      select(-c("B", "Z", "X", "X.")) %>% 
      slice(1:(n() - 4)) %>% 
      mutate( X = 0) %>% 
      as.matrix()

X <- rep(0,21)
blosum62 <- rbind(blosum62, X)

## Define training/test set
set.seed(2005)
data_A0201_Xy <- data_A0201 %>% 
      mutate(Set = sample(c("train", "test"),
                          size = nrow(.),
                          replace = TRUE,
                          prob = c(0.8, 0.2))) %>% 
   drop_na()

#Padding short sequences with "X"
pad_data_A0201_Xy <- data_A0201_Xy %>% 
   mutate(CDR3b = str_pad(string = CDR3b, 
                          width = max(nchar(CDR3b)), 
                          side="right", pad="X"))

## View binder and set distribution
data_A0201_Xy %>% 
      count(Binding, Set)

## Encode peptides and define training/test matrices
### Using pep_encode, to create 3D tensor of the CDR3b sequences
### with dimensions n_peps x l_peps x l_enc, this is converted to a 1D array. 
### so the first l_peps x l_enc entries correspond to one seq.

X_train <- pad_data_A0201_Xy %>% 
      filter(Set == "train") %>% 
      pull(CDR3b)%>% 
      pep_encode(pep = .) %>% 
      array_reshape(., c(nrow(.), 25, 21, 1))
#Before reshape: 
#dim(X_train) = 158396 -seqs     25-lenght of seqs    21-cols in Blosum62

X_test <- data_A0201_Xy %>% 
   filter(Set == "test") %>% 
   pull(Peptide) %>% 
   pep_encode_Signe(pep = ., matrix = blosum62) %>% 
   array_reshape(., c(nrow(.), 9, 20, 1))

X_test <- data_A0201_Xy %>% 
      filter(Set == "test") %>% 
      pull(CDR3b) %>% 
      blosum_encoding(x = .,
                      m = blosum62)

y_train <- data_A0201_Xy %>% 
      filter(Set == "train") %>% 
      pull(Binding)

y_test <- data_A0201_Xy %>% 
      filter(Set == "test") %>% 
      pull(Binding) 


# Model data --------------------------------------------------------------


# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
