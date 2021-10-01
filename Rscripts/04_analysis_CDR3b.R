# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))


# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_complete <- read_tsv(file = "data/03_data_complete.tsv.gz")
blosum62_raw <- read.table(file = "data/_raw/BLOSUM62.txt", 
                           skip = 6)


# Wrangle data ------------------------------------------------------------
blosum62 <- blosum62_raw %>% 
      select(-c("B", "Z", "X", "X.")) %>% 
      slice(1:(n() - 4)) %>% 
      as.matrix()


## Define training/test set
set.seed(2005)
data_complete_Xy <- data_complete %>% 
      mutate(Set = sample(c("train", "test"),
                          size = nrow(.),
                          replace = TRUE,
                          prob = c(0.8, 0.2)))

## View binder and set distribution
data_complete_Xy %>% 
      count(Binding, Set)

# View the distrubution of CDR3B_length 
data_complete %>% count(CDR3b_size)


## Encode peptides and define training/test matrices
X_train <- data_complete_Xy %>% 
      filter(Set == "train",
             CDR3b_size == 13) %>% 
      pull(CDR3b)%>% 
      blosum_encoding(x = .,
                      m = blosum62)

X_test <- data_complete_Xy %>% 
      filter(Set == "test") %>% 
      pull(Peptide) %>% 
      blosum_encoding(x = .,
                      m = blosum62)
y_train <- data_complete_Xy %>% 
      filter(Set == "train") %>% 
      pull(Binding) %>% 
      to_categorical()
y_test <- data_complete_Xy %>% 
      filter(Set == "test") %>% 
      pull(Binding) %>% 
      to_categorical()


# Model data --------------------------------------------------------------


# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
