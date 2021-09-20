# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
blosum62_raw <- read.table(file = "data/_raw/BLOSUM62.txt", 
                           skip = 6)


# Wrangle data ------------------------------------------------------------
blosum62 <- blosum62_raw %>% 
   select(-c("B", "Z", "X", "X.")) %>% 
   slice(1:(n() - 4))


## Define training and test set
set.seed(2005)
data_complete_Xy <- data_complete %>% 
      mutate(Set = sample(c("train", "test"),
                          size = nrow(.),
                          replace = TRUE,
                          prob = c(0.8, 0.2)))

## View binder and set distribution
data_complete_Xy %>% 
      count(Binding, Set)

# Model data --------------------------------------------------------------




blosum_encoding(x = peptides,
                m = blosum62)



# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
