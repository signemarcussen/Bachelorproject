# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("keras")
suppressWarnings(library("PepTools"))


# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")


# Wrangle data ------------------------------------------------------------


# Model data --------------------------------------------------------------


# Write data --------------------------------------------------------------