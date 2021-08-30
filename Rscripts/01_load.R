# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------


# Load data ---------------------------------------------------------------
my_data <- read_tsv(file = ".tsv")


# Wrangle data ------------------------------------------------------------
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