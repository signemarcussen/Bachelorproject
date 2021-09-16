## Analyse output from NetMHCpan
library(readxl)


# Create files for netMHCpan software --------------------------------
## Make list of peptide sequences for NetMHCpan
peptides_netMHCpan <- data_clean %>% 
   select(Peptide) %>% 
   write_tsv(.,
             file = "~/Bachelor/Bachelorproject/data/peptides.pep",
             col_names = FALSE)


## Make HLA file for NetMHCpan
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
   mutate(Allele = str_replace_all(Allele, "\\*", "") %>% 
             paste("HLA", ., sep = "-")) %>% 
   t() %>% 
   write.table(x = .,
               file = "~/Bachelor/Bachelorproject/data/HLA.csv",
               sep = ",",
               col.names = FALSE,
               row.names = FALSE,
               quote = FALSE)

#####################
### Run in Ubuntu ###
#####################

# Load data
allele_names_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/pMHC_predictions.xls", 
                               sep = "\t",
                               nrows = 1,
                               header = TRUE) %>% 
                    names()
      
pMHC_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/pMHC_predictions.xls", sep = "\t",
                       header = TRUE,
                       skip = 1)

# Clean data
col_names <- str_subset(allele_names_raw, "HLA") %>% 
      str_replace("HLA.", "") %>%
      str_replace("\\.", "\\:") %>% 
      gsub(pattern = "^(.{1})(.*)$",        
           replacement = "\\1*\\2",
           x = .) %>% 
      append("Peptide", 
             after = 0) 

pMHC_clean <- as_tibble(pMHC_raw) %>% 
      select("Peptide",
             matches("EL_Rank")) %>% 
      rename_at(vars(everything()), 
                function(x) col_names)

HLA_correct <- c()
for (row in 1:nrow(data_clean)) {
      alleles <- as.character(data_clean[row, 4:9])
      alleles_score <- subset(x = pMHC_clean, 
                              select = alleles)[row, ]
      HLA_correct <- append(HLA_correct, 
                            colnames(alleles_score)[max.col(alleles_score)])
      
}
