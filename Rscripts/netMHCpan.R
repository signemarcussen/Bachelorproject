## Analyse output from NetMHCpan
library(readxl)

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
for (row in 1:nrow(data_subset)) {
      alleles <- as.character(data_subset[row, 4:9])
      alleles_score <- subset(x = pMHC_clean, 
                              select = alleles)[row, ]
      HLA_correct <- append(HLA_correct, 
                            colnames(alleles_score)[max.col(alleles_score)])
      
}

mutate(HLA = HLA_correct)
