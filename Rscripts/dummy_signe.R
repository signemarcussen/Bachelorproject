## Create fastafile of peptides
peptides_netMHCpan <- as.list(data_subset$Peptide) %>% 
      map(~strsplit(.x, "")) %>% 
      map(~unlist(.x)) %>% 
      map(~as.vector(.x)) %>% 
      write.fasta(., 
                  names = paste("seq", c(1:length(.)), sep = "_"),
                  file.out = "~/Bachelor/Bachelorproject/data/peptides.fa")

## Read pMHC_predictions in two files
allele_names_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/pMHC_predictions.xls", 
                               sep = "\t",
                               nrows = 1,
                               header = TRUE)
pMHC_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/pMHC_predictions.xls", 
                       sep = "\t",
                       header = TRUE,
                       skip = 1)

## Leons PepTools package
install.packages("devtools")
devtools::install_github("leonjessen/PepTools")
library("PepTools")
PEPTIDES %>% pep_encode() %>% dim()

## Gamle matching af alleller
alleles_score <- subset(x = pMHC_clean, 
                        select = alleles)[row, ]

