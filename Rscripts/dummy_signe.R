## Create fastafile of peptides
peptides_netMHCpan <- as.list(data_subset$Peptide) %>% 
      map(~strsplit(.x, "")) %>% 
      map(~unlist(.x)) %>% 
      map(~as.vector(.x)) %>% 
      write.fasta(., 
                  names = paste("seq", c(1:length(.)), sep = "_"),
                  file.out = "~/Bachelor/Bachelorproject/data/peptides.fa")

## Read pMHC_predictions in two files
pMHC_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/pMHC_predictions.xls", 
                       sep = "\t",
                       header = TRUE,
                       skip = 1)


pMHP_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/_raw/pMHC_predictions.xls", 
                       sep = "\t",
                       header = TRUE)
