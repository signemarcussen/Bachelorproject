## Create fastafile of peptides
peptides_netMHCpan <- as.list(data_subset$Peptide) %>% 
      map(~strsplit(.x, "")) %>% 
      map(~unlist(.x)) %>% 
      map(~as.vector(.x)) %>% 
      write.fasta(., 
                  names = paste("seq", c(1:length(.)), sep = "_"),
                  file.out = "~/Bachelor/Bachelorproject/data/peptides.fa")