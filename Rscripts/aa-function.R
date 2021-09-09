## Selecting peptide-seq only containing the 20 amino acids 
library(sjmisc)
a1<-("AXSTGHDF")
a2<-("SLIFSYV")
peptide <- c(a1, a2)

data_subset <- data_subset %>% add_row(Peptide = a1)

amino_acids <- c("L","I","T","S","F","A","N","M","P","G","K","Q","Y","V","H","W","D","E","R","C")
amino_acids_1 <- ("LITSFANMPGKQYVHWDERC")
data_subset_1 <- data_subset %>% 
      rowwise %>%  
