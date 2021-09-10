## Selecting peptide-seq only containing the 20 amino acids 
library(sjmisc)
library(tidyverse)
library(stringr)
a1<-("AXSTGHDF")
a11<- c("A", "X", "S", "T", "G", "H", "D", "F")
a2<-("SLIFSYV")
peptide <- c(a1, a2)
# pep_split <- str_split(peptide, "") returs a list of vectors w peptide seq split up
# pep_split[[1]] is the first seq
# str_detect(amino_acids_1,pep_split[[1]]) #returns T, F, T, T ... 

data_subset <- tail(data_subset) %>% add_row(Peptide = a1)

amino_acids <- c("L","I","T","S","F","A","N","M","P","G","K","Q","Y","V","H","W","D","E","R","C")
amino_acids_1 <- ("LITSFANMPGKQYVHWDERC")

#Input function
peptides <- data_subset$Peptide

split <- c()
protein_aa <- c()
results <- c()

for (i in 1:length(peptides)) {
      split <- str_split(peptides[i],"") # list of splitted aa-seqs 
      for (ii in 1:length(split)) {
            protein_aa <- all(str_detect(amino_acids_1, split[[ii]]))#Returns TRUE, if all elements are TRUE
            
      }
      #print(split)
      #print(protein_aa)
      
      results <- rbind(results, c(peptides[i],protein_aa))
      #print(results)
}


colnames(results) <- c("peptide", "result")

results <- results %>% 
      as_tibble()
results