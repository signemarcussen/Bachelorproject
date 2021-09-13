## Selecting peptide-seq only containing the 20 Proteinogenic amino acids
library(sjmisc)
library(tidyverse)
library(stringr)
a1<-("AXSTGHDF")
#a11<- c("A", "X", "S", "T", "G", "H", "D", "F")
a2<-("SLIFSYVU")
#peptide <- c(a1, a2)
# pep_split <- str_split(peptide, "") returs a list of vectors w peptide seq split up
# pep_split[[1]] is the first seq
# str_detect(amino_acids_1,pep_split[[1]]) #returns T, F, T, T ... 

data_subset <- data_subset %>% 
   add_row(Experiment = "A1", Peptide = a1) %>% 
   add_row(Experiment = "A2", Peptide = a2)

#amino_acids <- c("L","I","T","S","F","A","N","M","P","G","K","Q","Y","V","H","W","D","E","R","C")
amino_acids <- ("LITSFANMPGKQYVHWDERC")

#Input function
peptides <- data_subset$Peptide
#peptides <- data_clean$Peptide

split <- c()
protein_aa <- c()
results <- c()

for (i in 1:length(peptides)) {
      split <- str_split(peptides[i],"") # list of splited aa-seqs 
      for (ii in 1:length(split)) {
            protein_aa <- all(str_detect(amino_acids, split[[ii]]))#Returns TRUE, if all elements are TRUE
            
      }
      results <- rbind(results, c(peptides[i],protein_aa))
}
# Changing format of results-column
colnames(results) <- c("Peptide", "Proteinogenic")
results <- results %>% 
      as_tibble()

#Joining and sorting the data 
data_subset_1 <- full_join(data_subset,results, by = "Peptide")


######################################
data<-data_subset
non_proteinogenic_aa <- function(data) {
   
   amino_acids <- ("LITSFANMPGKQYVHWDERC")
   #Input function
   peptides <- data$Peptide
   
   split <- c()
   protein_aa <- c()
   results <- c()
   
   for (i in 1:length(peptides)) {
      split <- str_split(peptides[i],"") # list of splited aa-seqs 
      
      for (ii in 1:length(split)) {
         protein_aa <- all(str_detect(amino_acids, 
                                      split[[ii]]))#Returns TRUE, if all elements are TRUE
         
      }
      results <- rbind(results, c(peptides[i],protein_aa))
   }
   # Changing format of results-column
   colnames(results) <- c("Peptide", "Proteinogenic")
   results <- results %>% 
      as_tibble()
   
   #Joining and sorting the data 
   data <- full_join(data,results, by = "Peptide")
   
   non_proteinogenic <- data %>% 
      filter( Proteinogenic == "FALSE")
   
   return(non_proteinogenic)
}
