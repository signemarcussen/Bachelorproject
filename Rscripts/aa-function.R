## Selecting peptide-seq only containing the 20 Proteinogenic amino acids
library(sjmisc)
library(tidyverse)
library(stringr)

# test data
a1<-("AXSTGHDF")
a2<-("SLIFSYVU")
#a11<- c("A", "X", "S", "T", "G", "H", "D", "F")

data_subset <- data_clean %>% 
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

non_proteinogenic_aa <- function(data) {
   
   amino_acids <- ("LITSFANMPGKQYVHWDERC")
   #Input 
   peptides <- data$Peptide
   
   split <- c()
   protein_aa <- c()
   results <- c()
   
   for (i in 1:length(peptides)) {
      split <- str_split(peptides[i],"") # list of split aa-seqs 
      
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
   non_proteinogenic <- full_join(data,results, 
                                  by = "Peptide") %>%
      filter( Proteinogenic == "FALSE")
   
   return(non_proteinogenic)
}
