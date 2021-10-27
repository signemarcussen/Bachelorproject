library(tidyverse)
# Work with subset
set.seed(1234)
#data_clean <- data_clean %>% sample_n(50)

### Create non-binders by mismatching ### 

data_binder <- data.frame(CDR3b = LETTERS[1:10], 
                          Peptide = LETTERS[1:10],
                          Allele = LETTERS[1:10],
                          Binding = 1) %>% 
   as_tibble() %>% mutate(across(where(is.factor), as.character))

data_binder

vec <- c("G","A","A")
vec1 <- c("A","A","A")
names(vec) <- c("CDR3b", "Peptide", "Allele")


non_binder <- c()
data_binder1 <- data_binder %>% 
   select(-Binding)
nRow <- nrow(data_binder1)

for (i in 1:nRow) {
   
   obs <- c(sample(data_binder$CDR3b,1), 
            data_binder$Peptide[i],
            data_binder$Allele[i])
   
   for (j in 1:nRow) {
     while (obs==) {
        #While stopper når condition er FALSE
        
     }
      
   }
   
}

print(non_binder)

#brug distint across alt andet end binding. c(-binding)
#distinct(starwars, across(contains("color")))

data_complete1 <- data_complete %>%  
   add_row(CDR3b = "AA", Peptide = "AA", HLA ="AA", Binding = 1, CDR3b_size =1)

set.seed(99)
non_binder <- data_complete %>% 
   select(CDR3b, Peptide, HLA) %>% 
   mutate( CDR3b = sample(CDR3b)) %>% 
   add_row(CDR3b = "AA", Peptide = "AA", HLA ="AA")

combine <- bind_rows(data_complete1, 
                     non_binder) %>% 
   distinct(., across(- c(Binding,CDR3b_size)), 
            .keep_all = TRUE) %>% 
   mutate(CDR3b_size = nchar(CDR3b)) #%>% 
   #replace_na(select("Binding"), value = 0)
###############################################################
data_complete <- data_A0201 %>% 
   #select(CDR3b, Peptide) %>% 
   mutate(Binding = 1) %>%    
   filter(str_length(Peptide) == 9)

set.seed(100)
non_binders <- data_complete %>% 
   select(CDR3b, Peptide) %>% 
   mutate( CDR3b = sample(CDR3b))

data_complete_combined <- bind_rows(data_complete, 
                     non_binders) %>% 
   distinct(., across(- Binding), 
            .keep_all = TRUE) %>% 
   mutate(CDR3b_size = nchar(CDR3b)) %>% 
   replace_na(list(Binding = 0))

#number of duplicates including allele
#289523*2-560640
#[1] 18406
# number of duplicates using data_A0201
#105312*2 - nrow(data_complete_combined)
#[1] 24725