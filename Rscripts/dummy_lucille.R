# Work with subset
set.seed(1234)
data_clean <- data_clean %>% sample_n(50)

pMHC_clean_longer <- pivot_longer(pMHC_clean, 
                                  cols = -Peptide, 
                                  names_to = "Alleles", 
                                  values_to = "EL_rank")

### Create non-binders by mismatching ### 

data_binder <- data.frame(x1 = 1:10, x2 = LETTERS[1:10], b = 1) %>% 
   as_tibble()

set.seed(9)
non_binder <- data %>% mutate( x1 = sample(x1))

combine <- bind_rows(data_binder, non_binder) %>% 
   distinct(combine, across(-b), .keep_all = TRUE)

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
data_complete <- data_clean %>% 
   select(CDR3b, Peptide) %>% 
   mutate(HLA = HLA_correct,
          Binding = 1) %>%    
   filter(str_length(Peptide) == 9)

set.seed(99)
non_binders <- data_complete %>% 
   select(CDR3b, Peptide, HLA) %>% 
   mutate( CDR3b = sample(CDR3b))

data_complete_combined <- bind_rows(data_complete, 
                     non_binders) %>% 
   distinct(., across(- Binding), 
            .keep_all = TRUE) %>% 
   mutate(CDR3b_size = nchar(CDR3b)) %>% 
   replace_na(list(Binding = 0))
