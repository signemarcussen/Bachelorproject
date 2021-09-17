
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

