
### Create non-binders by mismatching ### 

data <- data.frame(x1 = 1:10, x2 = LETTERS[1:10]) %>% 
   as_tibble()

data %>% mutate( x1 = sample(x1))

#brug distint across alt andet end binding. c(-binding)
distinct(starwars, across(contains("color")))