## Create fastafile of peptides
peptides_netMHCpan <- as.list(data_subset$Peptide) %>% 
      map(~strsplit(.x, "")) %>% 
      map(~unlist(.x)) %>% 
      map(~as.vector(.x)) %>% 
      write.fasta(., 
                  names = paste("seq", c(1:length(.)), sep = "_"),
                  file.out = "~/Bachelor/Bachelorproject/data/peptides.fa")

## Read pMHC_predictions in two files
allele_names_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/pMHC_predictions.xls", 
                               sep = "\t",
                               nrows = 1,
                               header = TRUE)
pMHC_raw <- read.table(file = "~/Bachelor/Bachelorproject/data/pMHC_predictions.xls", 
                       sep = "\t",
                       header = TRUE,
                       skip = 1)

## Leons PepTools package
install.packages("devtools")
devtools::install_github("leonjessen/PepTools")
library("PepTools")
PEPTIDES %>% pep_encode() %>% dim()

## Gamle matching af alleller
alleles_score <- subset(x = pMHC_clean, 
                        select = alleles)[row, ]


## Subset --------------

set.seed(1234)
data_subset <- data_clean %>% sample_n(50)

## Only peptides binding to HLA-A*02:01 ------------
pMHC_clean_A0201 <- pMHC_clean %>% 
   select(Peptide, `A*02:01`) %>% 
   mutate(`A*02:01` = as.numeric(as.character(`A*02:01`))) %>% 
   filter(`A*02:01` < 2)

## Match alleles in the two files and choose the strongest binder for each peptide
HLA_correct <- c()
for (row in 1:nrow(data_clean)) {
   
   peptide <- data_clean[row, "Peptide"]
   alleles <- as.character(data_clean[row, 4:9])
   
   alleles_score <- pMHC_clean %>% 
      filter(Peptide == as.character(peptide)) %>% 
      select(any_of(alleles))
   
   HLA_correct <- append(HLA_correct, 
                         colnames(alleles_score)[apply(alleles_score, 
                                                       1, 
                                                       which.min)])
}

## Create non-binder data
NB <- c()
names(NB) <- c("CDR3b", "Peptide", "Allele")
while (TRUE) {
   
   for (i in 1:nrow(data_clean_matched)) {
      new_CDR <- sample(data_clean_matched$CDR3b, 1)
      pep <- data_clean_matched$Peptide[i]
      allele <- data_clean_matched$Allele[i]
      sample(Peptide, Allele)
      if 
   }
   
   sample(CDR3b)
   
   
   
   if (nrow(NB) == nrow(data_clean_matched)) 
      return(NB)
}


while (FALSE) {
   cdr <- data_clean_matched %>% sample(1)
   if (cdr )
}


cdr3b <- data_clean_matched %>% pull(CDR3b)

for (i in 1:nrow(b)) {
   b$CDR3b[i] <- sample(cdr3b, 1)
   while (do.call(paste0, slice(b, i)) %in% do.call(paste0, data_clean_matched)) {
      b$CDR3b[i] <- sample(cdr3b, 1)
   }   
}

## Sequential CNN
cnn_model <- keras_model_sequential() %>% 
   layer_conv_1d(filters = 32,
                 kernel_size = 3,
                 activation = 'relu',
                 input_shape = input_shape_pep) %>%
   layer_max_pooling_1d(pool_size = 2) %>% 
   #layer_dropout(rate = 0.25) %>% 
   layer_flatten() %>% 
   layer_dense(units = 32, activation = "relu") %>% 
   layer_dense(units  = 1, activation   = 'sigmoid')


# ## View class distribution
# data_A0201 %>% 
#       ggplot(mapping = aes(Set,
#                            fill = Binding)) +
#       geom_bar(position = "dodge")











