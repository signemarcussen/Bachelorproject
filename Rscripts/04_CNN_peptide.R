# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("keras")
suppressWarnings(library("tidyverse"))
#library("tensorflow")
#library("reticulate")
suppressWarnings(library(PepTools))


# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_A0201 <- read_tsv(file = "data/03_data_complete_A0201.tsv.gz")
blosum62_raw <- read.table(file = "data/_raw/BLOSUM62.txt", 
                           skip = 6)


# Wrangle data ------------------------------------------------------------
blosum62 <- blosum62_raw %>% 
      select(-c("B", "Z", "X", "X.")) %>% 
      slice(1:(n() - 4)) %>% 
      as.matrix()
      

## Define training/test set
set.seed(2005)
data_A0201_Xy <- data_A0201 %>% 
      mutate(Set = sample(c("train", "test"),
                          size = nrow(.),
                          replace = TRUE,
                          prob = c(0.8, 0.2)))

## View binder and set distribution
data_A0201_Xy %>% 
      count(Binding, Set)

## Encode peptides and define training/test
X_train <- data_A0201_Xy %>% 
   filter(Set == "train") %>% 
   pull(Peptide) %>% 
   pep_encode_Signe(pep = ., matrix = blosum62) %>% 
   array_reshape(., c(nrow(.), 9, 20, 1))

X_test <- data_A0201_Xy %>% 
   filter(Set == "test") %>% 
   pull(Peptide) %>% 
   pep_encode_Signe(pep = ., matrix = blosum62) %>% 
   array_reshape(., c(nrow(.), 9, 20, 1))

y_train <- data_A0201_Xy %>% 
   filter(Set == "train") %>% 
    pull(Binding)

y_test <- data_A0201_Xy %>% 
   filter(Set == "test") %>% 
   pull(Binding)


# Model data --------------------------------------------------------------

## Set hyperparameters
n_epochs <- 100 #300 / 50
batch_size <- 50
#batch_size <- 158402 #nrow(yy_train)
loss_func <- "binary_crossentropy"
learn_rate <- 0.001
input_shape <- c(9, 20, 1)

## Build model architecture
cnn_model <- keras_model_sequential() %>% 
   layer_conv_2d(filters = 32,
                 kernel_size = c(3, 3),
                 activation = 'relu',
                 input_shape = input_shape) %>% 
   layer_dropout(rate = 0.25) %>% 
   layer_flatten() %>% 
   #layer_dense(units  = 20, activation = 'relu') %>% 
   layer_dense(units  = 180, activation = 'relu') %>% 
   layer_dropout(rate = 0.4) %>% 
   #layer_dense(units  = 10, activation  = 'relu') %>%
   layer_dense(units  = 90, activation  = 'relu') %>%
   layer_dropout(rate = 0.3) %>%
   layer_dense(units  = 1, activation   = 'sigmoid')
    
## Compile model
cnn_model %>% 
   compile(loss = 'binary_crossentropy',
           optimizer = "adam",
           metrics = "accuracy")

## View model
cnn_model %>% summary()


## Train model
cnn_history <- cnn_model %>% 
   fit(x = X_train,
       y = y_train,
       epochs = n_epochs,
       batch_size = batch_size,
       validation_split = 0.2)


# ## Evaluate model
# performance_test <- model %>% 
#    evaluate(X_test, y_test)
# accuracy_test <- performance_test %>% 
#    pluck("accuracy") %>% 
#    round(3) * 100
# 
# performance_train <- model %>% 
#    evaluate(X_train, y_train)
# accuracy_train <- performance_train %>% 
#    pluck("accuracy") %>% 
#    round(3) * 100



# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
