# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("tensorflow")
library("keras")
library("reticulate")


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

## Encode peptides and define training/test matrices
X_train <- data_A0201_Xy %>% 
      filter(Set == "train") %>% 
      pull(Peptide) %>% 
      blosum_encoding(x = .,
                      m = blosum62)
X_test <- data_A0201_Xy %>% 
      filter(Set == "test") %>% 
      pull(Peptide) %>% 
      blosum_encoding(x = .,
                      m = blosum62)
y_train <- data_A0201_Xy %>% 
      filter(Set == "train") %>% 
      pull(Binding)
y_test <- data_A0201_Xy %>% 
      filter(Set == "test") %>% 
      pull(Binding)



# Model data --------------------------------------------------------------

## Set hyperparameters
n_epochs <- 15 #300 / 50
batch_size <- 128
loss_func <- "binary_crossentropy"
learn_rate <- 0.001
input_shape <- c(9, 20)

## Set model structure
cnn_model <- keras_model_sequential() %>% 
   layer_conv_1d(filters = c(16),
                 kernel_size = c(3),
                 activation = "sigmoid",
                 input_shape = c(9, 20)) #%>% 
   #layer_max_pooling_1d(pool_size = 2)
    
## Compile model
cnn_model %>% 
   compile(loss = loss_func,
           optimizer = optimizer_adam(learning_rate = learn_rate),
           metrics = c("accuracy"))

## View model
cnn_model %>% summary()


## Train model
cnn_history <- cnn_model %>% 
   fit(X_train,
       y_train,
       batch_size = batch_size,
       epochs = n_epochs,
       validation_split = 0.2)


## Evaluate model
performance_test <- model %>% 
   evaluate(X_test, y_test)
accuracy_test <- performance_test %>% 
   pluck("accuracy") %>% 
   round(3) * 100

performance_train <- model %>% 
   evaluate(X_train, y_train)
accuracy_train <- performance_train %>% 
   pluck("accuracy") %>% 
   round(3) * 100



# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
