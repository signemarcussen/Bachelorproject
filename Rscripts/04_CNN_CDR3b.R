# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library(keras)
library(PepTools)

# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")
blosum62_raw <- read.table(file = "data/_raw/BLOSUM62.txt", 
                           skip = 6)


# Wrangle data ------------------------------------------------------------
blosum62 <- blosum62_raw %>% 
      select(-c("B", "Z", "X", "X.")) %>% 
      slice(1:(n() - 4)) %>% 
      mutate( X = 0) %>% 
      as.matrix()

X <- rep(0,21)
blosum62 <- rbind(blosum62, X)

## Define training/test set
set.seed(2005)
data_A0201 <- data_A0201 %>%
   sample_n(10000)

data_A0201_Xy <- data_A0201 %>% 
      mutate(Set = sample(c("train", "test"),
                          size = nrow(.),
                          replace = TRUE,
                          prob = c(0.8, 0.2))) %>% 
            # Partition = sample(1:5,
             #                   size = nrow(.),
              #                  replace = TRUE,
               #                 prob = c(0.2, 0.2, 0.2, 0.2, 0.2))) %>% 
   drop_na()

#Padding short sequences with "X"
data_A0201_Xy <- data_A0201_Xy %>% 
   mutate(CDR3b = str_pad(string = CDR3b, 
                          width = max(nchar(CDR3b)), 
                          side="right", pad="X"))

## View binder and set distribution
data_A0201_Xy %>% 
      count(Binding, Set)

## Encode peptides and define training/test matrices
### Using pep_encode, to create 3D tensor of the CDR3b sequences
### with dimensions n_peps x l_peps x l_enc, this is converted to a 1D array. 
### so the first l_peps x l_enc entries correspond to one seq.
max_seq <- data_A0201_Xy %>% select(CDR3b_size) %>%  max(.)
   
X_train <- data_A0201_Xy %>% 
      filter(Set == "train") %>% 
      pull(CDR3b)%>% 
      pep_encode(pep = .) %>% 
      array_reshape(., c(nrow(.), max_seq, 21, 1))
#Before reshape: 
#dim(X_train) = 158396 -seqs     25-lenght of seqs    21-cols in Blosum62

X_test <- data_A0201_Xy %>% 
   filter(Set == "test") %>% 
   pull(CDR3b)%>% 
   pep_encode(pep = .) %>% 
   array_reshape(., c(nrow(.), max_seq, 21, 1))

y_train <- data_A0201_Xy %>% 
      filter(Set == "train") %>% 
      pull(Binding)

y_test <- data_A0201_Xy %>% 
      filter(Set == "test") %>% 
      pull(Binding) 


# Model data --------------------------------------------------------------

## Set hyperparameters
n_epochs <- 5 #300 / 50
batch_size <- 500
loss_func <- "binary_crossentropy"
learn_rate <- 0.001
input_shape <- c(max_seq, 21, 1)

## Build model architecture
cnn_model <- keras_model_sequential() %>% 
   layer_conv_2d(filters = 32,
                 kernel_size = c(3, 3),
                 activation = 'relu',
                 input_shape = input_shape) %>%
   layer_max_pooling_2d(pool_size = c(2,2)) %>% 
   layer_conv_2d(filters = 64,
                 kernel_size = c(3, 3),
                 activation = 'relu') %>%
   #layer_dropout(rate = 0.25) %>% 
   layer_flatten() %>% 
   #layer_dense(units  = 180, activation = 'relu') %>% 
   #layer_dropout(rate = 0.4) %>% 
   #layer_dense(units  = 10, activation  = 'relu') %>%
   #layer_dropout(rate = 0.3) %>%
   layer_dense(units = 10, activation = "relu") %>% 
   layer_dense(units  = 1, activation   = 'sigmoid')

## Compile model
cnn_model %>% 
   compile(loss = loss_func,
           optimizer = optimizer_rmsprop(learning_rate = learn_rate),
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

# Model data --------------------------------------------------------------


# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
