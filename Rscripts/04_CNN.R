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
data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")
blosum62_raw <- read.table(file = "data/_raw/BLOSUM62.txt", 
                           skip = 6)


# Wrangle data ------------------------------------------------------------
## Blosum matrix
X <- rep(0,21)
blosum62_X <- blosum62_raw %>% 
   select(-c("B", "Z", "X", "X.")) %>% 
   slice(1:(n() - 4)) %>% 
   mutate(X = 0) %>% 
   as.matrix() %>% 
   rbind(X)

blosum62 <- blosum62_X %>% 
   as.data.frame() %>% 
   select(-X) %>% 
   slice(1:(nrow(.)-1)) %>% 
   as.matrix()


## Define training/test set
set.seed(2005)
data_A0201 <- data_A0201 %>% # SUBSET
   sample_n(80000)

data_A0201_Xy <- data_A0201 %>%
      mutate(Set = sample(c("train", "test"),
                          size = nrow(.),
                          replace = TRUE,
                          prob = c(0.8, 0.2)))
    

## Pad short CDR3b sequences with "X" to same length
max_CDR3b <- data_A0201_Xy %>% 
   select(CDR3b_size) %>% 
   max(.)

data_A0201_Xy <- data_A0201_Xy %>% 
   mutate(CDR3b = str_pad(string = CDR3b, 
                          width = max_CDR3b, 
                          side = "right", 
                          pad = "X"))

## View binder and set distribution
data_A0201_Xy %>% 
      count(Binding, Set)

## Encode amino acids and define training/test matrices
X_train_pep <- data_A0201_Xy %>% 
   filter(Set == "train") %>% 
   pull(Peptide) %>% 
   blosum_encoding(peptide = ., 
                   blosum_matrix = blosum62) %>% 
   array_reshape(., c(nrow(.), 9, 20, 1))

X_test_pep <- data_A0201_Xy %>% 
   filter(Set == "test") %>% 
   pull(Peptide) %>% 
   blosum_encoding(peptide = ., 
                   blosum_matrix = blosum62) %>% 
   array_reshape(., c(nrow(.), 9, 20, 1))

X_train_CDR3b <- data_A0201_Xy %>% 
   filter(Set == "train") %>% 
   pull(CDR3b) %>% 
   blosum_encoding(peptide = .,
                   blosum_matrix = blosum62_X) %>% 
   array_reshape(., c(nrow(.), max_CDR3b, 21, 1))

X_test_CDR3b <- data_A0201_Xy %>% 
   filter(Set == "test") %>% 
   pull(CDR3b)%>% 
   blosum_encoding(peptide = .,
                   blosum62_X) %>% 
   array_reshape(., c(nrow(.), max_CDR3b, 21, 1))

y_train <- data_A0201_Xy %>% 
   filter(Set == "train") %>% 
   pull(Binding)

y_test <- data_A0201_Xy %>% 
   filter(Set == "test") %>% 
   pull(Binding)


# Model data --------------------------------------------------------------

## Set hyperparameters
n_epochs <- 10 #300 / 50
batch_size <- 128
loss_func <- "binary_crossentropy"
learn_rate <- 0.001
input_shape_pep <- c(9, 20, 1)
input_shape_CDR3b <- c(max_CDR3b, 21, 1)

# Functional API: https://keras.rstudio.com/articles/functional_api.html)

## Build peptide model
peptide_input <- layer_input(shape = input_shape_pep)

pep_k1 <- peptide_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(1, 20),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_2d(pool_size = 2)
pep_k3 <- peptide_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(3, 20),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))
pep_k5 <- peptide_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(5, 20),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))
pep_k7 <- peptide_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(7, 20),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))
pep_k9 <- peptide_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(9, 20),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))


pep_output <- layer_concatenate(inputs = c(pep_k1, 
                                           pep_k3, 
                                           pep_k5,
                                           pep_k7,
                                           pep_k9))


## Build CDR3b model
CDR3b_input <- layer_input(shape = input_shape_CDR3b)

CDR3b_k1 <- CDR3b_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(1, 21),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_CDR3b) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))
CDR3b_k3 <- CDR3b_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(3, 21),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_CDR3b) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))
CDR3b_k5 <- CDR3b_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(5, 21),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_CDR3b) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))
CDR3b_k7 <- CDR3b_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(7, 21),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_CDR3b) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))
CDR3b_k9 <- CDR3b_input %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = c(9, 21),
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_CDR3b) %>% 
   layer_max_pooling_2d(pool_size = c(2, 2))

CDR3b_output <- layer_concatenate(inputs = c(CDR3b_k1,
                                             CDR3b_k3,
                                             CDR3b_k5,
                                             CDR3b_k7,
                                             CDR3b_k9))

## Concatenate models and keep building
concatenated_model <- layer_concatenate(list(pep_output,
                                             CDR3b_output),
                                        axis = 1) %>% 
   layer_flatten() %>% 
   layer_dense(units = 100,
               activation = "relu")

output_model <- concatenated_model %>% 
   layer_dense(units = 1,
               activation = "sigmoid")

cnn_model <- keras_model(
   inputs = list(peptide_input, CDR3b_input),
   outputs = output_model)

## Compile model
cnn_model %>% 
   compile(loss = loss_func,
           optimizer = optimizer_rmsprop(learning_rate = learn_rate),
           metrics = "accuracy")

## View model
cnn_model %>% summary()


## Train model
cnn_history <- cnn_model %>% 
   fit(x = list(X_train_pep, X_train_CDR3b),
       y = y_train,
       epochs = n_epochs,
       batch_size = batch_size,
       validation_split = 0.25,
       callbacks = callback_early_stopping(monitor = "val_loss",
                                           patience = 1))


## Evaluate model
performance_test <- cnn_model %>%
   evaluate(list(X_test_pep, X_test_CDR3b), 
            y_test)
accuracy_test <- performance_test %>%
   pluck("accuracy") %>%
   round(3) * 100



# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
