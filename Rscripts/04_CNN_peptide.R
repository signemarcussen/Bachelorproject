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
   sample_n(10000)

data_A0201_Xy <- data_A0201 %>% 
      mutate(Set = sample(c("train", "test"),
                          size = nrow(.),
                          replace = TRUE,
                          prob = c(0.8, 0.2))) %>% 
   # Partition = sample(1:5,
   #                   size = nrow(.),
   #                  replace = TRUE,
   #                 prob = c(0.2, 0.2, 0.2, 0.2, 0.2)))

## Pad short CDR3b sequences with "X" to same length
data_A0201_Xy <- data_A0201_Xy %>% 
   mutate(CDR3b = str_pad(string = CDR3b, 
                          width = max(nchar(CDR3b)), 
                          side = "right", 
                          pad = "X"))

## View binder and set distribution
data_A0201_Xy %>% 
      count(Binding, Set)

## Encode peptides and define training/test matrices
X_train <- data_A0201_Xy %>% 
   filter(Set == "train") %>% 
   pull(Peptide) %>% 
   blosum_encoding(peptide = ., 
                   blosum_matrix = blosum62) %>% 
   array_reshape(., c(nrow(.), 9, 20, 1))

X_test <- data_A0201_Xy %>% 
   filter(Set == "test") %>% 
   pull(Peptide) %>% 
   blosum_encoding(peptide = ., 
                   blosum_matrix = blosum62) %>% 
   array_reshape(., c(nrow(.), 9, 20, 1))

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
input_shape_pep <- c(9, 20)
#input_shape_CDR3b <- c(max_seq, 21, 1)


## Build model with functional API: https://keras.rstudio.com/articles/functional_api.html)
peptide_input <- layer_input(shape = input_shape_pep)
#CDR3b_input <- layer_input(shape = input_shape_CDR3b)

pep_k1 <- peptide_input %>% 
   layer_conv_1d(filters = 16,
                 kernel_size = 1,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_1d(pool_size = 2)
pep_k3 <- peptide_input %>% 
   layer_conv_1d(filters = 16,
                 kernel_size = 3,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_1d(pool_size = 2)
pep_k5 <- peptide_input %>% 
   layer_conv_1d(filters = 16,
                 kernel_size = 5,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_1d(pool_size = 2)
pep_k7 <- peptide_input %>% 
   layer_conv_1d(filters = 16,
                 kernel_size = 7,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_1d(pool_size = 2)
pep_k9 <- peptide_input %>% 
   layer_conv_1d(filters = 16,
                 kernel_size = 9,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_1d(pool_size = 2)


pep_output <- layer_concatenate(inputs = c(pep_k1, 
                                           pep_k3, 
                                           pep_k5,
                                           pep_k7,
                                           pep_k9),
                                axis = 1)

CDR3b_k1 <- CDR3b_input %>% 
   layer_conv_1d(filters = 16,
                 kernel_size = 1,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape_pep) %>% 
   layer_max_pooling_1d(pool_size = 2)




model <- keras_model(
   inputs = input,
   outputs = output
)



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
