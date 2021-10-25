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
blosum62 <- blosum62_raw %>% 
      select(-c("B", "Z", "X", "X.")) %>% 
      slice(1:(n() - 4)) %>% 
      as.matrix()
      

## Define training/test set
set.seed(2005)
data_A0201 <- data_A0201 %>% # SUBSET
   sample_n(10000)

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
n_epochs <- 5 #300 / 50
batch_size <- 128 #128
loss_func <- "binary_crossentropy"
learn_rate <- 0.001
input_shape <- c(9, 20, 1)

## Build model architecture
cnn_model <- keras_model_sequential() %>% 
   layer_conv_2d(filters = 32,
                 kernel_size = 3,
                 activation = 'relu',
                 input_shape = input_shape) %>%
   layer_max_pooling_2d(pool_size = c(2,2)) %>% 
   #layer_dropout(rate = 0.25) %>% 
   layer_flatten() %>% 
   layer_dense(units = 32, activation = "relu") %>% 
   layer_dense(units  = 1, activation   = 'sigmoid')


## Model with several kernel_sizes (Not working)
conv_l1_k1 <- keras_model_sequential() %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = 1,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape) %>% 
   layer_max_pooling_2d(pool_size = c(2,2))
conv_l1_k3 <- keras_model_sequential() %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = 3,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape) %>%
   layer_max_pooling_2d(pool_size = c(2,2))
conv_l1_k5 <- keras_model_sequential() %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = 5,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape) %>%
   layer_max_pooling_2d(pool_size = c(2,2))
conv_l1_k7 <- keras_model_sequential() %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = 7,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape) %>%
   layer_max_pooling_2d(pool_size = c(2,2))
conv_l1_k9 <- keras_model_sequential() %>% 
   layer_conv_2d(filters = 16,
                 kernel_size = 9,
                 padding = "same",
                 activation = 'relu',
                 input_shape = input_shape) 

predictions <- layer_concatenate(inputs = c(conv_l1_k1, 
                                            conv_l1_k3, 
                                            conv_l1_k5, 
                                            conv_l1_k7, 
                                            conv_l1_k9)) %>% 
   layer_dense(units = 32, activation = "relu") %>% 
   layer_dense(units  = 1, activation   = 'sigmoid')

cnn_model <- keras_model(inputs = input_shape,
                         outputs = predictions)
## Another try: (Read up on functional API: https://keras.rstudio.com/articles/functional_api.html)
conv_k1 <- layer_conv_2d(filters = 16,
                            kernel_size = 1,
                            padding = "same",
                            activation = 'relu',
                            input_shape = input_shape) 

pool_k1 <- layer_max_pooling_2d(object = conv_k1, pool_size = c(2,2))

input <- layer_input(shape = input_shape)
model_out <- 

model <- keras_model(
   inputs = c(),
   outputs = c()
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
