# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("keras")
suppressWarnings(library("PepTools"))
library("tidymodels")


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


## Subset
set.seed(2005)
data_A0201 <- data_A0201 %>% # SUBSET
      sample_n(100)


## Pad short CDR3b sequences with "X" to same length
max_CDR3b <- data_A0201 %>% 
      select(CDR3b_size) %>% 
      max(.)

data_A0201 <- data_A0201 %>% 
      mutate(CDR3b = str_pad(string = CDR3b, 
                             width = max_CDR3b, 
                             side = "right", 
                             pad = "X"))

## Define independent test set, and the rest for 5-fold CV
data_A0201 <- data_A0201 %>%
      mutate(Set = sample(c("train", "test"),
                          size = nrow(.),
                          replace = TRUE,
                          prob = c(0.8, 0.2)))
data_A0201_test <- data_A0201 %>% 
      filter(Set == "test")
data_A0201 <- data_A0201 %>% 
      filter(Set == "train") %>% 
      select(-Set)

# View binder distribution
data_A0201_test %>% count(Binding)
data_A0201 %>% count(Binding)


## 5-fold nested cross-validation
set.seed(789)
partitions <- data_A0201 %>% 
      nested_cv(outside = vfold_cv(v = 5), 
                inside = vfold_cv(v = 4))

# ## How to extract:
# # First outer folds:
# as.data.frame(partitions$splits[[1]])
# # First inner fold in first outer fold:
# as.data.frame(partitions$inner_resamples[[1]]$splits[[1]])
# # Partition in train and test:
# as.data.frame(partitions$inner_resamples[[1]]$splits[[1]],
#               data = "assessment") #test
# as.data.frame(partitions$inner_resamples[[1]]$splits[[1]],
#               data = "analysis") #train



# Build model architecture ------------------------------------------------

## Set hyperparameters
n_epochs <- 10 #300 / 50
batch_size <- 128
loss_func <- "binary_crossentropy"
learn_rate <- 0.001
input_shape_pep <- c(9, 20, 1)
input_shape_CDR3b <- c(max_CDR3b, 21, 1)

# Train 20 models ---------------------------------------------------------
meta_data <- list()
model_dir <- "models/"
outer_folds <- 1:5
inner_folds <- 1:4
i <- 1
for (outer_i in outer_folds) {
      for (inner_j in inner_folds) {
            
            # Set model architecture ------------------------------
            
            # Build peptide layers
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
            
            # Build CDR3b layers
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
            
            # Concatenate final model architecture
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
            
            # Compile model
            cnn_model %>% 
                  compile(loss = loss_func,
                          optimizer = optimizer_rmsprop(learning_rate = learn_rate),
                          metrics = "accuracy")      
            
            
            # Set partitions ----------------------------------------------
            test_partition <- as.data.frame(
                  partitions$splits[[outer_i]],
                  data = "assessment")
            validation_partition <- as.data.frame(
                  partitions$inner_resamples[[outer_i]]$splits[[inner_j]],
                  data = "assessment")
            training_partitions <- as.data.frame(
                  partitions$inner_resamples[[outer_i]]$splits[[inner_j]],
                  data = "analysis")
            
            # Set model file
            mdl_i <- str_c("mdl_", i)
            model_file <- str_c(model_dir, mdl_i, ".hdf5")
            i <- i + 1
            
            # Set callbacks used for early stopping
            callbacks_list <- list(
                  callback_early_stopping(monitor = "val_loss",
                                          patience = 3),
                  callback_model_checkpoint(filepath = model_file,
                                            monitor = "val_loss",
                                            save_best_only = TRUE)
                  )
            
            # Define X and y training data and encode amino acids
            X_train_pep <- training_partitions %>% 
                  pull(Peptide) %>% 
                  blosum_encoding(peptide = ., 
                                  blosum_matrix = blosum62) %>% 
                  array_reshape(., c(nrow(.), 9, 20, 1))
            
            X_train_CDR3b <- training_partitions %>% 
                  pull(CDR3b) %>% 
                  blosum_encoding(peptide = .,
                                  blosum_matrix = blosum62_X) %>% 
                  array_reshape(., c(nrow(.), max_CDR3b, 21, 1))
            
            y_train <- training_partitions %>%
                  pull(Binding)
            
            # Define X and y validation data and encode amino acids
            X_val_pep <- validation_partition %>% 
                  pull(Peptide) %>% 
                  blosum_encoding(peptide = ., 
                                  blosum_matrix = blosum62) %>% 
                  array_reshape(., c(nrow(.), 9, 20, 1))
            
            X_val_CDR3b <- validation_partition %>% 
                  pull(CDR3b) %>% 
                  blosum_encoding(peptide = .,
                                  blosum_matrix = blosum62_X) %>% 
                  array_reshape(., c(nrow(.), max_CDR3b, 21, 1))
            
            y_val <- validation_partition %>%
                  pull(Binding)
            
            # Define X and y test data and encode amino acids
            X_test_pep <- test_partition %>%
                  pull(Peptide) %>% 
                  blosum_encoding(peptide = ., 
                                  blosum_matrix = blosum62) %>% 
                  array_reshape(., c(nrow(.), 9, 20, 1))
            
            X_test_CDR3b <- test_partition %>%
                  pull(CDR3b)%>% 
                  blosum_encoding(peptide = .,
                                  blosum62_X) %>% 
                  array_reshape(., c(nrow(.), max_CDR3b, 21, 1))
            
            y_test <- test_partition %>% 
                  pull(Binding)
            
            
            # Train model ---------------------------------------------
            history <- cnn_model %>% 
                  fit(x = list(X_train_pep, X_train_CDR3b),
                      y = y_train,
                      epochs = n_epochs,
                      batch_size = batch_size,
                      validation_data = list(list(X_val_pep, X_val_CDR3b),
                                             y_val),
                      callbacks = callbacks_list)
            
            # Save meta data
            meta_data[[mdl_i]] <- list()
            meta_data[[mdl_i]]$model_file <- model_file
            meta_data[[mdl_i]]$history <- history
            
            # Save model
            cnn_model %>% save_model_hdf5(filepath = model_file)
            
            
            # Make predictions on test data ---------------------------
            prediction <- cnn_model %>% 
                  predict(list(X_test_pep, 
                               X_test_CDR3b))
            meta_data[[mdl_i]]$predictions_test <- prediction
            
            # Performance on test data
            performance <- cnn_model %>%
                  evaluate(list(X_test_pep, X_test_CDR3b),
                           y_test)
            meta_data[[mdl_i]]$performance_test <- performance
            
            # Add predictions to our data set
            pred_mdl <- str_c("pred_", mdl_i)
            test_partition <- test_partition %>%
                  mutate(!!pred_mdl := as.numeric(
                        meta_data[[mdl_i]]$predictions_test)
                        )
            
            data_A0201 <- data_A0201 %>% 
                  left_join(y = test_partition,
                            by = c("CDR3b", "Peptide", "Binding", "CDR3b_size"))
            
      }
      
}

## View model
#cnn_model %>% summary()

## Calculate mean predictions
data_A0201 <- data_A0201 %>% 
      mutate(pred_mdl_mean = select(., contains("pred_")) %>% 
                   rowMeans(na.rm = TRUE))



# Model evaluation ---------------------------------------------------------

## Define X and y final test data and encode amino acids
X_test_final_pep <- data_A0201_test %>% 
      pull(Peptide) %>% 
      blosum_encoding(peptide = ., 
                      blosum_matrix = blosum62) %>% 
      array_reshape(., c(nrow(.), 9, 20, 1))

X_test_final_CDR3b <- data_A0201_test %>%
      pull(CDR3b)%>% 
      blosum_encoding(peptide = .,
                      blosum62_X) %>% 
      array_reshape(., c(nrow(.), max_CDR3b, 21, 1))

y_test_final <- data_A0201_test %>% 
      pull(Binding)
      

model_files <- list.files(path = "models")
all_models <- lapply(model_files, function(mdl_file) {
      mdl_file <- str_c("models/", mdl_file)
      load_model_hdf5(mdl_file)
})

a <- data_A0201_test
## Evaluate 20 models on test data never seen before
for (mdl_i in 1:20) {
      
      # Make predictions
      predictions <- all_models[[mdl_i]] %>% 
            predict(list(X_test_final_pep, 
                         X_test_final_CDR3b))
      # %>% 
      #       as.array()
      
      # Add predictions to dataset
      pred_mdl <- str_c("pred_mdl_", i)
      # a <- a %>% 
      #       bind_cols(predictions)
      a <- a %>%
             mutate(!!pred_mdl := predictions)
}
pred_mdl <- str_c("pred_", mdl_i)
test_partition <- test_partition %>%
      mutate(!!pred_mdl := as.numeric(
            meta_data[[mdl_i]]$predictions_test)
      )

# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
