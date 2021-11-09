# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("keras")
suppressWarnings(library("PepTools"))
library("pROC")

# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------


## Predictions of the model 
prediction <- cnn_model %>% predict(list(X_test_pep,X_test_CDR3b))

## ROC and AUC 
roc_obj <- roc(response = y_test, predictor = prediction[,1], plot = TRUE)
auc_obj <- round(auc(y_test, prediction[,1]),4)

#create ROC plot
ggroc(roc_obj, colour = 'steelblue', size = 2) +
      ggtitle(paste0('ROC Curve ', '(AUC = ', auc_obj, ')'))

# Get the class prediction w. a threshold of 0.5
# y_pred <- prediction %>%  as.numeric(prediction[,1] >= 0.5)

## Sensitivity / specificity 
# Getting the coordinates from the ROC-curve. 
my_coords <- coords(roc_obj, "all")
best_coords <- coords(roc_obj, "best", best.method="youden") 
ggplot(my_coords) +
      geom_line(mapping = aes(y=specificity, x=threshold, color = 'red'))+
      geom_line(mapping = aes(y=sensitivity, x=threshold, color = 'blue'))+
      geom_hline(mapping = aes(yintercept = best_coords$specificity, color = 'red'),
                 linetype='dotted') +
      geom_hline(aes (yintercept = best_coords$sensitivity, color = 'blue'), 
                 linetype='dotted')+
      geom_vline(xintercept = best_coords$threshold, linetype='dotted', color = 'black')+
      labs(x = "Cutoff for predicted probability", y = "Sensitivity, Specificity") +
      theme_minimal()


# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------

