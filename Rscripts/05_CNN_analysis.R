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
data_A0201_mdl_preds_test <- read_tsv(file = "data/04_data_A0201_mdl_preds_test.tsv.gz")


# Wrangle data ------------------------------------------------------------

## Compute ROC and AUC 
ROC_obj <- data_A0201_mdl_preds_test %>% 
      roc(response = Binding, 
          predictor = pred_mdl_mean, 
          plot = TRUE)
AUC_obj <- auc(ROC_obj) %>% 
      round(digits = 2)


# Visualize data ----------------------------------------------------------

## ROC plot
ROC_obj %>% ggroc(
      colour = 'steelblue', 
                  size = 2) + 
      ggtitle(paste0('ROC Curve ', '(AUC = ', AUC_obj, ')'))

# Get the class prediction w. a threshold of 0.5
# y_pred <- prediction %>%  as.numeric(prediction[,1] >= 0.5)

## Sensitivity / specificity plot
# Getting the coordinates from the ROC-curve. 
my_coords <- coords(ROC_obj, 
                    x = "all")
best_coords <- coords(ROC_obj, 
                      x = "best", 
                      best.method = "youden") 
ggplot(my_coords) +
      geom_line(mapping = aes(y = specificity, 
                              x = threshold, 
                              color = 'red'))+
      geom_line(mapping = aes(y = sensitivity, 
                              x = threshold, 
                              color = 'blue'))+
      geom_hline(mapping = aes(yintercept = best_coords$specificity, 
                               color = 'red'),
                 linetype='dotted') +
      geom_hline(mapping = aes(yintercept = best_coords$sensitivity, 
                               color = 'blue'), 
                 linetype='dotted')+
      geom_vline(xintercept = best_coords$threshold, 
                 linetype='dotted', 
                 color = 'black')+
      labs(x = "Cutoff for predicted probability", 
           y = "Sensitivity, Specificity") +
      theme_minimal()


# Write data --------------------------------------------------------------

