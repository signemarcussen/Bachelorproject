# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("keras")
suppressWarnings(library("PepTools"))
library("pROC")
library("ggplot2")
library("patchwork")


# Define functions --------------------------------------------------------
source("Rscripts/99_project_functions.R")


# Load data ---------------------------------------------------------------
blosum_test_preds <- read_tsv(file = "data/04_i_data_A0201_mdl_preds_test.tsv.gz")
blosum_CV_preds <- read_tsv(file = "data/04_i_data_A0201_mdl_preds_CV.tsv.gz")


onehot_test_preds <- read_tsv(file = "data/04_ii_data_A0201_onehot_mdl_preds_test_RMSall.tsv.gz")
onehot_CV_preds <- read_tsv(file = "data/04_ii_data_A0201_onehot_mdl_preds_CV_RMSall.tsv.gz")

# Metadata - DONT ASSIGN IT, just load and it will load into workspace
load(file = "data/04_i_blosum_metadata.Rdata")
load(file = "data/04_ii_onehot_metadata.Rdata")

# Wrangle data -------------------------------------------------------------------------------------
blosum_test_preds <- blosum_test_preds %>% mutate(y_pred = case_when(pred_mdl_mean >= 0.49 ~ 1,
                                                                     pred_mdl_mean < 0.49 ~ 0),
                                                  correct = factor(case_when(Binding == y_pred ~ "Yes",
                                                                             Binding != y_pred ~ "No")))

onehot_test_preds <- onehot_test_preds %>% mutate(y_pred = case_when(pred_mdl_mean >= 0.51 ~ 1,
                                                                     pred_mdl_mean < 0.51 ~ 0),
                                                  correct = factor(case_when(Binding == y_pred ~ "Yes",
                                                                             Binding != y_pred ~ "No")))
# Performance plots for Blosum-encoding ------------------------------------------------------------

## ROC and AUC values
ROC_blosum_test <- blosum_test_preds %>% 
      roc(response = Binding, 
          predictor = pred_mdl_mean)
AUC_blosum_test <- auc(ROC_blosum_test) %>% 
      round(digits = 2)

ROC_blosum_CV <- blosum_CV_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)
AUC_blosum_CV <- auc(ROC_blosum_test) %>% 
   round(digits = 2)

## ROC plot for Blosum
p_ROC_blosum <- ROC_blosum_test %>% ggroc(legacy.axes = TRUE,
                     colour = 'steelblue', 
                     size = 1.5) + 
   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                color="grey", linetype="dashed") +
   theme_minimal() +   
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10)) + 
   labs(x = "1-Specificity",
        y = "Sensitivity", 
        title = paste0('ROC Curve for Blosum encoding, ',
                       'AUC = ', AUC_blosum_test,''))
p_ROC_blosum

## Sensitivity / specificity plot, Blosum
# Getting the coordinates from the ROC-curve. 
coords_blosum <- coords(ROC_blosum_test, 
                        x = "all")

p_ss_blosum <- ggplot(coords_blosum) +
   geom_line(mapping = aes(y = specificity, 
                           x = threshold, 
                           color = 'Specificity'))+
   geom_line(mapping = aes(y = sensitivity, 
                           x = threshold, 
                           color = 'Sensitivity'))+
   geom_vline(xintercept = 0.49,
              linetype='dotted')+
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10)) +
   labs(x = "Cut-off for predicted probability", 
        y = "Sensitivity, Specificity",
        title = "SS plot Blosum, cut-off ~0.49",
        color = " ") 
p_ss_blosum

## Confusion matrix Blosum
## Confusion matrix
# Binding = row, y_pred = column
CM_blosum <- table(blosum_test_preds$Binding, 
            blosum_test_preds$y_pred)

# True positives 
TP_blosum <- CM_blosum[2,2]

# True negatives 
TN_blosum <- CM_blosum[1,1]

# False positives 
FP_blosum <- CM_blosum[1,2]

# False negatives 
FN_blosum <- CM_blosum[2,1]

# Create random numbers to spread the data points
random <- runif(nrow(onehot_test_preds), -0.45, 0.45)
random1 <- runif(nrow(onehot_test_preds), -0.45, 0.45)


p_blosum_CM <- blosum_test_preds %>% 
   ggplot(., mapping = aes(x = y_pred+random1, y = Binding+random)) +
   geom_point(aes(colour = factor(correct)),
              size = 0.5) +
   annotate(geom = "text", 
            x = 0, y = 0, 
            label = paste0('TN = ', TN_blosum),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 0, 
            label = paste0('FP = ', FP_blosum),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 0, y = 1, 
            label = paste0('FN = ', FN_blosum),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 1, 
            label = paste0('TP = ', TP_blosum),
            color = "black",
            size=8) +
   scale_x_continuous(name = "Predicted Class",
                      breaks = c(0,1)) +
   scale_y_continuous(name = "Actual Class",
                      breaks = c(0,1)) +
   theme_minimal()+ 
   theme(plot.title = element_text( size = 10,
                                    hjust = 0.5),
         plot.subtitle = element_text(face = "italic"),
         legend.position = "none") +
   labs(title = "Confusion Matrix, Blosum encoding w cut-off threshold  = 0.49")
p_blosum_CM


# Performance plots for One-Hot-encoding ------------------------------------------------------------

## ROC and AUC values
ROC_onehot_test <- onehot_test_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)
AUC_onehot_test <- auc(ROC_onehot_test) %>% 
   round(digits = 2)

ROC_onehot_CV <- onehot_CV_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)
AUC_onehot_CV <- auc(ROC_onehot_test) %>% 
   round(digits = 2)

## ROC plot for One-Hot
p_ROC_onehot <- ROC_onehot_test %>% ggroc(legacy.axes = TRUE,
                          colour = 'steelblue', 
                          size = 1.5)  + 
   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                color="grey", linetype="dashed")+
   theme_minimal() +   
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10)) + 
   labs(x = "1-Specificity",
        y = "Sensitivity", 
        title = paste0('ROC Curve for One-Hot encoding, ',
                       'AUC = ', AUC_onehot_test,''))
p_ROC_onehot

## Sensitivity / specificity plot, One-Hot
# Getting the coordinates from the ROC-curve. 
coords_onehot <- coords(ROC_onehot_test, 
                    x = "all")

p_ss_onehot <- ggplot(coords_onehot) +
   geom_line(mapping = aes(y = specificity, 
                           x = threshold, 
                           color = 'Specificity'))+
   geom_line(mapping = aes(y = sensitivity, 
                           x = threshold, 
                           color = 'Sensitivity'))+
   geom_vline(xintercept = 0.51,
              linetype='dotted')+
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10)) +
   labs(x = "Cut-off for predicted probability", 
        y = "Sensitivity, Specificity",
        title = "ss plot One-Hot, cut-off ~ 0.51",
        color = " ")

p_ss_onehot

## Confusion matrix One-Hot
## Confusion matrix
CM <- table(onehot_test_preds$Binding, 
            onehot_test_preds$y_pred) %>% as.matrix()

# True positives 
TP <- CM[2,2]

# True negatives 
TN <- CM[1,1]

# False positives 
FP <- CM[1,2]
 
# False negatives 
FN <- CM[2,1]

# Create random numbers to spread the data points
random <- runif(nrow(onehot_test_preds), -0.45, 0.45)
random1 <- runif(nrow(onehot_test_preds), -0.45, 0.45)


p_onehot_CM <- onehot_test_preds %>% 
   ggplot(., mapping = aes(x = y_pred+random1, y = Binding+random)) +
   geom_point(aes(colour = factor(correct)),
              size = 0.5) +
   annotate(geom = "text", 
            x = 0, y = 0, 
            label = paste0('TN = ', TN),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 0, 
            label = paste0('FP = ', FP),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 0, y = 1, 
            label = paste0('FN = ', FN),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 1, 
            label = paste0('TP = ', TP),
            color = "black",
            size=8) +
   scale_x_continuous(name = "Predicted Class",
                      breaks = c(0,1)) +
   scale_y_continuous(name = "Actual Class",
                      breaks = c(0,1)) +
   theme_minimal()+ 
   theme(plot.title = element_text(size = 10,
                                   hjust = 0.5),
         legend.position = "none") +
   labs(title = "Confusion Matrix, One-Hot encoding w cut-off threshold  = 0.51")
p_onehot_CM

# Collect plots by type -----------------------------------------------------------------------------------------------

p_ROC_blosum + p_ROC_onehot

p_ss_blosum + p_ss_onehot

p_blosum_CM + p_onehot_CM


# Write data --------------------------------------------------------------

# ggsave(filename = "results/04_plot_preTreatContinuous.png",
#        plot = plot1,
#        width = 9.22,
#        height = 3.99,
#        units = "in")