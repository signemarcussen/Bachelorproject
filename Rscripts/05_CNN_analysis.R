# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("keras")
library("pROC")
library("ggplot2")
library("patchwork")

# Load data ---------------------------------------------------------------
blosum_test_preds <- read_tsv(file = "data/04_i_data_A0201_mdl_preds_test.tsv.gz")
blosum_CV_preds <- read_tsv(file = "data/04_i_data_A0201_mdl_preds_CV.tsv.gz")


onehot_test_preds <- read_tsv(file = "data/04_ii_data_A0201_onehot_mdl_preds_test_RMSall.tsv.gz")
onehot_CV_preds <- read_tsv(file = "data/04_ii_data_A0201_onehot_mdl_preds_CV_RMSall.tsv.gz")

blosum_test_preds <- blosum_test_preds %>% sample_n(2000)
blosum_CV_preds <- blosum_CV_preds %>% sample_n(2000)


onehot_test_preds <- onehot_test_preds %>% sample_n(2000)
onehot_CV_preds <- onehot_CV_preds %>% sample_n(2000)


# Metadata - DONT ASSIGN IT, just load and it will load into workspace
load(file = "data/04_i_blosum_metadata.Rdata")
load(file = "data/04_ii_onehot_metadata.Rdata")

# Wrangle data ------------------------------------------------------------
blosum_test_preds <- blosum_test_preds %>% mutate(y_pred = case_when(pred_mdl_mean >= 0.49 ~ 1,
                                                                     pred_mdl_mean < 0.49 ~ 0),
                                                  correct = factor(case_when(Binding == y_pred ~ "Yes",
                                                                             Binding != y_pred ~ "No")))

blosum_CV_preds <- blosum_CV_preds %>% mutate(y_pred = case_when(pred_mdl_mean >= 0.49 ~ 1,
                                                                     pred_mdl_mean < 0.49 ~ 0),
                                                  correct = factor(case_when(Binding == y_pred ~ "Yes",
                                                                             Binding != y_pred ~ "No")))

onehot_test_preds <- onehot_test_preds %>% mutate(y_pred = case_when(pred_mdl_mean >= 0.51 ~ 1,
                                                                     pred_mdl_mean < 0.51 ~ 0),
                                                  correct = factor(case_when(Binding == y_pred ~ "Yes",
                                                                             Binding != y_pred ~ "No")))

onehot_CV_preds <- onehot_CV_preds %>% mutate(y_pred = case_when(pred_mdl_mean >= 0.51 ~ 1,
                                                                     pred_mdl_mean < 0.51 ~ 0),
                                                  correct = factor(case_when(Binding == y_pred ~ "Yes",
                                                                             Binding != y_pred ~ "No")))


# Accuracy and loss during training of models -----------------------------
p <- list()
for (i in 1:20) {
      mdl <- str_c("mdl_", i)
      plt <- meta_data_onehot[[mdl]]$history %>% 
            plot()
      p[[i]] = plt
}

(p[[1]]+p[[2]]+p[[3]]+p[[4]]+p[[5]]) / 
      (p[[6]]+p[[7]]+p[[8]]+p[[9]]+p[[10]]) / 
      (p[[11]]+p[[12]]+p[[13]]+p[[14]]+p[[15]]) /
      (p[[16]]+p[[17]]+p[[18]]+p[[19]]+p[[20]])

## For model 5
training_mdl_5 <- meta_data_blosum[["mdl_5"]]$history %>% 
      plot() +
      xlim(1, 20) +
      labs(title = "Model 5") +
      theme(plot.title = element_text(hjust = 0.5))


# Performance plots for Blosum-encoding -----------------------------------

## ROC and AUC values Blosum
ROC_blosum_test <- blosum_test_preds %>% 
      roc(response = Binding, 
          predictor = pred_mdl_mean)
AUC_blosum_test <- auc(ROC_blosum_test) %>% 
      round(digits = 2)

ROC_blosum_CV <- blosum_CV_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)
AUC_blosum_CV <- auc(ROC_blosum_CV) %>% 
   round(digits = 2)

## ROC and AUC values One-Hot
ROC_onehot_test <- onehot_test_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)

AUC_onehot_test <- auc(ROC_onehot_test) %>% 
   round(digits = 2)

ROC_onehot_CV <- onehot_CV_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)
AUC_onehot_CV <- auc(ROC_onehot_CV) %>% 
   round(digits = 2)

# Bar plot for the AUC scores 

all_AUC <- tibble(Method = c("CV", "test", "CV", "test"), 
                  Model = c("Blosum", "Blosum", "One-Hot", "One-Hot"), 
                  AUC = c(AUC_blosum_CV, AUC_blosum_test, AUC_onehot_CV,AUC_onehot_test))
all_AUC %>%  ggplot(aes(x = Model, y = AUC)) +
   geom_bar(stat = "identity")

## ROC plot for One-Hot
# p_ROC_onehot <- ROC_onehot_test %>% ggroc(legacy.axes = TRUE,
#                           colour = 'steelblue', 
#                           size = 1.5)  + 
#    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
#                 color="grey", linetype="dashed")+
#    theme_minimal() +   
#    theme(plot.title = element_text(hjust = 0.5,
#                                    size = 10)) + 
#    labs(x = "1-Specificity",
#         y = "Sensitivity", 
#         title = paste0('ROC Curve for One-Hot encoding, ',
#                        'AUC = ', AUC_onehot_test,''))
# p_ROC_onehot

ggroc(list('CV, AUC = 0.74'= ROC_onehot_CV,
           'Hold-out, AUC = 0.77' = ROC_onehot_test),
      legacy.axes = TRUE,
      size = 1) + 
   scale_colour_manual("Model",values = c("darkgreen", "green")) +
   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                color="grey", linetype="dashed") + 
   theme_minimal() +   
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         legend.position = c(0.9,0.15)) + 
   labs(x = "1-Specificity",
        y = "Sensitivity", 
        title = "ROC curve, One-Hot encoding") 
#_______________________________________________________________________


## ROC plot for Blosum
# p_ROC_blosum <- ROC_blosum_test %>% ggroc(legacy.axes = TRUE,
#                      colour = 'steelblue', 
#                      size = 1.5) + 
#    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
#                 color="grey", linetype="dashed") +
#    theme_minimal() +   
#    theme(plot.title = element_text(hjust = 0.5,
#                                    size = 10)) + 
#    labs(x = "1-Specificity",
#         y = "Sensitivity", 
#         title = paste0('ROC Curve for Blosum encoding, ',
#                        'AUC = ', AUC_blosum_test,''))
# p_ROC_blosum

ggroc(list('CV, AUC = 0.74'= ROC_blosum_CV,
           'Hold-out, AUC = 0.77' = ROC_blosum_test),
      legacy.axes = TRUE,
      size = 1) + 
   scale_colour_manual("Model",values = c("steelblue", "darkblue")) +
   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                color="grey", linetype="dashed") + 
   theme_minimal() +   
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         legend.position = c(0.9,0.15)) + 
   labs(x = "1-Specificity",
        y = "Sensitivity", 
        title = "ROC curve, Blosum encoding") 


## Sensitivity / specificity plot, Blosum
# Getting the coordinates from the ROC-curve. 
coords_blosum_test <- coords(ROC_blosum_test, 
                        x = "all")
coords_blosum_CV <- coords(ROC_blosum_CV, 
                             x = "all")

p_ss_blosum_test <- ggplot(coords_blosum_test) +
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
        title = "SS plot Blosum, Hold-out set, cut-off ~0.49",
        color = " ")

p_ss_blosum_CV <- ggplot(coords_blosum_CV) +
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
        title = "SS plot Blosum, CV set, cut-off ~0.49",
        color = " ")

p_ss_blosum_test / p_ss_blosum_CV

## Confusion matrix Blosum, hold-out 
## Confusion matrix
# Binding = row, y_pred = column
CM_blosum_test <- table(blosum_test_preds$Binding, 
            blosum_test_preds$y_pred)

# True positives 
TP_blosum_test <- CM_blosum_test[2,2]

# True negatives 
TN_blosum_test <- CM_blosum_test[1,1]

# False positives 
FP_blosum_test <- CM_blosum_test[1,2]

# False negatives 
FN_blosum_test <- CM_blosum_test[2,1]

# Accucary
(TP_blosum_test+TN_blosum_test)/(TP_blosum_test+TN_blosum_test+FP_blosum_test+FN_blosum_test)

# Create random numbers to spread the data points
random <- runif(nrow(blosum_test_preds), -0.45, 0.45)
random1 <- runif(nrow(blosum_test_preds), -0.45, 0.45)


p_blosum_CM_test <- blosum_test_preds %>% 
   ggplot(., mapping = aes(x = y_pred+random1, y = Binding+random)) +
   geom_point(aes(colour = factor(correct)),
              size = 0.5) +
   annotate(geom = "text", 
            x = 0, y = 0, 
            label = paste0('TN = ', TN_blosum_test),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 0, 
            label = paste0('FP = ', FP_blosum_test),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 0, y = 1, 
            label = paste0('FN = ', FN_blosum_test),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 1, 
            label = paste0('TP = ', TP_blosum_test),
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
   labs(title = "Confusion Matrix, Blosum encoding w cut-off threshold  = 0.49, Hold-out set")

## Confusion matrix Blosum, CV
## Confusion matrix
# Binding = row, y_pred = column
CM_blosum_CV <- table(blosum_CV_preds$Binding, 
                        blosum_CV_preds$y_pred)

# True positives 
TP_blosum_CV <- CM_blosum_CV[2,2]

# True negatives 
TN_blosum_CV <- CM_blosum_CV[1,1]

# False positives 
FP_blosum_CV <- CM_blosum_CV[1,2]

# False negatives 
FN_blosum_CV <- CM_blosum_CV[2,1]

# Accucary
#(TP_blosum_test+TN_blosum_test)/(TP_blosum_test+TN_blosum_test+FP_blosum_test+FN_blosum_test)

# Create random numbers to spread the data points
random2 <- runif(nrow(blosum_CV_preds), -0.45, 0.45)
random3 <- runif(nrow(blosum_CV_preds), -0.45, 0.45)


p_blosum_CM_CV <- blosum_CV_preds %>% 
   ggplot(., mapping = aes(x = y_pred+random2, y = Binding+random3)) +
   geom_point(aes(colour = factor(correct)),
              size = 0.5) +
   annotate(geom = "text", 
            x = 0, y = 0, 
            label = paste0('TN = ', TN_blosum_CV),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 0, 
            label = paste0('FP = ', FP_blosum_CV),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 0, y = 1, 
            label = paste0('FN = ', FN_blosum_CV),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 1, 
            label = paste0('TP = ', TP_blosum_CV),
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
   labs(title = "Confusion Matrix, Blosum encoding w cut-off threshold  = 0.49, CV set")

p_blosum_CM_test + p_blosum_CM_CV


# Performance plots for One-Hot-encoding ------------------------------------------------------------


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

# Accuracy 
(TP+TN)/(TP+TN+FP+FN)

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



ggroc(list('One-Hot, AUC = 0.79'= ROC_onehot_test,
           'Blosum, AUC = 0.77' = ROC_blosum_test),
      legacy.axes = TRUE,
      size = 1) + 
   scale_colour_manual("Model",values = c("green", "darkblue")) +
   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                color="grey", linetype="dashed") + 
   theme_minimal() +   
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         legend.position = c(0.9,0.15)) + 
   labs(title = "ROC curve, Hold-out test set")  
           
           # paste0('ROC Curve for One-Hot encoding, ',
           #        'AUC = ', AUC_onehot_test,''))
# Write data --------------------------------------------------------------

# ggsave(filename = "results/04_plot_preTreatContinuous.png",
#        plot = plot1,
#        width = 9.22,
#        height = 3.99,
#        units = "in")