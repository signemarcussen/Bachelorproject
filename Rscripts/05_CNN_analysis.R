# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("keras")
library("pROC")
library("ggplot2")
library("patchwork")
library("ggthemes")
library("scales")

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
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))+
      labs(x = "Epoch", 
           y = " ") +
      scale_y_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ","))+
      scale_colour_manual("Encoding",
                          values = c("red",
                                     "black"))
training_mdl_5

meta_data_blosum[["mdl_5"]]$history %>% 
   as.data.frame() %>% 
   ggplot() +
   geom_point(aes(x = epoch, y = value, 
                  color = data)) +
   geom_smooth(aes(x = epoch, y = value, 
                 color = data), se = FALSE) +
   facet_wrap(~metric, nrow = 2) +
   #Change strip size 
   xlim(1, 20) +
   labs(title = "Model 5") +
   theme_minimal()+
   theme(plot.title = element_text(hjust = 0.5),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12))+
   labs(x = "Epoch", 
        y = " ") +
   scale_colour_manual("Encoding",
                       values = c("red",
                                  "black"))


# ROC and AUC values -----------------------------------

## ROC and AUC values Blosum
ROC_blosum_test <- blosum_test_preds %>% 
      roc(response = Binding, 
          predictor = pred_mdl_mean)

ROC_blosum_test_CL <- "#003399"
   
AUC_blosum_test <- auc(ROC_blosum_test) %>% 
      round(digits = 2)

ROC_blosum_CV <- blosum_CV_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)

ROC_blosum_CV_CL <- "#99CCFF"

AUC_blosum_CV <- auc(ROC_blosum_CV) %>% 
   round(digits = 2)

## ROC and AUC values One-Hot
ROC_onehot_test <- onehot_test_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)

ROC_onehot_test_CL <-"#FF9900"

AUC_onehot_test <- auc(ROC_onehot_test) %>% 
   round(digits = 2)

ROC_onehot_CV <- onehot_CV_preds %>% 
   roc(response = Binding, 
       predictor = pred_mdl_mean)

ROC_onehot_CV_CL <- "#FF3300"

AUC_onehot_CV <- auc(ROC_onehot_CV) %>% 
   round(digits = 2)

## Bar plot for the AUC scores ----------------------------------------------

all_AUC <- tibble(Test_set = c("CV", "test", "CV", "test"), 
                  Encoding = c("Blosum", "Blosum", "One-Hot", "One-Hot"), 
                  AUC = c(AUC_blosum_CV, AUC_blosum_test, AUC_onehot_CV,AUC_onehot_test))

p_all_AUC <- all_AUC %>%  ggplot(aes(x = Test_set, y = AUC,
                        fill = Encoding)) +
            geom_col(stat = "identity",
                     position = "dodge",
                     alpha = 0.7) +
            geom_text(aes(label = AUC),
                      position = position_dodge(width = 1),
                      vjust = -0.5,
                      colour = "black", 
                      size = 5) +
            theme_hc()+
            theme(axis.title = element_text(size = 14),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  legend.position = "right") +
            labs(x = "Test set", 
                 y = "AUC",
                 fill = "Encoding") +
            scale_y_continuous(labels = comma_format(big.mark = ".",
                                                     decimal.mark = ","),
                               limits=c(0,1)) +
            scale_x_discrete(labels = c("Cross-validation", 
                                          "Hold-out")) +
            scale_fill_manual(values = c(ROC_blosum_test_CL,
                                         ROC_onehot_test_CL))

p_all_AUC
## ROC plot for the two best AUC scores ------------------------------------------------------

p_ROC_best <- ggroc(list('One-Hot, AUC = 0,79'= ROC_onehot_test,
           'Blosum, AUC = 0,77' = ROC_blosum_test),
           legacy.axes = TRUE,
           size = 1) + 
            scale_colour_manual("Encoding",
                                values = c(ROC_onehot_test_CL,
                                           ROC_blosum_test_CL)) +
            geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                         color="grey", linetype="dashed") + 
            theme_minimal() +   
            theme(plot.title = element_text(hjust = 0.5,
                                            size = 10),
                  legend.position = c(0.9,0.15),
                  axis.title = element_text(size = 14),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12)) + 
            labs(title = "ROC curve, Hold-out test set",
                 x = "1-Specificity",
                 y = "Sensitivity",) +
            scale_y_continuous(labels = comma_format(big.mark = ".",
                                                     decimal.mark = ","))+
            scale_x_continuous(labels = comma_format(big.mark = ".",
                                                     decimal.mark = ","))
p_ROC_best        

## ROC plot for One-Hot

p_ROC_onehot <- ggroc(list('CV, AUC = 0,76'= ROC_onehot_CV,
                           'Hold-out, AUC = 0,79' = ROC_onehot_test),
                            legacy.axes = TRUE,
            size = 1) + 
         scale_colour_manual("Test set",
                             values = c(ROC_onehot_CV_CL,ROC_onehot_test_CL)) +
         geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                      color="grey", linetype="dashed") + 
         theme_minimal() +   
         theme(plot.title = element_text(hjust = 0.5,
                                         size = 10),
               legend.position = c(0.9,0.15),
               axis.title = element_text(size = 14),
               axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12)) + 
         labs(title = "ROC curve, Hold-out test set",
              x = "1-Specificity",
              y = "Sensitivity",) +
         scale_y_continuous(labels = comma_format(big.mark = ".",
                                                  decimal.mark = ","))+
         scale_x_continuous(labels = comma_format(big.mark = ".",
                                                  decimal.mark = ","))
p_ROC_onehot

## ROC plot for Blosum

p_ROC_blosum <- ggroc(list('CV, AUC = 0,74'= ROC_blosum_CV,
                       'Hold-out, AUC = 0,77' = ROC_blosum_test),
                       legacy.axes = TRUE,
                       size = 1) + 
   scale_colour_manual("Test set",
                       values = c(ROC_blosum_CV_CL,
                                  ROC_blosum_test_CL)) +
   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                color="grey", linetype="dashed") + 
   theme_minimal() +   
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         legend.position = c(0.9,0.15),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12)) + 
   labs(title = "ROC curve, Hold-out test set",
        x = "1-Specificity",
        y = "Sensitivity",) +
   scale_y_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))+
   scale_x_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))
p_ROC_blosum

# Sensitivity / specificity plot --------------------------------------------

## Sensitivity / specificity plot, Blosum, Hold-out -----------------------------------
# Getting the coordinates from the ROC-curve, test set 
coords_blosum_test <- coords(ROC_blosum_test, 
                        x = "all")

# Finding the threshold 
coords_blosum_test <- coords_blosum_test %>% 
   mutate(diff = specificity-sensitivity)

# Find value in tibble
threshold_blosum_test <- 0.4969333

# Defining clases based on threshold 
blosum_test_preds <- blosum_test_preds %>% 
   mutate(y_pred = case_when(pred_mdl_mean >= threshold_blosum_test ~ 1,
                             pred_mdl_mean < threshold_blosum_test ~ 0),
          correct = factor(case_when(Binding == y_pred ~ "Yes",
                                     Binding != y_pred ~ "No")))

# Creating plot, Blosum hold out set 

p_ss_blosum_test <- ggplot(coords_blosum_test) +
   geom_line(mapping = aes(y = specificity, 
                           x = threshold, 
                           color = 'Specificity (TNR) '))+
   geom_line(mapping = aes(y = sensitivity, 
                           x = threshold, 
                           color = 'Sensitivity (TRP)'))+
   geom_vline(xintercept = threshold_blosum_test,
              linetype='dotted')+
   scale_colour_manual(values = c(ROC_blosum_CV_CL,
                                  ROC_blosum_test_CL))+
   # scale_y_continuous(sec_axis(~ .*1, 
   #                             name = "Specificity"))+
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12)) +
   labs(x = "Cut-off for predicted probability", 
        y = "Sensitivity, Specificity",
        title = "SS plot Blosum, Hold-out set, cut-off ~0.49",
        color = " ")+
   scale_y_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))+
   scale_x_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))

p_ss_blosum_test

## Sensitivity / specificity plot, Blosum, CV -----------------------------------

# Getting the coordinates from the ROC-curve, CV
coords_blosum_CV <- coords(ROC_blosum_CV, 
                           x = "all")
# Finding the threshold 
coords_blosum_CV <- coords_blosum_CV %>% 
   mutate(diff = specificity-sensitivity)

# Find value in tibble
threshold_blosum_CV <- 0.4969333


blosum_CV_preds <- blosum_CV_preds %>% 
   mutate(y_pred = case_when(pred_mdl_mean >= threshold_blosum_CV ~ 1,
                             pred_mdl_mean < threshold_blosum_CV ~ 0),
          correct = factor(case_when(Binding == y_pred ~ "Yes",
                                     Binding != y_pred ~ "No")))

# Creating plot, Blosum CV set 

p_ss_blosum_CV <- ggplot(coords_blosum_CV) +
   geom_line(mapping = aes(y = specificity, 
                           x = threshold, 
                           color = 'Specificity (TNR) '))+
   geom_line(mapping = aes(y = sensitivity, 
                           x = threshold, 
                           color = 'Sensitivity (TRP)'))+
   geom_vline(xintercept = threshold_blosum_CV,
              linetype='dotted')+
   scale_colour_manual(values = c(ROC_blosum_CV_CL,
                                  ROC_blosum_test_CL))+
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12)) +
   labs(x = "Cut-off for predicted probability", 
        y = "Sensitivity, Specificity",
        title = "SS plot Blosum, CV set, cut-off ~0.49",
        color = " ")+
   scale_y_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))+
   scale_x_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))

p_ss_blosum_CV

## Sensitivity / specificity plot, One-Hot, Hold-out  -----------------------------------
# Getting the coordinates from the ROC-curve
coords_onehot_test <- coords(ROC_onehot_test, 
                             x = "all")

# Finding the threshold 
coords_onehot_test <- coords_onehot_test %>% 
   mutate(diff = specificity-sensitivity)

# Find value in tibble
threshold_onehot_test <- 0.4969333

# Defining clases based on threshold 
onehot_test_preds <- onehot_test_preds %>% 
   mutate(y_pred = case_when(pred_mdl_mean >= threshold_onehot_test ~ 1,
                             pred_mdl_mean < threshold_onehot_test ~ 0),
          correct = factor(case_when(Binding == y_pred ~ "Yes",
                                     Binding != y_pred ~ "No")))

# Creating plot, Blosum hold out set 

p_ss_onehot_test <- ggplot(coords_onehot_test) +
   geom_line(mapping = aes(y = specificity, 
                           x = threshold, 
                           color = 'Specificity (TNR) '))+
   geom_line(mapping = aes(y = sensitivity, 
                           x = threshold, 
                           color = 'Sensitivity (TRP)'))+
   geom_vline(xintercept = threshold_onehot_test,
              linetype='dotted')+
   scale_colour_manual(values = c(ROC_onehot_CV_CL,
                                  ROC_onehot_test_CL))+
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12)) +
   labs(x = "Cut-off for predicted probability", 
        y = "Sensitivity, Specificity",
        title = "SS plot One-hot, Hold-out set, cut-off ~0.49",
        color = " ")+
   scale_y_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))+
   scale_x_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))

p_ss_onehot_test

## Sensitivity / specificity plot, One-Hot, CV  -----------------------------------
# Getting the coordinates from the ROC-curve
coords_onehot_CV <- coords(ROC_onehot_CV, 
                             x = "all")

# Finding the threshold 
coords_onehot_CV <- coords_onehot_CV %>% 
   mutate(diff = specificity-sensitivity)

# Find value in tibble
threshold_onehot_CV <- 0.4969333

# Defining clases based on threshold 
onehot_CV_preds <- onehot_CV_preds %>% 
   mutate(y_pred = case_when(pred_mdl_mean >= threshold_onehot_CV ~ 1,
                             pred_mdl_mean < threshold_onehot_CV ~ 0),
          correct = factor(case_when(Binding == y_pred ~ "Yes",
                                     Binding != y_pred ~ "No")))

# Creating plot, Blosum CV set 

p_ss_onehot_CV <- ggplot(coords_onehot_CV) +
   geom_line(mapping = aes(y = specificity, 
                           x = threshold, 
                           color = 'Specificity (TNR) '))+
   geom_line(mapping = aes(y = sensitivity, 
                           x = threshold, 
                           color = 'Sensitivity (TRP)'))+
   geom_vline(xintercept = threshold_onehot_CV,
              linetype='dotted')+
   scale_colour_manual(values = c(ROC_onehot_CV_CL,
                                  ROC_onehot_test_CL))+
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12)) +
   labs(x = "Cut-off for predicted probability", 
        y = "Sensitivity, Specificity",
        title = "SS plot One-hot, CV set, cut-off ~0.49",
        color = " ")+
   scale_y_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))+
   scale_x_continuous(labels = comma_format(big.mark = ".",
                                            decimal.mark = ","))

p_ss_onehot_CV


# Confusion matrix ------------------------------------------------------------
## CM, Blosum, hold-out -------------------------------------------------------

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
   scale_colour_manual(values = c(ROC_blosum_CV_CL,
                                  ROC_blosum_test_CL))+
   scale_x_continuous(name = "Predicted Class",
                      breaks = c(0,1)) +
   scale_y_continuous(name = "Actual Class",
                      breaks = c(0,1)) +
   theme_minimal()+ 
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.position = "none") +
   labs(title = "Confusion Matrix, Blosum encoding w cut-off threshold  = 0.49, Hold-out set")

p_blosum_CM_test

## CM, One-Hot, hold-out -------------------------------------------------------

## Confusion matrix
# Binding = row, y_pred = column
CM_onehot_test <- table(onehot_test_preds$Binding, 
                        onehot_test_preds$y_pred)

# True positives 
TP_onehot_test <- CM_onehot_test[2,2]

# True negatives 
TN_onehot_test <- CM_onehot_test[1,1]

# False positives 
FP_onehot_test <- CM_onehot_test[1,2]

# False negatives 
FN_onehot_test <- CM_onehot_test[2,1]

# Accucary
(TP_onehot_test+TN_onehot_test)/(TP_onehot_test+TN_onehot_test+FP_onehot_test+FN_onehot_test)

# Create random numbers to spread the data points
random <- runif(nrow(onehot_test_preds), -0.45, 0.45)
random1 <- runif(nrow(onehot_test_preds), -0.45, 0.45)


p_onehot_CM_test <- onehot_test_preds %>% 
   ggplot(., mapping = aes(x = y_pred+random1, y = Binding+random)) +
   geom_point(aes(colour = factor(correct)),
              size = 0.5) +
   annotate(geom = "text", 
            x = 0, y = 0, 
            label = paste0('TN = ', TN_onehot_test),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 0, 
            label = paste0('FP = ', FP_onehot_test),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 0, y = 1, 
            label = paste0('FN = ', FN_onehot_test),
            color = "black",
            size=8) +
   annotate(geom = "text", 
            x = 1, y = 1, 
            label = paste0('TP = ', TP_onehot_test),
            color = "black",
            size=8) +
   scale_colour_manual(values = c(ROC_onehot_test_CL,
                                  ROC_onehot_CV_CL))+
   scale_x_continuous(name = "Predicted Class",
                      breaks = c(0,1)) +
   scale_y_continuous(name = "Actual Class",
                      breaks = c(0,1)) +
   theme_minimal()+ 
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 10),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.position = "none") +
   labs(title = "Confusion Matrix, onehot encoding w cut-off threshold  = 0.49, Hold-out set")

p_onehot_CM_test


# Collect plots by type -----------------------------------------------------------------------------------------------

p_all_AUC + p_ROC_best

p_ss_blosum_test / p_ss_onehot_test +
   plot_layout(guides = "collect")

p_blosum_CM_test + p_onehot_CM_test +
   plot_layout(guides = "collect",
               axis.title.x="collect")


# Write data --------------------------------------------------------------

# ggsave(filename = "results/04_plot_preTreatContinuous.png",
#        plot = plot1,
#        width = 9.22,
#        height = 3.99,
#        units = "in")