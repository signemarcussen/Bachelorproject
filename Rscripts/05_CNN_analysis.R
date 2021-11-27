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
library("gridExtra")

# Load data ---------------------------------------------------------------
blosum_test_preds <- read_tsv(file = "data/04_i_data_A0201_mdl_preds_test.tsv.gz")
blosum_CV_preds <- read_tsv(file = "data/04_i_data_A0201_mdl_preds_CV.tsv.gz")


onehot_test_preds <- read_tsv(file = "data/04_ii_data_A0201_onehot_mdl_preds_test_RMSall.tsv.gz")
onehot_CV_preds <- read_tsv(file = "data/04_ii_data_A0201_onehot_mdl_preds_CV_RMSall.tsv.gz")


# Metadata - DONT ASSIGN IT, just load and it will load into workspace
load(file = "data/04_i_blosum_metadata.Rdata")
load(file = "data/04_ii_onehot_metadata.Rdata")


# Define colors 
color_blosum_test <- "#003399"

color_blosum_CV <- "#99CCFF"

color_onehot_test <-"#FF9900"

color_onehot_CV <- "#FF3300"

# Loss during training of models -----------------------------
## One-Hot encoding -----------------------------
p <- list()
for (i in 1:20) {
      mdl <- str_c("mdl_", i)
      plt <- meta_data_onehot[[mdl]]$history %>% 
         as.data.frame() %>% 
         filter(metric == "loss") %>% 
         ggplot() +
         geom_point(aes(x = epoch, y = value,
                        color = data)) +
         geom_smooth(aes(x = epoch, y = value,
                         color = data), se = FALSE) +
         facet_wrap(~metric) + 
         xlim(1, 16) +
         theme_minimal()+
         theme(plot.title = element_text(hjust = 0.5,
                                         size = 8,
                                         face = "bold"),
               axis.title = element_text(size = 8),
               strip.text = element_blank()) +
         labs(title = paste0("Model ", i),
              x = "Epoch",
              y = "Loss") +
         scale_colour_manual(" ",
                             values = c(color_onehot_CV,
                                        color_onehot_test))
      
      p[[i]] = plt
}


p_mdl_onehot <- (p[[1]]+p[[2]]+p[[3]]+p[[4]]+p[[5]] +
             p[[6]]+p[[7]]+p[[8]]+p[[9]]+p[[10]] +
             p[[11]]+p[[12]]+p[[13]]+p[[14]]+p[[15]] +
             p[[16]]+p[[17]]+p[[18]]+p[[19]]+p[[20]]) +
   plot_annotation(title = "Loss",
                   subtitle = "One-Hot Encoding",
                   theme = theme(plot.title = element_text(size = 14,
                                                           hjust = 0.5),
                                 plot.subtitle = element_text(size = 12,
                                                              hjust = 0.5))) + 
   plot_layout(ncol = 5, 
               guides = "collect") &
   theme(legend.position = 'bottom')


## Blosum encoding -----------------------------
p <- list()
for (i in 1:20) {
   mdl <- str_c("mdl_", i)
   plt <- meta_data_blosum[[mdl]]$history %>% 
      as.data.frame() %>% 
      filter(metric == "loss") %>% 
      ggplot() +
      geom_point(aes(x = epoch, y = value,
                     color = data)) +
      geom_smooth(aes(x = epoch, y = value,
                      color = data), se = FALSE) +
      facet_wrap(~metric) + 
      xlim(1, 16) +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5,
                                      size = 8,
                                      face = "bold"),
            axis.title = element_text(size = 8),
            strip.text = element_blank()) +
      labs(title = paste0("Model ", i),
           x = "Epoch",
           y = "Loss") +
      scale_colour_manual(" ",
                          values = c(color_blosum_CV,
                                     color_blosum_test))
   
   p[[i]] = plt
}


p_mdl_blosum <- (p[[1]]+p[[2]]+p[[3]]+p[[4]]+p[[5]] +
                    p[[6]]+p[[7]]+p[[8]]+p[[9]]+p[[10]] +
                    p[[11]]+p[[12]]+p[[13]]+p[[14]]+p[[15]] +
                    p[[16]]+p[[17]]+p[[18]]+p[[19]]+p[[20]]) +
   plot_annotation(title = "Loss",
                   subtitle = "Blosum Encoding",
                   theme = theme(plot.title = element_text(size = 14,
                                                           hjust = 0.5),
                                 plot.subtitle = element_text(size = 12,
                                                           hjust = 0.5))) +
   plot_layout(ncol = 5, 
               guides = "collect") &
   theme(legend.position = 'bottom')


### For model 5, Blosum -------------------------------------------------------

p_mdl_5_blosum <- meta_data_blosum[["mdl_5"]]$history %>%
   as.data.frame() %>% 
   ggplot() +
   geom_point(aes(x = epoch, y = value, 
                  color = data)) +
   geom_smooth(aes(x = epoch, y = value, 
                 color = data), se = FALSE) +
   facet_wrap(~metric, nrow = 2,
              labeller = as_labeller(c("loss" = "Loss",
                                       "accuracy" ="Accuracy"))) +
   xlim(1, 20) +
   theme_minimal()+
   theme(axis.title = element_text(size = 14),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 11),
         strip.text = element_text(size = 12))+
   labs(x = "Epoch", 
        y = " ") +
   scale_colour_manual(" ",
                       values = c(color_blosum_CV,
                                  color_blosum_test),
                       labels = c("Training",
                                  "Validation"))
p_mdl_5_blosum


# ROC and AUC values -----------------------------------

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

## Bar plot for the AUC scores ----------------------------------------------

all_AUC <- tibble(Test_set = c("CV_b", "test_b", "CV_o", "test_o"), 
                  Encoding = c("Blosum", "Blosum", "One-Hot", "One-Hot"), 
                  AUC = c(AUC_blosum_CV, AUC_blosum_test, AUC_onehot_CV,AUC_onehot_test))

p_all_AUC <- all_AUC %>%  ggplot(aes(x = Encoding, y = AUC,
                        fill = Test_set)) +
            geom_col(stat = "identity",
                     position = "dodge",
                     alpha = 0.8) +
            geom_text(aes(label = AUC),
                      position = position_dodge(width = 0.9),
                      vjust = -0.5,
                      colour = "black", 
                      size = 5) +
            geom_text(aes(label =  c("CV",
                                     "Hold-out",
                                     "CV",
                                     "Hold-out"),
                          y = 0.03),
                      position = position_dodge(width = 0.9),
                      colour = "black", 
                      size = 5) +
            theme_hc()+
            theme(axis.title = element_text(size = 14),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  legend.position = "none") +
            labs(x = "Encoding",
                 y = "AUC") +
            scale_y_continuous(labels = comma_format(big.mark = " ",
                                                     decimal.mark = "."),
                               limits=c(0,1)) +
            scale_fill_manual(values = c("test_b" = color_blosum_test,
                                         "test_o" = color_onehot_test,
                                         "CV_b" = color_blosum_CV,
                                         "CV_o" = color_onehot_CV))

## ROC plot for the two best AUC scores ------------------------------------------------------

p_ROC_best <- ggroc(list('One-Hot, AUC = 0.79'= ROC_onehot_test,
           'Blosum, AUC = 0.77' = ROC_blosum_test),
           legacy.axes = TRUE,
           size = 1) + 
            scale_colour_manual("Encoding",
                                values = c(color_onehot_test,
                                           color_blosum_test)) +
            geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                         color="grey", linetype="dashed") + 
            theme_minimal() +   
            theme(legend.position = c(0.85,0.17),
                  axis.title = element_text(size = 14),
                  axis.text =  element_text(size = 12),
                  legend.text = element_text(size = 11),
                  legend.title = element_text(size = 12)) + 
            labs(x = "1-Specificity",
                 y = "Sensitivity")
      


## ROC plot for One-Hot -----------------------------------------------------

p_ROC_onehot <- ggroc(list('CV, AUC = 0.76'= ROC_onehot_CV,
                           'Hold-out, AUC = 0.79' = ROC_onehot_test),
                            legacy.axes = TRUE,
            size = 1) + 
         scale_colour_manual("Test set",
                             values = c(color_onehot_CV,
                                        color_onehot_test)) +
         geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                      color="grey", linetype="dashed") + 
         theme_minimal() +   
         theme(plot.title = element_text(hjust = 0.5,
                                         size = 14),
               plot.subtitle = element_text(hjust = 0.5,
                                         size = 12),
               legend.position = c(0.85,0.2),
               axis.title = element_text(size = 14),
               axis.text =  element_text(size = 12),
               legend.text = element_text(size = 11),
               legend.title = element_text(size = 12)) + 
         labs(title = "ROC",
              subtitle = "One-Hot Encoding",
              x = "1-Specificity",
              y = "Sensitivity")


## ROC plot for Blosum ------------------------------------------------------

p_ROC_blosum <- ggroc(list('CV, AUC = 0.74'= ROC_blosum_CV,
                       'Hold-out, AUC = 0.77' = ROC_blosum_test),
                       legacy.axes = TRUE,
                       size = 1) + 
   scale_colour_manual("Test set",
                       values = c(color_blosum_CV,
                                  color_blosum_test)) +
   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                color="grey", linetype="dashed") + 
   theme_minimal() +   
   theme(plot.title = element_text(hjust = 0.5,
                                   size = 14),
         plot.subtitle = element_text(hjust = 0.5,
                                   size = 12),
         legend.position = c(0.85,0.2),
         axis.title = element_text(size = 14),
         axis.text =  element_text(size = 12),
         legend.text = element_text(size = 11),
         legend.title = element_text(size = 12)) + 
   labs(title = "ROC",
        subtitle = "Blosum Encoding",
        x = "1-Specificity",
        y = "Sensitivity")

# Sensitivity / specificity plot --------------------------------------------

## Sensitivity / specificity plot, Blosum, Hold-out -----------------------------------
# Getting the coordinates from the ROC-curve, test set 
coords_blosum_test <- coords(ROC_blosum_test, 
                        x = "all")

# Finding the threshold 
coords_blosum_test <- coords_blosum_test %>% 
   mutate(diff = specificity-sensitivity)

# Find value in tibble
threshold_blosum_test <- 0.4903516

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
                           color = 'Specificity (TNR) ')) +
   geom_line(mapping = aes(y = sensitivity, 
                           x = threshold, 
                           color = 'Sensitivity (TRP)')) +
   geom_vline(xintercept = threshold_blosum_test,
              linetype='dotted')+
   scale_colour_manual(values = c(color_blosum_CV,
                                  color_blosum_test)) +
   theme_minimal() +
   theme(axis.title = element_text(size = 14),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 11)) +
   labs(x = "Threshold", 
        y = "Rate",
        color = " ") 


## Sensitivity / specificity plot, Blosum, CV -----------------------------------

# Getting the coordinates from the ROC-curve, CV
coords_blosum_CV <- coords(ROC_blosum_CV, 
                           x = "all")
# Finding the threshold 
coords_blosum_CV <- coords_blosum_CV %>% 
   mutate(diff = specificity-sensitivity)

# Find value in tibble
threshold_blosum_CV <- 0.4912365

## The threshold is very close to the one for Hold-out set, 
## therefore plot is not created 


## Sensitivity / specificity plot, One-Hot, Hold-out  -----------------------------------
# Getting the coordinates from the ROC-curve
coords_onehot_test <- coords(ROC_onehot_test, 
                             x = "all")

# Finding the threshold 
coords_onehot_test <- coords_onehot_test %>% 
   mutate(diff = specificity-sensitivity)

# Find value in tibble
threshold_onehot_test <- 0.5069805

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
   scale_colour_manual(values = c(color_onehot_CV,
                                  color_onehot_test))+
   theme_minimal() +
   theme(axis.title = element_text(size = 14),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 11)) +
   labs(x = "Threshold", 
        y = "Rate",
        color = " ")


## Sensitivity / specificity plot, One-Hot, CV  -----------------------------------
# Getting the coordinates from the ROC-curve
coords_onehot_CV <- coords(ROC_onehot_CV, 
                             x = "all")

# Finding the threshold 
coords_onehot_CV <- coords_onehot_CV %>% 
   mutate(diff = specificity-sensitivity)

# Find value in tibble
threshold_onehot_CV <- 0.5108373

## The threshold is very close to the one for Hold-out set, 
## therefore plot is not created 



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
   scale_colour_manual(values = c(color_blosum_CV,
                                  "#0066CC"))+
   scale_x_continuous(name = "Predicted Class",
                      breaks = c(0,1)) +
   scale_y_continuous(name = "Actual Class",
                      breaks = c(0,1)) +
   theme_minimal()+ 
   theme(#axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_blank(),
         legend.position = "none")


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

# Accuracy
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
   scale_colour_manual(values = c(color_onehot_test,
                                  color_onehot_CV))+
   scale_x_continuous(name = "Predicted Class",
                      breaks = c(0,1)) +
   scale_y_continuous(name = "Actual Class",
                      breaks = c(0,1)) +
   theme_minimal()+ 
   theme(#axis.title = element_text(size = 14),
         axis.text = element_text(size = 12),
         axis.title = element_blank(),
         legend.position = "none") 

both_CM <- grid.arrange(p_blosum_CM_test,
             p_onehot_CM_test,
             ncol = 2,
             left = "Actual Class",
             bottom = "Predicted Class")


# Save plots --------------------------------------------------------------

ggsave(filename = "results/CNN_onehot_loss.png",
       plot = p_mdl_onehot,
       width = 7.85,
       height = 7, 
       units = "in")

ggsave(filename = "results/CNN_blosum_loss.png",
       plot = p_mdl_blosum,
       width = 7.85,
       height = 7, 
       units = "in")

ggsave(filename = "results/CNN_blosum_loss_mdl_5.png",
       plot = p_mdl_5_blosum,
       width = 7.39,
       height = 3.47, 
       units = "in")

ggsave(filename = "results/all_AUC.png",
       plot = p_all_AUC,
       width = 8,
       height = 6, 
       units = "in")

ggsave(filename = "results/ROC_best_test.png",
       plot = p_ROC_best,
       width = 7.39,
       height = 3.47, 
       units = "in")

ggsave(filename = "results/ROC_onehot.png",
       plot = p_ROC_onehot,
       width = 7.39,
       height = 3.47, 
       units = "in")

ggsave(filename = "results/ROC_blosum.png",
       plot = p_ROC_blosum,
       width = 7.39,
       height = 3.47, 
       units = "in")

ggsave(filename = "results/ss_blosum_test.png",
       plot = p_ss_blosum_test,
       width = 7.5,
       height = 2.6, 
       units = "in")

ggsave(filename = "results/ss_onehot_test.png",
       plot = p_ss_onehot_test,
       width = 7.5,
       height = 2.6, 
       units = "in")

ggsave(filename = "results/blosum_CM_test.png",
       plot = p_blosum_CM_test,
       width = 5,
       height = 5, 
       units = "in")

ggsave(filename = "results/onehot_CM_test.png",
       plot = p_onehot_CM_test,
       width = 5,
       height = 5, 
       units = "in")

# ggsave(filename = "results/both_CM.png",
#        plot = both_CM,
#        width = 10,
#        height = 5,
#        units = "in")
