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
          plot = FALSE)
AUC_obj <- auc(ROC_obj) %>% 
      round(digits = 2)

# Get the class prediction w. a threshold of 0.5
#y_pred <- as.numeric(data_A0201_mdl_preds_test$pred_mdl_mean >= 0.5)

data_A0201_mdl_preds_test <- data_A0201_mdl_preds_test %>% 
   mutate(y_pred = case_when(pred_mdl_mean >= 0.5 ~ 1,
                             pred_mdl_mean < 0.5 ~ 0),
          correct = factor(case_when(Binding == y_pred ~ "Yes",
                              Binding != y_pred ~ "No")))
          # Binding = as.factor(Binding),
          # pred_mdl_mean = as.factor(pred_mdl_mean))

# Visualize data ----------------------------------------------------------

## ROC plot
ROC_obj %>% ggroc(
      colour = 'steelblue', 
                  size = 1.5)  + 
   geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                color="grey", linetype="dashed")+
   ggtitle(paste0('ROC Curve ',
                  '(AUC = ', AUC_obj, ')'))+
   labs(x = "Specificity", 
        y = "Sensitivity") +
   theme_minimal()


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
      theme_minimal() +
      theme(legend.position = "none")
      
## Confusion matrix
CM <- table(data_A0201_mdl_preds_test$Binding, 
            data_A0201_mdl_preds_test$y_pred) %>% as.matrix()

#True positives 
TP <- CM[2,2]

#True negatives 
TN <- CM[1,1]

#False positives 
FP <- CM[2,1]

#False negatives 
FN <- CM[1,2]


data_A0201_mdl_preds_test %>% 
   mutate(Binding = factor(Binding, levels= c(1,0)),
          y_pred = factor(y_pred,levels=c(1,0))) %>% 
   ggplot(.,mapping = aes(x = y_pred,
                     y= Binding,
                     fill=correct))+
   geom_tile() +
   #geom_text(aes(label= paste("",CM)))+
   scale_x_discrete(position = "top")
   theme_minimal()


#-----------------------
random <- runif(nrow(data_A0201_mdl_preds_test), 0, 0.5)
   
data_A0201_mdl_preds_test %>% ggplot(.,
                                     mapping = aes(x = Binding,
                                                   y= y_pred,
                                                   fill = correct)) +
   geom_point() +
   theme_minimal()
 
# Write data --------------------------------------------------------------

