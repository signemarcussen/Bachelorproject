# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
suppressWarnings(library("tidyverse"))
library("ggplot2")
library("ggthemes")
library("scales")
library("patchwork")
library("keras")

# Load data ---------------------------------------------------------------
#data_A0201 <- read_tsv(file = "data/03_data_A0201_complete.tsv.gz")
data_clean_all <- read_tsv(file = "data/02_data_clean_all.tsv.gz")
data_clean_matched <- read_tsv(file = "data/03_data_clean_matched.tsv.gz")
load(file = "data/04_i_blosum_metadata.Rdata")
load(file = "data/04_ii_onehot_metadata.Rdata")


# Wrangling data  ------------------------------------------------------------

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

## Monitor of acc and loss for model 5
training_mdl_5 <- meta_data_blosum[["mdl_5"]]$history %>% 
      plot() +
      xlim(1, 20) +
      labs(title = "Model 5") +
      theme(plot.title = element_text(hjust = 0.5))

p2 <- meta_data_onehot[["mdl_2"]]$history %>% 
      plot(metrics = "loss")
p3 <- meta_data_onehot[["mdl_3"]]$history %>% 
      plot(metrics = "loss")
p4 <- meta_data_onehot[["mdl_4"]]$history %>% 
      plot(metrics = "loss") +
      xlim(0,30) +
      labs(title = "hej")

(p1+p2) / (p3+p4)


# Creating plots ------------------------------------------------------------

## HLA distribution 
# 10 most abundant alleles 
top_alelle <- data_clean_matched %>% 
      group_by(Allele) %>% 
      summarise(Count = n()) %>%  
      top_n(10) %>% 
      select(Allele)

inner_join(data_clean_matched, 
           top_alelle, by = "Allele") %>% 
      ggplot(aes(x = Allele)) +
      geom_text(aes(label = ..count..), 
             stat = "count", vjust = -0.5, 
             colour = "black", size = 3) +
      geom_bar(fill="#3366CC") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 35, 
                                       hjust = 1)) +
      labs(x = "HLA allele", 
           y = "Allele count") +
      scale_y_continuous(labels = comma_format(big.mark = "."))


## Distribution of the peptide lenght 
data_clean_all %>% 
   mutate(pep_lenght = as.factor(pep_lenght)) %>% 
   ggplot(aes(x = pep_lenght)) + 
      geom_bar(fill="#3366CC") + 
      geom_text(aes(label = ..count..), 
                stat = "count", vjust = -0.5, 
                colour = "black", size = 3) +
      annotate(geom="text", 
               x= as.factor(14), y=280000, 
               label="n = 579.880",
               color="black") +
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold", 
                                      size = 16),
            plot.subtitle = element_text(face = "italic")) +
      labs(x = "Peptide lenght", 
           y = "Peptide count") +
      scale_y_continuous(labels = comma_format(big.mark = "."))


# Save plots --------------------------------------------------------------
ggsave(plot = training_mdl_5,
       filename = "results/plt_onehot_training_mdl_5.png")
