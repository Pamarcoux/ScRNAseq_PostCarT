library(rio)
library(tidyverse)
library(here)
library(ggrepel)


# Analysis ----------------------------------------------------------------

open_filter <- function (data_FC,min_expression = 0.05 ,threshold_pvalue = 0.05,threshold_FC = 0.5) {
  Col2 <- colnames(data_FC)[2]
  Col3 <- colnames(data_FC)[3]
  
  data_FC <- data_FC |> 
    mutate(across(where(is.numeric), ~na_if(., Inf))) |>
    mutate(across(where(is.numeric), ~na_if(., -Inf))) |>
    drop_na() |>
    filter(Col2> min_expression & Col3 > min_expression) |> 
    mutate(diffexpressed = case_when(
      log2FC > threshold_FC & pValueBH < threshold_pvalue ~ "UP",
      log2FC < -threshold_FC & pValueBH < threshold_pvalue ~ "DOWN",
      TRUE ~ "NO"
    )) |> 
    rename('minus_log10pvBH' = '-log10pvBH',
           "Pathway_name" = 1)
}

top_n_upregulated <- function(data_FC_filter,n=15){
  data_FC_filter %>% 
    filter(diffexpressed == "UP") %>% 
    arrange(-log2FC) %>%
    top_n(n, log2FC)
  }

top_n_downregulated <- function(data_FC_filter,n=15){  
  data_FC_filter %>% 
    filter(diffexpressed == "DOWN") %>% 
    arrange(log2FC) %>%
    top_n(-n, log2FC)
}

labels_n_up_down <- function(data_FC_filter,n=5){
  data_FC_up_down <- rbind(top_n_upregulated(data_FC_filter,n),top_n_downregulated(data_FC_filter,n))
  data_FC_filter <- data_FC_filter %>% 
    mutate(label = ifelse(Pathway_name %in% data_FC_up_down$Pathway_name, Pathway_name, ""))
  return(data_FC_filter)
}

# Plots -------------------------------------------------------------------

Plot_volcano_DE <- function(data_FC_filter,graph_title ="") {
  ggplot(data_FC_filter, aes(x=log2FC, y= minus_log10pvBH, color=diffexpressed, label=label)) + 
  geom_point() +
  geom_text_repel(size = 3) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA)) +  # Set y-axis limits
  # scale_x_continuous(limits = c(-3,3)) +  # Set x-axis limits
  geom_vline(xintercept=c(-threshold_FC, threshold_FC), col="black", linetype="dotted") +  # Add vertical lines
  geom_hline(yintercept=-log10(threshold_pvalue), col="black", linetype="dotted") +  # Add horizontal line
  scale_color_manual(values=c("#377eb8","black", "#e41a1c"))+
  labs(title = graph_title,
       x = "Log2 FC", y = "-log10(pValueBH)",
       color = "Expression Diff")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_blood()
  }

#Pathway graph for FC
Plot_DE_up_down <- function(data_FC_up_down,graph_title ="") {
  ggplot(data_FC_up_down, aes(x=log2FC,y=reorder(Pathway_name,log2FC),fill=diffexpressed))+
  geom_bar(stat = "identity", color = "black", size = 0.2) + 
  labs(
    title = graph_title,
    x = "Log2 Fold Change (Log2FC)",
    y = ""
  ) +
  scale_fill_manual(values = c("UP" = "#e41a1c", "DOWN" = "#377eb8")) + # Couleurs distinctes
  theme_blood() +
  theme(
    plot.title.position = "plot",
    axis.line = element_line(size = 0.5, color = "black"),
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    legend.position = "none",
    axis.text.y = element_text(size = 8, face = "bold", color = "black")
  )
}


# #Changer les pvalues =0
# indices_zero <- which(dataFC$pValue == 0)
# # Générer des nombres aléatoires entre 100 et 300
# n <- length(indices_zero)
# valeurs_aleatoires <- runif(n, min = 100, max = 300)
# # Assigner les valeurs aléatoires aux indices correspondants dans dataFC$X.log10pvBH
# dataFC$X.log10pvBH[indices_zero] <- valeurs_aleatoires



