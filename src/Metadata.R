

theme_blood <- function() {
  theme_classic() + # Fond classique et épuré
    theme(
      axis.text.x = element_text(size = 6, face = "bold", color = "black"), # Labels des axes X
      axis.text.y = element_text(size = 6, face = "bold", color = "black"),
      axis.title = element_text(size = 8, face = "bold"),
      axis.title.y = element_text(size = 8, face = "bold", color = "black"), # Labels des axes X
      legend.title = element_text(size = 7, face = "bold"), # Titre de la légende
      legend.text = element_text(size = 6, face = "bold"),
      legend.box.spacing = unit(0.1, "cm"),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(
        t = 0.6, # Top margin
        r = 0.5, # Right margin
        b = 0, # Bottom margin
        l = 1
      ), # Left margin
      plot.title = element_text(hjust = 0.5, size = 9, face = "bold"), # Titre du graphique centré
      strip.placement = "outside", # Strips à l'extérieur
      strip.background = element_blank(), # Suppression du cadre autour des strips
      strip.text = element_text(size = 6, face = "bold", angle = 0)
    )
}

