# =============================================================================
# Graphique d'analyse globale des données - Marriage Data India
# =============================================================================

library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)

# Chargement des données
data <- read.csv("marriage_data_india.csv", stringsAsFactors = TRUE)

# Configuration du thème commun
theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.position = "none"
  )

# Palette de couleurs
col_primary <- "#2196F3"
col_secondary <- "#E91E63"
col_gradient <- c("#4CAF50", "#FFC107", "#F44336")

# =============================================================================
# Graphique 1: Répartition par genre
# =============================================================================
g1 <- data %>%
  group_by(Gender) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = pct, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Female" = col_secondary, "Male" = col_primary)) +
  labs(title = "Répartition par genre") +
  theme_void() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

# =============================================================================
# Graphique 2: Types de mariage
# =============================================================================
g2 <- data %>%
  group_by(Marriage_Type) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(Marriage_Type, -pct), y = pct, fill = Marriage_Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Arranged" = "#9C27B0", "Love" = "#FF5722")) +
  scale_x_discrete(labels = c("Arranged" = "Arrangé", "Love" = "Amour")) +
  labs(title = "Types de mariage", x = "", y = "Pourcentage (%)") +
  ylim(0, 75) +
  theme_custom

# =============================================================================
# Graphique 3: Niveau d'éducation
# =============================================================================
g3 <- data %>%
  group_by(Education_Level) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(Education_Level, -pct), y = pct, fill = Education_Level)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Niveau d'éducation", x = "", y = "Pourcentage (%)") +
  ylim(0, 42) +
  theme_custom +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# =============================================================================
# Graphique 4: Niveau de revenu
# =============================================================================
g4 <- data %>%
  group_by(Income_Level) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(Income_Level, levels = c("Low", "Middle", "High")), 
             y = pct, fill = Income_Level)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Low" = "#F44336", "Middle" = "#FFC107", "High" = "#4CAF50"),
                    labels = c("Faible", "Moyen", "Élevé")) +
  scale_x_discrete(labels = c("Low" = "Faible", "Middle" = "Moyen", "High" = "Élevé")) +
  labs(title = "Niveau de revenu", x = "", y = "Pourcentage (%)") +
  ylim(0, 40) +
  theme_custom

# =============================================================================
# Graphique 5: Satisfaction maritale
# =============================================================================
g5 <- data %>%
  group_by(Marital_Satisfaction) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(Marital_Satisfaction, levels = c("Low", "Medium", "High")), 
             y = pct, fill = Marital_Satisfaction)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Low" = "#F44336", "Medium" = "#FFC107", "High" = "#4CAF50")) +
  scale_x_discrete(labels = c("Low" = "Faible", "Medium" = "Moyenne", "High" = "Élevée")) +
  labs(title = "Satisfaction maritale", x = "", y = "Pourcentage (%)") +
  ylim(0, 40) +
  theme_custom

# =============================================================================
# Graphique 6: Statut de divorce
# =============================================================================
g6 <- data %>%
  group_by(Divorce_Status) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = pct, fill = Divorce_Status)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("No" = "#4CAF50", "Yes" = "#F44336"),
                    labels = c("Non divorcé", "Divorcé")) +
  labs(title = "Taux de divorce") +
  theme_void() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

# =============================================================================
# Assemblage du graphique composite
# =============================================================================
png("analyse_globale_mariages_inde.png", width = 1400, height = 900, res = 150)
grid.arrange(
  g1, g2, g3, g4, g5, g6,
  ncol = 3,
  nrow = 2,
  top = grid::textGrob("Vue d'ensemble des données - Marriage Data India (10,001 observations)",
                       gp = grid::gpar(fontsize = 14, fontface = "bold"))
)
dev.off()

cat("\n✓ Graphique sauvegardé: analyse_globale_mariages_inde.png\n")

# =============================================================================
# Affichage à l'écran
# =============================================================================
grid.arrange(
  g1, g2, g3, g4, g5, g6,
  ncol = 3,
  nrow = 2,
  top = grid::textGrob("Vue d'ensemble des données - Marriage Data India (10,001 observations)",
                       gp = grid::gpar(fontsize = 14, fontface = "bold"))
)

# =============================================================================
# Statistiques complémentaires
# =============================================================================
cat("\n=== STATISTIQUES DESCRIPTIVES PRINCIPALES ===\n\n")

cat("📊 DIMENSIONS\n")
cat("   - Observations:", nrow(data), "\n")
cat("   - Variables:", ncol(data), "\n\n")

cat("👥 ÂGE AU MARIAGE\n")
cat("   - Moyenne:", round(mean(data$Age_at_Marriage), 1), "ans\n")
cat("   - Médiane:", round(median(data$Age_at_Marriage), 1), "ans\n")
cat("   - Écart-type:", round(sd(data$Age_at_Marriage), 1), "ans\n\n")

cat("👶 NOMBRE D'ENFANTS\n")
cat("   - Moyenne:", round(mean(data$Children_Count), 1), "enfants\n")
cat("   - Médiane:", median(data$Children_Count), "enfants\n")
cat("   - Écart-type:", round(sd(data$Children_Count), 1), "\n\n")

cat("⏱️  DURÉE DU MARIAGE\n")
cat("   - Moyenne:", round(mean(data$Years_Since_Marriage), 1), "années\n")
cat("   - Médiane:", median(data$Years_Since_Marriage), "années\n\n")

cat("🌍 VARIABLES CULTURELLES\n")
cat("   - Approbation parentale:", 
    round(sum(data$Parental_Approval == "Yes") / nrow(data) * 100, 1), "%\n")
cat("   - Échange de dot:", 
    round(sum(data$Dowry_Exchanged == "Yes") / nrow(data) * 100, 1), "%\n")
cat("   - Mariages inter-castes:", 
    round(sum(data$`Inter-Caste` == "Yes") / nrow(data) * 100, 1), "%\n")
cat("   - Mariages inter-religieux:", 
    round(sum(data$`Inter-Religion` == "Yes") / nrow(data) * 100, 1), "%\n")
cat("   - Milieu urbain:", 
    round(sum(data$Urban_Rural == "Urban") / nrow(data) * 100, 1), "%\n")
cat("   - Milieu rural:", 
    round(sum(data$Urban_Rural == "Rural") / nrow(data) * 100, 1), "%\n\n")

cat("🕉️  RÉPARTITION RELIGIEUSE\n")
religion_stats <- data %>%
  group_by(Religion) %>%
  summarise(n = n(), pct = n() / nrow(data) * 100)
for (i in 1:nrow(religion_stats)) {
  cat("   -", religion_stats$Religion[i], ":", 
      round(religion_stats$pct[i], 1), "%\n")
}
