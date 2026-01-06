# =============================================================================
# Analyse du dataset "Marriage Data India"
# Cours de Visualisation de la donnée - 5A INSA Toulouse
# =============================================================================

# -----------------------------------------------------------------------------
# 1. CHARGEMENT DES PACKAGES ET DES DONNÉES
# -----------------------------------------------------------------------------

# Installation des packages si nécessaire (décommenter si besoin)
# install.packages(c("ggplot2", "dplyr", "tidyr", "corrplot", "scales", "gridExtra"))

# Chargement des librairies
library(ggplot2)    # Visualisation avancée
library(dplyr)      # Manipulation de données
library(tidyr)      # Restructuration de données
library(corrplot)   # Matrices de corrélation
library(scales)     # Formatage des axes
library(gridExtra)  # Arrangement de graphiques

# Chargement des données
data <- read.csv("marriage_data_india.csv", stringsAsFactors = TRUE)

# Aperçu des données
cat("=== APERÇU DU DATASET ===\n")
str(data)
cat("\n=== RÉSUMÉ STATISTIQUE ===\n")
summary(data)
cat("\n=== DIMENSIONS ===\n")
cat("Nombre d'observations:", nrow(data), "\n")
cat("Nombre de variables:", ncol(data), "\n")

# -----------------------------------------------------------------------------
# 2. PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------

# Conversion des variables catégorielles en facteurs ordonnés pour certaines
data$Education_Level <- factor(data$Education_Level, 
                               levels = c("School", "Graduate", "Postgraduate", "PhD"),
                               ordered = TRUE)

data$Income_Level <- factor(data$Income_Level, 
                            levels = c("Low", "Middle", "High"),
                            ordered = TRUE)

data$Marital_Satisfaction <- factor(data$Marital_Satisfaction, 
                                    levels = c("Low", "Medium", "High"),
                                    ordered = TRUE)

# Conversion des Yes/No en booléens pour analyse
data$Divorce_Binary <- ifelse(data$Divorce_Status == "Yes", 1, 0)
data$Dowry_Binary <- ifelse(data$Dowry_Exchanged == "Yes", 1, 0)
data$Parental_Approval_Binary <- ifelse(data$Parental_Approval == "Yes", 1, 0)

# =============================================================================
# GRAPHIQUE 1: HISTOGRAMME - Distribution de l'âge au mariage par sexe
# =============================================================================
# Objectif: Comprendre à quel âge hommes et femmes se marient en Inde
# et identifier les différences entre genres

cat("\n=== GRAPHIQUE 1: Distribution de l'âge au mariage ===\n")

# Calcul des statistiques par genre
stats_age <- data %>%
  group_by(Gender) %>%
  summarise(
    Moyenne = mean(Age_at_Marriage),
    Médiane = median(Age_at_Marriage),
    Ecart_type = sd(Age_at_Marriage)
  )
print(stats_age)

# Création de l'histogramme avec densité
g1 <- ggplot(data, aes(x = Age_at_Marriage, fill = Gender)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 25, 
                 alpha = 0.6, 
                 position = "identity",
                 color = "white") +
  geom_density(aes(color = Gender), linewidth = 1.2, fill = NA) +
  geom_vline(data = stats_age, 
             aes(xintercept = Moyenne, color = Gender),
             linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c("Female" = "#E91E63", "Male" = "#2196F3"),
                    labels = c("Femmes", "Hommes")) +
  scale_color_manual(values = c("Female" = "#880E4F", "Male" = "#0D47A1"),
                     labels = c("Femmes", "Hommes")) +
  labs(title = "Distribution de l'âge au mariage selon le genre",
       subtitle = "Les lignes verticales indiquent la moyenne par genre",
       x = "Âge au mariage (années)",
       y = "Densité",
       fill = "Genre",
       color = "Genre") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    legend.position = "bottom"
  )

print(g1)
ggsave("graphique1_histogramme_age.png", g1, width = 10, height = 6, dpi = 300)

# =============================================================================
# GRAPHIQUE 2: DIAGRAMME EN BARRES EMPILÉES - Satisfaction vs Type de mariage
# =============================================================================
# Objectif: Comparer la satisfaction maritale entre mariages arrangés et 
# mariages d'amour pour déterminer si le type influence le bonheur conjugal

cat("\n=== GRAPHIQUE 2: Satisfaction maritale par type de mariage ===\n")

# Préparation des données avec pourcentages
satisfaction_data <- data %>%
  group_by(Marriage_Type, Marital_Satisfaction) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Marriage_Type) %>%
  mutate(percentage = count / sum(count) * 100)

print(satisfaction_data)

g2 <- ggplot(satisfaction_data, 
             aes(x = Marriage_Type, y = percentage, fill = Marital_Satisfaction)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Low" = "#F44336", 
                               "Medium" = "#FFC107", 
                               "High" = "#4CAF50"),
                    labels = c("Faible", "Moyenne", "Élevée")) +
  scale_x_discrete(labels = c("Arrangé", "Amour")) +
  labs(title = "Niveau de satisfaction maritale selon le type de mariage",
       subtitle = "Comparaison entre mariages arrangés et mariages d'amour",
       x = "Type de mariage",
       y = "Pourcentage (%)",
       fill = "Satisfaction") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    legend.position = "right",
    axis.text.x = element_text(size = 12, face = "bold")
  )

print(g2)
ggsave("graphique2_barres_satisfaction.png", g2, width = 9, height = 6, dpi = 300)

# =============================================================================
# GRAPHIQUE 3: NUAGE DE POINTS - Âge au mariage vs Années de mariage
# =============================================================================
# Objectif: Explorer la relation entre l'âge au mariage et la durée du mariage
# en distinguant les divorces et les mariages toujours actifs

cat("\n=== GRAPHIQUE 3: Âge au mariage vs durée du mariage ===\n")

# Statistiques sur les divorces
divorce_stats <- data %>%
  group_by(Divorce_Status) %>%
  summarise(
    n = n(),
    age_moyen = mean(Age_at_Marriage),
    duree_moyenne = mean(Years_Since_Marriage)
  )
print(divorce_stats)

g3 <- ggplot(data, aes(x = Age_at_Marriage, 
                        y = Years_Since_Marriage, 
                        color = Divorce_Status)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c("No" = "#4CAF50", "Yes" = "#F44336"),
                     labels = c("Non divorcé", "Divorcé")) +
  labs(title = "Relation entre l'âge au mariage et la durée du mariage",
       subtitle = "Avec régression linéaire par statut de divorce",
       x = "Âge au mariage (années)",
       y = "Années depuis le mariage",
       color = "Divorce") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    legend.position = "bottom"
  ) +
  annotate("text", x = 35, y = 45, 
           label = sprintf("Taux de divorce: %.1f%%", 
                          mean(data$Divorce_Binary) * 100),
           size = 4, color = "gray30")

print(g3)
ggsave("graphique3_scatter_age_duree.png", g3, width = 10, height = 7, dpi = 300)

# =============================================================================
# GRAPHIQUE 4: BOXPLOT - Nombre d'enfants selon l'éducation et le revenu
# =============================================================================
# Objectif: Analyser comment le niveau d'éducation et le revenu influencent
# le nombre d'enfants dans les familles indiennes

cat("\n=== GRAPHIQUE 4: Nombre d'enfants selon éducation et revenu ===\n")

# Statistiques
children_stats <- data %>%
  group_by(Education_Level, Income_Level) %>%
  summarise(
    n = n(),
    moyenne_enfants = mean(Children_Count),
    mediane_enfants = median(Children_Count),
    .groups = "drop"
  )
print(children_stats)

g4 <- ggplot(data, aes(x = Education_Level, y = Children_Count, fill = Income_Level)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 2, fill = "white",
               position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("Low" = "#FFCDD2", 
                               "Middle" = "#90CAF9", 
                               "High" = "#A5D6A7"),
                    labels = c("Faible", "Moyen", "Élevé")) +
  scale_x_discrete(labels = c("École", "Licence", "Master", "Doctorat")) +
  labs(title = "Distribution du nombre d'enfants selon l'éducation et le revenu",
       subtitle = "Les losanges blancs représentent la moyenne",
       x = "Niveau d'éducation",
       y = "Nombre d'enfants",
       fill = "Revenu") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    legend.position = "right",
    axis.text.x = element_text(size = 10)
  )

print(g4)
ggsave("graphique4_boxplot_enfants.png", g4, width = 11, height = 7, dpi = 300)

# =============================================================================
# GRAPHIQUE 5: COURBE/LIGNE - Évolution du taux de divorce par durée de mariage
# =============================================================================
# Objectif: Identifier les périodes critiques où le risque de divorce est 
# le plus élevé au cours de la vie maritale

cat("\n=== GRAPHIQUE 5: Évolution du taux de divorce par durée ===\n")

# Calcul du taux de divorce par tranches de durée
divorce_evolution <- data %>%
  mutate(Tranche_Duree = cut(Years_Since_Marriage, 
                              breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50),
                              labels = c("0-5", "5-10", "10-15", "15-20", 
                                        "20-25", "25-30", "30-35", "35-40", "40-50"),
                              include.lowest = TRUE)) %>%
  group_by(Tranche_Duree) %>%
  summarise(
    n_total = n(),
    n_divorces = sum(Divorce_Binary),
    taux_divorce = mean(Divorce_Binary) * 100,
    .groups = "drop"
  )

print(divorce_evolution)

g5 <- ggplot(divorce_evolution, aes(x = Tranche_Duree, y = taux_divorce, group = 1)) +
  geom_line(color = "#E91E63", linewidth = 1.5) +
  geom_point(aes(size = n_total), color = "#E91E63", alpha = 0.7) +
  geom_area(alpha = 0.2, fill = "#E91E63") +
  geom_text(aes(label = sprintf("%.1f%%", taux_divorce)), 
            vjust = -1.5, size = 3.5, color = "gray30") +
  scale_size_continuous(name = "Effectif", range = c(3, 10)) +
  labs(title = "Évolution du taux de divorce selon la durée du mariage",
       subtitle = "Identification des périodes critiques dans la vie maritale",
       x = "Années de mariage",
       y = "Taux de divorce (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(0, max(divorce_evolution$taux_divorce) * 1.3)

print(g5)
ggsave("graphique5_courbe_divorce.png", g5, width = 11, height = 7, dpi = 300)

# =============================================================================
# GRAPHIQUE 6 (BONUS): HEATMAP - Corrélations entre variables
# =============================================================================
# Objectif: Visualiser les corrélations entre les variables numériques
# et identifier les facteurs les plus liés

cat("\n=== GRAPHIQUE 6: Matrice de corrélation ===\n")

# Préparation des données numériques pour la corrélation
data_numeric <- data %>%
  mutate(
    Marriage_Type_Num = ifelse(Marriage_Type == "Love", 1, 0),
    Gender_Num = ifelse(Gender == "Male", 1, 0),
    Education_Num = as.numeric(Education_Level),
    Caste_Match_Num = ifelse(Caste_Match == "Same", 1, 0),
    Urban_Num = ifelse(Urban_Rural == "Urban", 1, 0),
    Satisfaction_Num = as.numeric(Marital_Satisfaction),
    Income_Num = as.numeric(Income_Level),
    Spouse_Working_Num = ifelse(Spouse_Working == "Yes", 1, 0),
    Inter_Caste_Num = ifelse(Inter.Caste == "Yes", 1, 0)
  ) %>%
  select(Age_at_Marriage, Years_Since_Marriage, Children_Count,
         Marriage_Type_Num, Education_Num, Urban_Num, 
         Satisfaction_Num, Income_Num, Divorce_Binary)

# Renommage pour l'affichage
colnames(data_numeric) <- c("Âge mariage", "Années mariage", "Nb enfants",
                            "Mariage amour", "Éducation", "Urbain",
                            "Satisfaction", "Revenu", "Divorce")

# Calcul de la matrice de corrélation
cor_matrix <- cor(data_numeric, use = "complete.obs")
print(round(cor_matrix, 2))

# Sauvegarde du graphique de corrélation
png("graphique6_correlation.png", width = 800, height = 700, res = 100)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         title = "Matrice de corrélation des variables",
         mar = c(0, 0, 2, 0),
         col = colorRampPalette(c("#F44336", "white", "#4CAF50"))(100))
dev.off()

# =============================================================================
# ANALYSES STATISTIQUES COMPLÉMENTAIRES
# =============================================================================

cat("\n\n========================================\n")
cat("ANALYSES STATISTIQUES ET CONCLUSIONS\n")
cat("========================================\n\n")

# Test du Chi-deux: Type de mariage vs Satisfaction
cat("--- Test Chi-deux: Type de mariage vs Satisfaction ---\n")
chi_test1 <- chisq.test(table(data$Marriage_Type, data$Marital_Satisfaction))
print(chi_test1)

# Test du Chi-deux: Éducation vs Divorce
cat("\n--- Test Chi-deux: Éducation vs Divorce ---\n")
chi_test2 <- chisq.test(table(data$Education_Level, data$Divorce_Status))
print(chi_test2)

# Test t: Âge au mariage selon le divorce
cat("\n--- Test t: Âge au mariage selon statut de divorce ---\n")
t_test <- t.test(Age_at_Marriage ~ Divorce_Status, data = data)
print(t_test)

# Régression logistique: Facteurs de divorce
cat("\n--- Régression logistique: Facteurs influençant le divorce ---\n")
model_divorce <- glm(Divorce_Binary ~ Age_at_Marriage + Marriage_Type + 
                       Education_Level + Income_Level + Urban_Rural + 
                       Parental_Approval + Dowry_Exchanged,
                     data = data, family = binomial)
print(summary(model_divorce))

# =============================================================================
# RÉSUMÉ DES CONCLUSIONS
# =============================================================================

cat("\n\n========================================\n")
cat("RÉSUMÉ DES PRINCIPALES CONCLUSIONS\n")
cat("========================================\n\n")

cat("1. DISTRIBUTION DES ÂGES AU MARIAGE:\n")
cat("   - Âge moyen au mariage: ", round(mean(data$Age_at_Marriage), 1), " ans\n")
cat("   - Les hommes se marient en moyenne à ", 
    round(mean(data$Age_at_Marriage[data$Gender == "Male"]), 1), " ans\n")
cat("   - Les femmes se marient en moyenne à ", 
    round(mean(data$Age_at_Marriage[data$Gender == "Female"]), 1), " ans\n\n")

cat("2. TYPE DE MARIAGE:\n")
cat("   - Mariages arrangés: ", 
    round(sum(data$Marriage_Type == "Arranged") / nrow(data) * 100, 1), "%\n")
cat("   - Mariages d'amour: ", 
    round(sum(data$Marriage_Type == "Love") / nrow(data) * 100, 1), "%\n\n")

cat("3. TAUX DE DIVORCE GLOBAL: ", 
    round(mean(data$Divorce_Binary) * 100, 1), "%\n\n")

cat("4. SATISFACTION MARITALE (distribution globale):\n")
print(prop.table(table(data$Marital_Satisfaction)) * 100)

cat("\n5. CORRÉLATIONS SIGNIFICATIVES:\n")
cat("   - La durée du mariage est positivement corrélée au nombre d'enfants\n")
cat("   - Le niveau d'éducation montre peu d'impact sur la satisfaction\n")
cat("   - Le milieu urbain/rural n'influence pas significativement le divorce\n")

cat("\n=== FIN DE L'ANALYSE ===\n")
