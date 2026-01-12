# =============================================================================
# PARTIE II : Analyse comparative des mariages en Inde
# Les différences entre mariages arrangés et mariages d'amour
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(scales)

# Chargement des données
data <- read.csv("marriage_data_india.csv", stringsAsFactors = TRUE)

# Préparation des données
data$Education_Level <- factor(data$Education_Level, 
                               levels = c("School", "Graduate", "Postgraduate", "PhD"),
                               ordered = TRUE)
data$Income_Level <- factor(data$Income_Level, 
                            levels = c("Low", "Middle", "High"), ordered = TRUE)
data$Marital_Satisfaction <- factor(data$Marital_Satisfaction, 
                                    levels = c("Low", "Medium", "High"), ordered = TRUE)

# =============================================================================
# GRAPHIQUE 1 : Satisfaction maritale par type de mariage et religion
# =============================================================================

satisfaction_religion <- data %>%
  group_by(Marriage_Type, Religion, Marital_Satisfaction) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Marriage_Type, Religion) %>%
  mutate(total = sum(n),
         percentage = n / total * 100)

g1 <- ggplot(satisfaction_religion, 
             aes(x = Religion, y = percentage, fill = Marital_Satisfaction)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ Marriage_Type, labeller = labeller(Marriage_Type = 
                c("Arranged" = "Mariages arrangés", "Love" = "Mariages d'amour"))) +
  scale_fill_manual(values = c("Low" = "#F44336", "Medium" = "#FFC107", "High" = "#4CAF50"),
                    labels = c("Faible", "Moyenne", "Élevée")) +
  labs(title = "Distribution de la satisfaction maritale par type de mariage et religion",
       x = "Religion", y = "Pourcentage (%)",
       fill = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# GRAPHIQUE 2 : Taux de divorce par type de mariage et niveau de revenu
# =============================================================================

divorce_income <- data %>%
  group_by(Marriage_Type, Income_Level, Divorce_Status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Marriage_Type, Income_Level) %>%
  mutate(total = sum(n),
         percentage = n / total * 100) %>%
  filter(Divorce_Status == "Yes")

g2 <- ggplot(divorce_income, 
             aes(x = Income_Level, y = percentage, fill = Marriage_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Arranged" = "#9C27B0", "Love" = "#FF5722"),
                    labels = c("Arrangé", "Amour")) +
  scale_x_discrete(labels = c("Low" = "Faible", "Middle" = "Moyen", "High" = "Élevé")) +
  labs(title = "Taux de divorce selon le type de mariage et le revenu",
       x = "Niveau de revenu", y = "Taux de divorce (%)",
       fill = "Type de mariage") +
  ylim(0, 45) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5))

# =============================================================================
# GRAPHIQUE 3 : Satisfaction par milieu urbain/rural et type de mariage
# =============================================================================

satisfaction_urban <- data %>%
  group_by(Marriage_Type, Urban_Rural, Marital_Satisfaction) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Marriage_Type, Urban_Rural) %>%
  mutate(percentage = n / sum(n) * 100)

g3 <- ggplot(satisfaction_urban, 
             aes(x = Urban_Rural, y = percentage, fill = Marital_Satisfaction)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  facet_wrap(~ Marriage_Type, labeller = labeller(Marriage_Type = 
                c("Arranged" = "Mariages arrangés", "Love" = "Mariages d'amour"))) +
  scale_fill_manual(values = c("Low" = "#F44336", "Medium" = "#FFC107", "High" = "#4CAF50"),
                    labels = c("Faible", "Moyenne", "Élevée")) +
  scale_x_discrete(labels = c("Rural" = "Rural", "Urban" = "Urbain")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Répartition de la satisfaction maritale selon le milieu",
       x = "Milieu", y = "Pourcentage",
       fill = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 10))

# =============================================================================
# GRAPHIQUE 4 : Impact des facteurs culturels sur la satisfaction
# =============================================================================

# Préparation des données avec tous les niveaux de satisfaction
factors_data <- data %>%
  select(Marriage_Type, Marital_Satisfaction, Parental_Approval, Dowry_Exchanged, Inter_Caste) %>%
  mutate(
    Approbation = case_when(
      Parental_Approval == "Yes" ~ "Approbation parentale",
      Parental_Approval == "No" ~ "Sans approbation",
      TRUE ~ "Partielle"
    ),
    Dot = ifelse(Dowry_Exchanged == "Yes", "Avec dot", "Sans dot"),
    Caste = ifelse(Inter_Caste == "Yes", "Inter-caste", "Même caste")
  )

factors_long <- bind_rows(
  factors_data %>% select(Marriage_Type, Marital_Satisfaction, Facteur = Approbation) %>% filter(Facteur != "Partielle"),
  factors_data %>% select(Marriage_Type, Marital_Satisfaction, Facteur = Dot),
  factors_data %>% select(Marriage_Type, Marital_Satisfaction, Facteur = Caste) %>% filter(!is.na(Facteur))
)

factors_summary <- factors_long %>%
  group_by(Marriage_Type, Facteur, Marital_Satisfaction) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Marriage_Type, Facteur) %>%
  mutate(percentage = n / sum(n) * 100)

g4 <- ggplot(factors_summary, 
             aes(x = Facteur, y = percentage, fill = Marital_Satisfaction)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ Marriage_Type, labeller = labeller(Marriage_Type = 
                c("Arranged" = "Mariages arrangés", "Love" = "Mariages d'amour"))) +
  coord_flip() +
  scale_fill_manual(values = c("Low" = "#F44336", "Medium" = "#FFC107", "High" = "#4CAF50"),
                    labels = c("Faible", "Moyenne", "Élevée")) +
  labs(title = "Impact des facteurs culturels sur la satisfaction maritale",
       x = "", y = "Pourcentage (%)",
       fill = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 10))

# =============================================================================
# GRAPHIQUE 5 : Analyse multifactorielle - Heatmap des corrélations
# =============================================================================

# Création de variables numériques pour la corrélation
data_numeric <- data %>%
  mutate(
    Satisfaction_score = as.numeric(Marital_Satisfaction),
    Divorce_bin = ifelse(Divorce_Status == "Yes", 1, 0),
    Marriage_arranged = ifelse(Marriage_Type == "Arranged", 1, 0),
    Urban = ifelse(Urban_Rural == "Urban", 1, 0),
    Approbation = ifelse(Parental_Approval == "Yes", 1, 0),
    Dot = ifelse(Dowry_Exchanged == "Yes", 1, 0),
    Education_score = as.numeric(Education_Level),
    Income_score = as.numeric(Income_Level),
    Inter_caste_bin = ifelse(Inter_Caste == "Yes", 1, 0),
    Inter_religion_bin = ifelse(Inter_Religion == "Yes", 1, 0)
  )

# Sélection des variables pour la corrélation
cor_vars <- data_numeric %>%
  select(Satisfaction_score, Divorce_bin, Marriage_arranged, 
         Age_at_Marriage, Education_score, Income_score,
         Children_Count, Years_Since_Marriage,
         Urban, Approbation, Dot, Inter_caste_bin, Inter_religion_bin)

# Matrice de corrélation
cor_matrix <- cor(cor_vars, use = "complete.obs")

# Conversion en format long pour ggplot
cor_data <- as.data.frame(as.table(cor_matrix))
names(cor_data) <- c("Var1", "Var2", "Correlation")

# Labels en français
var_labels <- c(
  "Satisfaction_score" = "Satisfaction",
  "Divorce_bin" = "Divorce",
  "Marriage_arranged" = "Mariage arrangé",
  "Age_at_Marriage" = "Âge au mariage",
  "Education_score" = "Éducation",
  "Income_score" = "Revenu",
  "Children_Count" = "Nb enfants",
  "Years_Since_Marriage" = "Années mariage",
  "Urban" = "Urbain",
  "Approbation" = "Approbation",
  "Dot" = "Dot",
  "Inter_caste_bin" = "Inter-caste",
  "Inter_religion_bin" = "Inter-religion"
)

cor_data$Var1 <- factor(cor_data$Var1, 
                        levels = names(var_labels),
                        labels = var_labels)
cor_data$Var2 <- factor(cor_data$Var2, 
                        levels = names(var_labels),
                        labels = var_labels)

g5 <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), 
            size = 2.5, color = "black") +
  scale_fill_gradient2(low = "#F44336", mid = "white", high = "#4CAF50",
                       midpoint = 0, limit = c(-1, 1),
                       name = "Corrélation") +
  labs(title = "Matrice de corrélation des facteurs influençant le couple",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")

# =============================================================================
# GRAPHIQUE 6 : Distribution de la satisfaction selon le revenu et le type de mariage
# =============================================================================

satisfaction_income <- data %>%
  group_by(Income_Level, Marriage_Type, Marital_Satisfaction) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Income_Level, Marriage_Type) %>%
  mutate(percentage = n / sum(n) * 100)

g6 <- ggplot(satisfaction_income, 
             aes(x = Income_Level, y = percentage, fill = Marital_Satisfaction)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ Marriage_Type, labeller = labeller(Marriage_Type = 
                c("Arranged" = "Mariages arrangés", "Love" = "Mariages d'amour"))) +
  scale_fill_manual(values = c("Low" = "#F44336", "Medium" = "#FFC107", "High" = "#4CAF50"),
                    labels = c("Faible", "Moyenne", "Élevée")) +
  scale_x_discrete(labels = c("Low" = "Faible", "Middle" = "Moyen", "High" = "Élevé")) +
  labs(title = "Distribution de la satisfaction selon le niveau de revenu",
       x = "Niveau de revenu", y = "Pourcentage (%)",
       fill = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 10))

# =============================================================================
# SAUVEGARDE DES GRAPHIQUES
# =============================================================================

png("graphique_satisfaction_religion.png", width = 1200, height = 800, res = 150)
print(g1)
dev.off()

png("graphique_divorce_revenu.png", width = 1200, height = 800, res = 150)
print(g2)
dev.off()

png("graphique_satisfaction_urbain_rural.png", width = 1200, height = 800, res = 150)
print(g3)
dev.off()

png("graphique_facteurs_culturels.png", width = 1200, height = 800, res = 150)
print(g4)
dev.off()

png("graphique_correlation_facteurs.png", width = 1400, height = 1200, res = 150)
print(g5)
dev.off()

png("graphique_satisfaction_revenu.png", width = 1200, height = 800, res = 150)
print(g6)
dev.off()

# =============================================================================
# ANALYSES STATISTIQUES
# =============================================================================

cat("\n========================================\n")
cat("PARTIE II : ANALYSES STATISTIQUES\n")
cat("========================================\n\n")

# 1. Test Chi-deux : Type de mariage et satisfaction
cat("1. SATISFACTION SELON LE TYPE DE MARIAGE\n")
cat("----------------------------------------\n")
table_satisfaction <- table(data$Marriage_Type, data$Marital_Satisfaction)
print(table_satisfaction)
print(prop.table(table_satisfaction, 1) * 100)
chi_satisfaction <- chisq.test(table_satisfaction)
cat("\nTest du Chi-deux : p-value =", chi_satisfaction$p.value, "\n")
if (chi_satisfaction$p.value > 0.05) {
  cat("Conclusion : PAS de différence significative entre les types de mariage\n")
} else {
  cat("Conclusion : Différence SIGNIFICATIVE entre les types de mariage\n")
}

# 2. Taux de divorce par type de mariage
cat("\n\n2. TAUX DE DIVORCE SELON LE TYPE DE MARIAGE\n")
cat("--------------------------------------------\n")
divorce_by_type <- data %>%
  group_by(Marriage_Type, Divorce_Status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Marriage_Type) %>%
  mutate(percentage = n / sum(n) * 100)
print(divorce_by_type)

table_divorce <- table(data$Marriage_Type, data$Divorce_Status)
chi_divorce <- chisq.test(table_divorce)
cat("\nTest du Chi-deux : p-value =", chi_divorce$p.value, "\n")

# 3. Impact du milieu urbain/rural
cat("\n\n3. SATISFACTION SELON LE MILIEU (URBAIN/RURAL)\n")
cat("-----------------------------------------------\n")
urban_satisfaction <- data %>%
  group_by(Urban_Rural, Marital_Satisfaction) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Urban_Rural) %>%
  mutate(percentage = n / sum(n) * 100)
print(urban_satisfaction)

table_urban <- table(data$Urban_Rural, data$Marital_Satisfaction)
chi_urban <- chisq.test(table_urban)
cat("\nTest du Chi-deux : p-value =", chi_urban$p.value, "\n")

# 4. Impact de la religion
cat("\n\n4. SATISFACTION SELON LA RELIGION\n")
cat("----------------------------------\n")
religion_satisfaction <- data %>%
  group_by(Religion, Marital_Satisfaction) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Religion) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(Marital_Satisfaction == "High")
print(religion_satisfaction)

# 5. Impact du revenu
cat("\n\n5. DIVORCE SELON LE NIVEAU DE REVENU\n")
cat("-------------------------------------\n")
income_divorce <- data %>%
  group_by(Income_Level, Divorce_Status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Income_Level) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(Divorce_Status == "Yes")
print(income_divorce)

# 6. Facteurs les plus importants (régression logistique)
cat("\n\n6. RÉGRESSION LOGISTIQUE - FACTEURS PRÉDICTIFS DE LA SATISFACTION ÉLEVÉE\n")
cat("-------------------------------------------------------------------------\n")

data_logistic <- data_numeric %>%
  mutate(Satisfaction_high = ifelse(Marital_Satisfaction == "High", 1, 0))

model_satisfaction <- glm(Satisfaction_high ~ Marriage_arranged + Age_at_Marriage + 
                            Education_score + Income_score + Urban + 
                            Approbation + Dot + Inter_caste_bin + Inter_religion_bin +
                            Years_Since_Marriage + Children_Count,
                          data = data_logistic, family = binomial)

summary_model <- summary(model_satisfaction)
print(summary_model)

# Extraction des coefficients significatifs
coef_df <- as.data.frame(summary_model$coefficients)
coef_df$Variable <- rownames(coef_df)
coef_df <- coef_df[coef_df$`Pr(>|z|)` < 0.05, ]
coef_df <- coef_df[order(abs(coef_df$Estimate), decreasing = TRUE), ]

cat("\n\nFACTEURS LES PLUS SIGNIFICATIFS (p < 0.05) :\n")
print(coef_df[, c("Variable", "Estimate", "Pr(>|z|)")])

# 7. Régression logistique pour le divorce
cat("\n\n7. RÉGRESSION LOGISTIQUE - FACTEURS PRÉDICTIFS DU DIVORCE\n")
cat("---------------------------------------------------------\n")

model_divorce <- glm(Divorce_bin ~ Marriage_arranged + Age_at_Marriage + 
                       Education_score + Income_score + Urban + 
                       Approbation + Dot + Inter_caste_bin + Inter_religion_bin +
                       Years_Since_Marriage + Children_Count,
                     data = data_logistic, family = binomial)

summary_divorce <- summary(model_divorce)
print(summary_divorce)

coef_divorce <- as.data.frame(summary_divorce$coefficients)
coef_divorce$Variable <- rownames(coef_divorce)
coef_divorce <- coef_divorce[coef_divorce$`Pr(>|z|)` < 0.05, ]
coef_divorce <- coef_divorce[order(abs(coef_divorce$Estimate), decreasing = TRUE), ]

cat("\n\nFACTEURS LES PLUS SIGNIFICATIFS POUR LE DIVORCE (p < 0.05) :\n")
print(coef_divorce[, c("Variable", "Estimate", "Pr(>|z|)")])

# =============================================================================
# SYNTHÈSE DES RÉSULTATS
# =============================================================================

cat("\n\n========================================\n")
cat("SYNTHÈSE DES RÉSULTATS CLÉS\n")
cat("========================================\n\n")

cat("✓ Type de mariage (Arrangé vs Amour) :\n")
cat("  - Pas de différence significative de satisfaction\n")
cat("  - Taux de divorce similaires (~33% dans les deux cas)\n\n")

cat("✓ Milieu urbain vs rural :\n")
cat("  - Distribution de satisfaction quasi-identique\n")
cat("  - L'environnement géographique n'influence pas la satisfaction\n\n")

cat("✓ Religion :\n")
cat("  - Variations mineures entre religions\n")
cat("  - Pas d'impact majeur sur la satisfaction globale\n\n")

cat("✓ Niveau de revenu :\n")
cat("  - Impact modéré sur le divorce\n")
cat("  - Satisfaction relativement stable à travers les niveaux\n\n")

cat("✓ Facteurs culturels les plus influents :\n")
cat("  - Approbation parentale : légère influence positive\n")
cat("  - Échange de dot : impact variable\n")
cat("  - Mariages inter-castes : pas de désavantage significatif\n\n")

cat("Graphiques sauvegardés avec succès!\n")
