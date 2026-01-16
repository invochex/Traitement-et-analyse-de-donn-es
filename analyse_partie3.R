# =============================================================================
# PARTIE III : Analyse de l'écart d'âge entre hommes et femmes
# Comparaison entre mariages arrangés et mariages d'amour
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Chargement des données
data <- read.csv("marriage_data_india.csv", stringsAsFactors = TRUE)

# Sélection des variables pertinentes
data_age <- data %>%
  select(Marriage_Type, Gender, Age_at_Marriage)

# =============================================================================
# CALCUL DES ÉCARTS D'ÂGE
# =============================================================================

# Statistiques descriptives par type de mariage et genre
age_stats <- data_age %>%
  group_by(Marriage_Type, Gender) %>%
  summarise(
    age_moyen = mean(Age_at_Marriage),
    age_median = median(Age_at_Marriage),
    age_min = min(Age_at_Marriage),
    age_max = max(Age_at_Marriage),
    ecart_type = sd(Age_at_Marriage),
    n = n(),
    .groups = "drop"
  )

# Calcul de l'écart d'âge moyen entre hommes et femmes
ecart_age <- age_stats %>%
  select(Marriage_Type, Gender, age_moyen) %>%
  pivot_wider(names_from = Gender, values_from = age_moyen) %>%
  mutate(ecart_age_moyen = Male - Female)

# =============================================================================
# GRAPHIQUE 1 : Distribution des âges par type de mariage et genre
# =============================================================================

g1 <- ggplot(data_age, aes(x = Age_at_Marriage, fill = Gender)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  facet_wrap(~ Marriage_Type, labeller = labeller(Marriage_Type = 
                c("Arranged" = "Mariages arrangés", "Love" = "Mariages d'amour"))) +
  scale_fill_manual(values = c("Male" = "#2196F3", "Female" = "#E91E63"),
                    labels = c("Femme", "Homme")) +
  labs(title = "Distribution des âges au mariage selon le type de mariage",
       x = "Âge au mariage", y = "Nombre de personnes",
       fill = "Genre") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 11))

# =============================================================================
# GRAPHIQUE 2 : Boîtes à moustaches des âges
# =============================================================================

g2 <- ggplot(data_age, aes(x = Gender, y = Age_at_Marriage, fill = Gender)) +
  geom_boxplot(width = 0.5, alpha = 0.7) +
  facet_wrap(~ Marriage_Type, labeller = labeller(Marriage_Type = 
                c("Arranged" = "Mariages arrangés", "Love" = "Mariages d'amour"))) +
  scale_fill_manual(values = c("Male" = "#2196F3", "Female" = "#E91E63"),
                    labels = c("Femme", "Homme")) +
  scale_x_discrete(labels = c("Female" = "Femme", "Male" = "Homme")) +
  labs(title = "Comparaison des âges au mariage par type et genre",
       x = "Genre", y = "Âge au mariage",
       fill = "Genre") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 11))

# =============================================================================
# GRAPHIQUE 3 : Âges moyens par type de mariage
# =============================================================================

g3 <- ggplot(age_stats, aes(x = Marriage_Type, y = age_moyen, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = age_moyen - ecart_type, ymax = age_moyen + ecart_type),
                position = position_dodge(width = 0.8), width = 0.3) +
  geom_text(aes(label = paste0(round(age_moyen, 1), " ans")),
            position = position_dodge(width = 0.8), vjust = -2, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Male" = "#2196F3", "Female" = "#E91E63"),
                    labels = c("Femme", "Homme")) +
  scale_x_discrete(labels = c("Arranged" = "Mariages arrangés", "Love" = "Mariages d'amour")) +
  labs(title = "Âge moyen au mariage selon le type et le genre",
       x = "Type de mariage", y = "Âge moyen (années)",
       fill = "Genre") +
  ylim(0, 35) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5))

# =============================================================================
# GRAPHIQUE 4 : Écart d'âge moyen entre hommes et femmes
# =============================================================================

ecart_age_plot <- ecart_age %>%
  mutate(Marriage_Type_fr = ifelse(Marriage_Type == "Arranged", 
                                    "Mariages arrangés", "Mariages d'amour"))

g4 <- ggplot(ecart_age_plot, aes(x = Marriage_Type_fr, y = ecart_age_moyen, fill = Marriage_Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(ecart_age_moyen, 2), " ans")),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Arranged" = "#9C27B0", "Love" = "#FF5722")) +
  labs(title = "Écart d'âge moyen entre hommes et femmes",
       subtitle = "Différence : Âge moyen homme - Âge moyen femme",
       x = "Type de mariage", y = "Écart d'âge moyen (années)") +
  ylim(0, max(ecart_age_plot$ecart_age_moyen) * 1.2) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"))

# =============================================================================
# GRAPHIQUE 5 : Densité des âges par type de mariage
# =============================================================================

g5 <- ggplot(data_age, aes(x = Age_at_Marriage, fill = Gender)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Marriage_Type, labeller = labeller(Marriage_Type = 
                c("Arranged" = "Mariages arrangés", "Love" = "Mariages d'amour"))) +
  scale_fill_manual(values = c("Male" = "#2196F3", "Female" = "#E91E63"),
                    labels = c("Femme", "Homme")) +
  labs(title = "Courbes de densité des âges au mariage",
       x = "Âge au mariage", y = "Densité",
       fill = "Genre") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 11))

# =============================================================================
# SAUVEGARDE DES GRAPHIQUES
# =============================================================================

png("graphique_distribution_ages.png", width = 1200, height = 800, res = 150)
print(g1)
dev.off()

png("graphique_boxplot_ages.png", width = 1200, height = 800, res = 150)
print(g2)
dev.off()

png("graphique_ages_moyens.png", width = 1200, height = 800, res = 150)
print(g3)
dev.off()

png("graphique_ecart_age.png", width = 1000, height = 700, res = 150)
print(g4)
dev.off()

png("graphique_densite_ages.png", width = 1200, height = 800, res = 150)
print(g5)
dev.off()

# =============================================================================
# ANALYSES STATISTIQUES
# =============================================================================

cat("\n========================================\n")
cat("PARTIE III : ANALYSE DE L'ÉCART D'ÂGE\n")
cat("========================================\n\n")

# 1. Statistiques descriptives
cat("1. STATISTIQUES DESCRIPTIVES PAR TYPE DE MARIAGE ET GENRE\n")
cat("----------------------------------------------------------\n")
print(age_stats)

# 2. Écart d'âge entre hommes et femmes
cat("\n\n2. ÉCART D'ÂGE MOYEN ENTRE HOMMES ET FEMMES\n")
cat("--------------------------------------------\n")
print(ecart_age)

# 3. Test t pour comparer les âges des hommes entre les deux types de mariage
cat("\n\n3. TEST T - COMPARAISON DES ÂGES DES HOMMES\n")
cat("--------------------------------------------\n")
age_homme_arranged <- data_age %>% 
  filter(Marriage_Type == "Arranged", Gender == "Male") %>% 
  pull(Age_at_Marriage)

age_homme_love <- data_age %>% 
  filter(Marriage_Type == "Love", Gender == "Male") %>% 
  pull(Age_at_Marriage)

t_test_homme <- t.test(age_homme_arranged, age_homme_love)
cat("Âge moyen hommes (Arrangé) :", round(mean(age_homme_arranged), 2), "ans\n")
cat("Âge moyen hommes (Amour) :", round(mean(age_homme_love), 2), "ans\n")
cat("p-value :", t_test_homme$p.value, "\n")
if (t_test_homme$p.value < 0.05) {
  cat("Conclusion : Différence SIGNIFICATIVE entre les types de mariage\n")
} else {
  cat("Conclusion : PAS de différence significative entre les types de mariage\n")
}

# 4. Test t pour comparer les âges des femmes entre les deux types de mariage
cat("\n\n4. TEST T - COMPARAISON DES ÂGES DES FEMMES\n")
cat("--------------------------------------------\n")
age_femme_arranged <- data_age %>% 
  filter(Marriage_Type == "Arranged", Gender == "Female") %>% 
  pull(Age_at_Marriage)

age_femme_love <- data_age %>% 
  filter(Marriage_Type == "Love", Gender == "Female") %>% 
  pull(Age_at_Marriage)

t_test_femme <- t.test(age_femme_arranged, age_femme_love)
cat("Âge moyen femmes (Arrangé) :", round(mean(age_femme_arranged), 2), "ans\n")
cat("Âge moyen femmes (Amour) :", round(mean(age_femme_love), 2), "ans\n")
cat("p-value :", t_test_femme$p.value, "\n")
if (t_test_femme$p.value < 0.05) {
  cat("Conclusion : Différence SIGNIFICATIVE entre les types de mariage\n")
} else {
  cat("Conclusion : PAS de différence significative entre les types de mariage\n")
}

# 5. Test pour comparer l'écart d'âge entre les deux types de mariage
cat("\n\n5. COMPARAISON DE L'ÉCART D'ÂGE\n")
cat("--------------------------------\n")
cat("Écart d'âge moyen (Mariages arrangés) :", round(ecart_age$ecart_age_moyen[ecart_age$Marriage_Type == "Arranged"], 2), "ans\n")
cat("Écart d'âge moyen (Mariages d'amour) :", round(ecart_age$ecart_age_moyen[ecart_age$Marriage_Type == "Love"], 2), "ans\n")

diff_ecart <- ecart_age$ecart_age_moyen[ecart_age$Marriage_Type == "Arranged"] - 
              ecart_age$ecart_age_moyen[ecart_age$Marriage_Type == "Love"]

cat("\nDifférence d'écart d'âge :", round(diff_ecart, 2), "ans\n")

if (abs(diff_ecart) > 1) {
  cat("Conclusion : L'écart d'âge diffère notablement entre les deux types de mariage\n")
} else {
  cat("Conclusion : L'écart d'âge est similaire dans les deux types de mariage\n")
}

# =============================================================================
# SYNTHÈSE
# =============================================================================

cat("\n\n========================================\n")
cat("SYNTHÈSE DES RÉSULTATS\n")
cat("========================================\n\n")

cat("✓ Mariages arrangés :\n")
cat("  - Âge moyen hommes :", round(age_stats$age_moyen[age_stats$Marriage_Type == "Arranged" & age_stats$Gender == "Male"], 2), "ans\n")
cat("  - Âge moyen femmes :", round(age_stats$age_moyen[age_stats$Marriage_Type == "Arranged" & age_stats$Gender == "Female"], 2), "ans\n")
cat("  - Écart d'âge moyen :", round(ecart_age$ecart_age_moyen[ecart_age$Marriage_Type == "Arranged"], 2), "ans\n\n")

cat("✓ Mariages d'amour :\n")
cat("  - Âge moyen hommes :", round(age_stats$age_moyen[age_stats$Marriage_Type == "Love" & age_stats$Gender == "Male"], 2), "ans\n")
cat("  - Âge moyen femmes :", round(age_stats$age_moyen[age_stats$Marriage_Type == "Love" & age_stats$Gender == "Female"], 2), "ans\n")
cat("  - Écart d'âge moyen :", round(ecart_age$ecart_age_moyen[ecart_age$Marriage_Type == "Love"], 2), "ans\n\n")

cat("Graphiques sauvegardés avec succès!\n")
