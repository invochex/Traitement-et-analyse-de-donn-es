# =============================================================================
# Script de création de l'archive pour le rendu
# =============================================================================

# Ce script génère le PDF et crée l'archive .tgz pour le rendu

# 1. Générer le rapport PDF
rmarkdown::render("rapport_NOM1_NOM2.Rmd", output_format = "pdf_document")

# 2. Créer l'archive .tgz
# Liste des fichiers à inclure
fichiers <- c(
  "rapport_NOM1_NOM2.pdf",      # Le rapport PDF
  "analyse_marriage_india.R",    # Le script R principal
  "rapport_NOM1_NOM2.Rmd",      # Le fichier RMarkdown source
  "marriage_data_india.csv"      # Les données
)

# Création de l'archive
tar("rendu_NOM1_NOM2.tgz", files = fichiers, compression = "gzip")

cat("Archive créée avec succès: rendu_NOM1_NOM2.tgz\n")
