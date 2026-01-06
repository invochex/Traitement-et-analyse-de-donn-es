# Instructions pour le rendu du projet

## Fichiers créés

1. **analyse_marriage_india.R** - Script R principal contenant :
   - Chargement et préparation des données
   - 6 graphiques commentés (histogramme, barres empilées, nuage de points, boxplot, courbe, heatmap)
   - Tests statistiques (Chi-deux, test t, régression logistique)
   - Conclusions détaillées

2. **rapport_NOM1_NOM2.Rmd** - Rapport RMarkdown générant le PDF de 5 pages avec :
   - 5 graphiques variés et commentés
   - Analyses et interprétations
   - Conclusions

3. **creer_archive.R** - Script pour générer l'archive finale

## Étapes pour générer le rendu

### Prérequis
Installer les packages R nécessaires :
```r
install.packages(c("ggplot2", "dplyr", "tidyr", "corrplot", "scales", "gridExtra", "rmarkdown", "knitr"))
```

### Étape 1 : Générer le PDF
Dans RStudio ou R :
```r
setwd("c:/Users/hilmi/Desktop/Cours_5A/Visualisation de la donnée/archive")
rmarkdown::render("rapport_NOM1_NOM2.Rmd")
```

### Étape 2 : Créer l'archive .tgz
Option 1 - Via R :
```r
source("creer_archive.R")
```

Option 2 - Via ligne de commande (Git Bash ou WSL) :
```bash
tar -czvf rendu_NOM1_NOM2.tgz rapport_NOM1_NOM2.pdf analyse_marriage_india.R marriage_data_india.csv
```

Option 3 - Via PowerShell avec 7-Zip :
```powershell
7z a -ttar rendu_NOM1_NOM2.tar rapport_NOM1_NOM2.pdf analyse_marriage_india.R marriage_data_india.csv
7z a -tgzip rendu_NOM1_NOM2.tgz rendu_NOM1_NOM2.tar
```

## Important : Renommer les fichiers
Avant de rendre :
1. Remplacer `NOM1` et `NOM2` par vos noms de famille dans :
   - `rapport_NOM1_NOM2.Rmd` (ligne author)
   - Nom du fichier PDF généré
   - Nom de l'archive finale

## Contenu de l'archive finale
- `rapport_NOM1_NOM2.pdf` (5 pages max, 5+ graphiques commentés)
- `analyse_marriage_india.R` (script R commenté)
- `marriage_data_india.csv` (données source)

## Types de graphiques utilisés
1. **Histogramme** avec densité - Distribution de l'âge au mariage
2. **Diagramme en barres empilées** - Satisfaction par type de mariage
3. **Nuage de points** avec régression - Âge vs durée du mariage
4. **Boxplot** - Nombre d'enfants selon éducation/revenu
5. **Courbe/Ligne** avec aire - Évolution du taux de divorce
6. **Heatmap** (bonus) - Matrice de corrélation
