# تحميل مكتبة قراءة ملفات الإكسل
library(readxl)

# قراءة الملف وتخزينه في متغير اسمه df
df <- read_excel("DONNEES.xlsx")

# عرض الأسطر الستة الأولى من البيانات للتأكد من سلامتها
head(df)
# ====================================================================
# Estimation du Modèle de Régression Linéaire Multiple
# Résolution du bug Stargazer et Traduction en Français
# ====================================================================

# 1. Chargement des packages nécessaires
if(!require(readxl)) install.packages("readxl")
if(!require(broom)) install.packages("broom") # Alternative très stable à stargazer

library(readxl)
library(broom)

# 2. Lecture du fichier de données
df <- read_excel("DONNEES.xlsx")

# 3. Nettoyage des espaces cachés dans les noms de colonnes
colnames(df) <- trimws(colnames(df))

# 4. Sélection des variables et suppression des lignes contenant des NA
variables_cles <- c("taux_de_recouvrement", "CA_BT", "CA_HTA", "CA_HTB", "total_créances", "TAUX_DE_PERTES")
df_propre <- na.omit(df[, variables_cles])

# 5. Estimation du modèle (OLS)
modele_complet <- lm(taux_de_recouvrement ~ CA_BT + CA_HTA + CA_HTB + 
                       total_créances + TAUX_DE_PERTES, data = df_propre)

# 6. Extraction des résultats sous forme de tableau propre grâce à 'broom'
resultats <- tidy(modele_complet)

# 7. Traduction des colonnes et des lignes en Français
colnames(resultats) <- c("Variable", "Coefficient (Beta)", "Erreur-Type", "Statistique t", "P-value (Significativité)")

resultats$Variable <- c("(Constante)", 
                        "Chiffre d'Affaires BT (CA_BT)", 
                        "Chiffre d'Affaires HTA (CA_HTA)", 
                        "Chiffre d'Affaires HTB (CA_HTB)", 
                        "Total des Créances", 
                        "Taux de Pertes")

# 8. Affichage du tableau final des coefficients en Français
cat("\n--- TABLEAU DES COEFFICIENTS (EN FRANÇAIS) ---\n")
print(as.data.frame(resultats), row.names = FALSE)

# 9. Extraction et affichage des métriques globales du modèle (R², R² ajusté, etc.)
cat("\n--- METRIQUES GLOBALES DU MODELE ---\n")
metriques <- glance(modele_complet)
cat("Coefficient de détermination (R2) :", metriques$r.squared, "\n")
cat("R2 Ajusté :", metriques$adj.r.squared, "\n")
cat("Statistique F (Fisher) :", metriques$statistic, "\n")
cat("P-value du modèle globale :", metriques$p.value, "\n")
cat("Nombre d'observations (Mois) :", metriques$nobs, "\n")
# 1. Test de Normalité des résidus (Shapiro-Wilk)
shapiro.test(residuals(modele_complet))

# 2. Test d'autocorrélation des résidus (Durbin-Watson)
# (Nécessite le package 'lmtest' : install.packages("lmtest"))
library(lmtest)
dwtest(modele_complet)

