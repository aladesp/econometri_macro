rm(list=ls())

install.packages("readxl")
install.packages("urca")
install.packages("tsDyn")
install.packages("ggplot2")

library(readxl)    # Lire fichiers Excel
library(vars)      # Estimer VAR/SVAR
library(urca)      # Test ADF (stationnarité)
library(tsDyn)     # Pour d'autres modèles VAR
library(ggplot2)   # (optionnel) pour jolis graphes

# Importer le feuillet "Monthly"
data <- read_excel("/Users/alanisdespland/Desktop/Projet macro/Monetarydat.xlsx", sheet = "Monthly")
petrole <- read_excel("/Users/alanisdespland/Desktop/DCOILWTICO.xlsx", sheet = "Monthly")

dataset <- data[, c("DATES","LIP", "UNEMP", "LCPI", "FFR", "RRSHOCK", 
                    "rrshock83", "BCSHOCK", "MP1_TC", "FF4_TC", "ED2_TC", 
                    "ED3_TC", "ED4_TC", "szshock")]
# 9 indice de shock

# TRANSFORMER LES DATES 
install.packages("zoo")
library(zoo)
DATES <- dataset$DATES
# Convert decimal to "yearmon" then to Date (start of the month)
date_vector <- as.Date(as.yearmon(DATES), frac = 0)
# Replace the decimal data
dataset$DATES <- date_vector

#MERGE dataset avec le pétrole 
# S'assurer que la date de petrole est au format Date
petrole$DATES <- as.Date(petrole$observation_date)
# Fusion des données sur la date
dataset_with_petrole <- merge(dataset, petrole, by = "DATES", all = FALSE)
head(dataset_with_petrole)

# Supprimer les lignes avec NA
DATA_base <- na.omit(dataset_with_petrole)
#on a une base de donnée de 1990 a 2003 


#VISUALISATION DES INDICES 
# === 1. Préparation ===
shocks <- c("RRSHOCK", "rrshock83", "BCSHOCK", "MP1_TC", "FF4_TC", 
            "ED2_TC", "ED3_TC", "ED4_TC", "szshock") 
dates <- DATA_base$DATES

# === 2. Créer un dossier de sortie ===
dir.create("indices_graph", showWarnings = FALSE)

# === 3. Boucle sur chaque choc ===
for (shock in shocks) {
  
  # Extraction de la série
  serie <- DATA_base[[shock]]
  
  # Définition du nom de fichier
  filename <- paste0("indices_graph/", shock, ".png")
  
  # Ouvrir un fichier PNG
  png(filename, width = 1000, height = 600, res = 150)
  
  # === Graphique stylé ===
  plot(dates, serie, type = "l", col = "#1f77b4", lwd = 2,
       main = paste0("Indice de choc : ", shock),
       xlab = "Temps", ylab = shock,
       cex.main = 1.4, cex.lab = 1.2, cex.axis = 1,
       las = 1, bty = "n")

  
  # === Grille légère ===
  grid(col = "grey90", lty = "dotted")
  
  # Fermer le fichier
  dev.off()
}

#VISUALISATION DES VARIABLES 
# === 1. Liste des variables à tracer (hors chocs)
variables <- c("LIP", "UNEMP", "LCPI", "FFR", "DCOILWTICO")
dates <- DATA_base$DATES

# === 2. Créer un dossier de sortie
dir.create("variables_graph", showWarnings = FALSE)

# === 3. Boucle de visualisation stylée
for (var in variables) {
  
  serie <- DATA_base[[var]]
  filename <- paste0("variables_graph/", var, ".png")
  
  png(filename, width = 1000, height = 600, res = 150)
  
  plot(dates, serie, type = "l", col = "#1f77b4", lwd = 2,
       main = paste0("Évolution de ", var),
       xlab = "Temps", ylab = var,
       cex.main = 1.4, cex.lab = 1.2, cex.axis = 1,
       las = 1, bty = "n")
  
  grid(col = "grey90", lty = "dotted")
  
  dev.off()
}

#STATIONARITE test ADF
# Test ADF VC = -3.42 à 5%
library(tseries)
adf.test(DATA_base$LIP) #Dickey-Fuller = -1.5 NON STATIONNAIRE
adf.test(DATA_base$UNEMP) #Dickey-Fuller = -2.05 NON STATIONNAIRE
adf.test(DATA_base$LCPI) #Dickey-Fuller = -4.01 STATIONNAIRE
adf.test(DATA_base$FFR) #Dickey-Fuller = -2.49 NON STATIONNAIRE
adf.test(DATA_base$DCOILWTICO) #Dickey-Fuller = -2.02 NON STATIONNAIRE

#Pour les variables non stationnaire les mettre en difference 
DLIP <- diff(DATA_base$LIP)
DUNEMP <- diff(DATA_base$UNEMP)
DFFR <- diff(DATA_base$FFR)
DDCOILWTICO <- diff(DATA_base$DCOILWTICO)

adf.test(DLIP) #Dickey-Fuller = -3.44
adf.test(DUNEMP) #Dickey-Fuller = -2.99
adf.test(DFFR) #Dickey-Fuller = -2.91
adf.test(DDCOILWTICO) #Dickey-Fuller = -6.29

# probleme de stationnarité donc deuxième fois diff
D2UNEMP <- diff(DUNEMP)
D2FFR <- diff(DFFR)

adf.test(D2UNEMP) #Dickey-Fuller = -8.75
adf.test(D2FFR) #Dickey-Fuller = -7.04

# Ajuster pour avoir la même longueur
# 1. Variables macro stationnaires
DLIP      <- diff(DATA_base$LIP)[-1]                 # diff → n-1, puis -1 pour aligner à n-2
D2UNEMP   <- diff(diff(DATA_base$UNEMP))             # déjà n-2
D2FFR     <- diff(diff(DATA_base$FFR))               # déjà n-2
DDCOILWTICO <- diff(DATA_base$DCOILWTICO)[-1]        # n-1 → -1 = n-2
LCPI      <- DATA_base$LCPI[-c(1,2)]                 # en niveau mais réduite à n-2

# 2. Indices de chocs à ajuster : on enlève les 2 premières lignes
chocs <- c("RRSHOCK", "rrshock83", "BCSHOCK", "MP1_TC", "FF4_TC", 
           "ED2_TC", "ED3_TC", "ED4_TC", "szshock")
shock_data <- DATA_base[, chocs]
shock_data_aligned <- shock_data[-c(1,2), ]

# 3. Dates alignées (après 2 diff = n - 2)
dates_aligned <- DATA_base$DATES[-c(1,2)]

# 4. Création de la base complète alignée
base_finale <- data.frame(
  date       = dates_aligned,
  DLIP       = DLIP,
  D2UNEMP    = D2UNEMP,
  LCPI       = LCPI,
  D2FFR      = D2FFR,
  Dpetrole   = DDCOILWTICO,
  shock_data_aligned  # toutes les colonnes de chocs ajoutées directement
)
# 5. Vérification
str(base_finale)
head(base_finale)



