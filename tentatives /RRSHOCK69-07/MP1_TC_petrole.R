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

# TRANSFORMER LES DATES 
install.packages("zoo")
library(zoo)
DATES <- data$DATES
# Convert decimal to "yearmon" then to Date (start of the month)
date_vector <- as.Date(as.yearmon(DATES), frac = 0)
# Replace the decimal data
data$DATES <- date_vector


MP1_TC_data <- data[, c("DATES","LIP", "UNEMP", "LCPI", "FFR", "MP1_TC")]

#merge les data
# S'assurer que la date de petrole est au format Date
petrole$DATES <- as.Date(petrole$observation_date)
# Fusion des données sur la date
MP1_TC_petrole <- merge(MP1_TC_data, petrole, by = "DATES", all = FALSE)
head(MP1_TC_petrole)

# les donneées se sont bien merge 
#il y avait un doubons de date 
library(dplyr)
MP1_TC_petrole <- MP1_TC_petrole %>% select(-observation_date)

# Supprimer les lignes avec NA
MP1_TC_petrole_86_12 <- na.omit(MP1_TC_petrole)

# VISUALISATION DU SHOCK
plot(MP1_TC_petrole_86_12 $MP1_TC, type = "l", main = "Gertler-Karadi monthly shocks, current futures (MP1_TC)", xlab = "Temps", ylab = "MP1_TC")

#STATIONARITE test ADF
# Test ADF VC = -3.42 à 5%
library(tseries)
adf.test(MP1_TC_petrole_86_12$LIP) #Dickey-Fuller = -1.72 NON STATIONNAIRE
adf.test(MP1_TC_petrole_86_12$UNEMP) #Dickey-Fuller = -2.34 NON STATIONNAIRE
adf.test(MP1_TC_petrole_86_12$LCPI) #Dickey-Fuller = -3.28 NON STATIONNAIRE
adf.test(MP1_TC_petrole_86_12$FFR) #Dickey-Fuller = -3.63 STATIONNAIRE
adf.test(MP1_TC_petrole_86_12$DCOILWTICO) #Dickey-Fuller = -2.65 NON STATIONNAIRE

#Pour les variables non stationnaire les mettre en difference 
DLIP <- diff(MP1_TC_petrole_86_12$LIP)
DUNEMP <- diff(MP1_TC_petrole_86_12$UNEMP)
DLCPI <- diff(MP1_TC_petrole_86_12$LCPI)
DDCOILWTICO <- diff(MP1_TC_petrole_86_12$DCOILWTICO)

adf.test(DLIP) #Dickey-Fuller = -4.68
adf.test(DUNEMP) #Dickey-Fuller = -3.25
adf.test(DLCPI) #Dickey-Fuller = -6.37
adf.test(DDCOILWTICO) #Dickey-Fuller = -7.56

# TOUT EST STATIONNAIRE APRES DIFFERENCE PARFAIT !
# Ajuster pour avoir la même longueur
MP1_TC <- MP1_TC_petrole_86_12$MP1_TC[-1]
FFR <- MP1_TC_petrole_86_12$FFR[-1]

# DATA set pour VAR_data_MP1_TC_petrole
VAR_data_MP1_TC_petrole <- data.frame(
  MP1_TC = MP1_TC,
  DLIP = DLIP,
  DUNEMP = DUNEMP,
  DLCPI = DLCPI,
  FFR = FFR,
  DDCOILWTICO = DDCOILWTICO
)

# Charger la librairie
library(vars)

# NOMBRE optimal de lags
# Choix du lag optimal (optionnel)
VARselect(VAR_data_MP1_TC_petrole, lag.max = 10, type = "const")$selection
#AIC:3  HQ:2  SC:1 FPE:3

# Estimation VAR avec RRSHOCK
VAR_MP1_TC_petrole <- VAR(VAR_data_MP1_TC_petrole, p = 3, type = "const")
summary(VAR_MP1_TC_petrole)


#IRF MP1_TC
# IRF DLIP
IRF_MP1_TC_petrole_DLIP <- irf(VAR_MP1_TC_petrole, impulse = "MP1_TC", 
                                response = "DLIP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_MP1_TC_petrole_DLIP)


#IRF MP1_TC
# Choix du choc
shock_name <- "MP1_TC"

# Estimation du VAR (supposons déjà fait et stocké dans VAR_MP1_TC)
responses <- c("DLIP", "DUNEMP", "DLCPI", "FFR", "DDCOILWTICO")

# Boucle IRF pour chaque réponse
for (resp in responses) {
  irf_MP1_TC_result <- irf(VAR_MP1_TC_petrole, impulse = shock_name, 
                           response = resp, n.ahead = 24, boot = TRUE, ci = 0.95)
  
  # Affichage
  plot(irf_MP1_TC_result, main = paste("IRF:", resp, "<-", shock_name))
}
