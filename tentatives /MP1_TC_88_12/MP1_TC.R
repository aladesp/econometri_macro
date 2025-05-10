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


# TRANSFORMER LES DATES 
install.packages("zoo")
library(zoo)
DATES <- data$DATES
# Convert decimal to "yearmon" then to Date (start of the month)
date_vector <- as.Date(as.yearmon(DATES), frac = 0)
# Replace the decimal data
data$DATES <- date_vector

# Garder uniquement les colonnes importantes POUR GK
MP1_data <- data[, c("DATES","LIP", "UNEMP", "LCPI", "FFR", "MP1_TC")]

# Supprimer les lignes avec NA
GK_MP1_TC_88_12 <- na.omit(MP1_data)

# VISUALISATION DU SHOCK
plot(GK_MP1_TC_88_12$MP1_TC, type = "l", main = "Gertler-Karadi monthly shocks (MP1_TC)", xlab = "Temps", ylab = "MP1_TC")


#STATIONARITE test ADF

# Test ADF VC = -3.42 à 5%
library(tseries)
#STATIONARITE test ADF

adf.test(GK_MP1_TC_88_12$LIP) #Dickey-Fuller = -1.72
adf.test(GK_MP1_TC_88_12$UNEMP) #Dickey-Fuller = -2.34
adf.test(GK_MP1_TC_88_12$LCPI) #Dickey-Fuller = -3.28
adf.test(GK_MP1_TC_88_12$FFR) #Dickey-Fuller = -3.63 STATIONNAIRE 

#Pour les variables non stationnaire les mettre en difference 
DLIP <- diff(GK_MP1_TC_88_12$LIP)
DUNEMP <- diff(GK_MP1_TC_88_12$UNEMP)
DLCPI <- diff(GK_MP1_TC_88_12$LCPI)

# Ajuster pour avoir la même longueur
MP1_TC <- GK_MP1_TC_88_12$MP1_TC[-1]
FFR <- GK_MP1_TC_88_12$FFR[-1]

# DATA set pour VAR_MP1_TC88_12
VAR_data_MP1_TC88_12 <- data.frame(
  MP1_TC = MP1_TC,
  DLIP = DLIP,
  DUNEMP = DUNEMP,
  DLCPI = DLCPI,
  FFR = FFR
)

# Charger la librairie
library(vars)

# NOMBRE optimal de lags
# Choix du lag optimal (optionnel)
VARselect(VAR_data_MP1_TC88_12, lag.max = 10, type = "const")$selection
#AIC:3  HQ:2  SC:2 FPE:3

# Estimation VAR avec RRSHOCK
VAR_MP1_TC88_12 <- VAR(VAR_data_MP1_TC88_12, p = 3, type = "const")
summary(VAR_MP1_TC88_12)


#IRF MP1_TC
# IRF DLIP
IRF_MP1_TC_DLIP <- irf(VAR_MP1_TC88_12, impulse = "MP1_TC", 
                             response = "DLIP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_MP1_TC_DLIP)

# IRF DUNEMP
IRF_MP1_TC_DUNEMP <- irf(VAR_MP1_TC88_12, impulse = "MP1_TC", 
                       response = "DUNEMP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_MP1_TC_DUNEMP)

# IRF DLCPI
IRF_MP1_TC_DLCPI <- irf(VAR_MP1_TC88_12, impulse = "MP1_TC", 
                       response = "DLCPI", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_MP1_TC_DLCPI)

# IRF FFR
IRF_MP1_TC_FFR <- irf(VAR_MP1_TC88_12, impulse = "MP1_TC", 
                       response = "FFR", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_MP1_TC_FFR)