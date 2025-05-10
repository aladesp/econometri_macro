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


rrshock83_data <- data[, c("DATES","LIP", "UNEMP", "LCPI", "FFR", "rrshock83")]


# Supprimer les lignes avec NA
rrshock83_DATA83_07 <- na.omit(rrshock83_data)

# VISUALISATION DU SHOCK
plot(rrshock83_DATA83_07$rrshock83, type = "l", main = "Romer-Romer Shock (rrshock83)", xlab = "Temps", ylab = "rrshock83")


#STATIONARITE test ADF

# Test ADF VC = -3.42 à 5%
library(tseries)
adf.test(rrshock83_DATA83_07$LIP) #Dickey-Fuller = -2.12 
adf.test(rrshock83_DATA83_07$UNEMP) #Dickey-Fuller = -3.04
adf.test(rrshock83_DATA83_07$LCPI) #Dickey-Fuller = -1.59
adf.test(rrshock83_DATA83_07$FFR) #Dickey-Fuller = -3.02


#Pour les variables non stationnaire les mettre en difference 
DLIP <- diff(rrshock83_DATA83_07$LIP)
DUNEMP <- diff(rrshock83_DATA83_07$UNEMP)
DLCPI <- diff(rrshock83_DATA83_07$LCPI)
DFFR <- diff(rrshock83_DATA83_07$FFR)

adf.test(DLIP) #Dickey-Fuller = -5.28
adf.test(DUNEMP) #Dickey-Fuller = -4.07
adf.test(DLCPI) #Dickey-Fuller = -5.35
adf.test(DFFR) #Dickey-Fuller = -5.00

# Ajuster pour avoir la même longueur
rrshock83 <- rrshock83_DATA83_07$rrshock83[-1]

# DATA set pour VAR_rrshock83_07
VAR_data_rrshock83_07 <- data.frame(
  rrshock83 = rrshock83,
  DLIP = DLIP,
  DUNEMP = DUNEMP,
  DLCPI = DLCPI,
  DFFR = DFFR
)

# Charger la librairie
library(vars)

# NOMBRE optimal de lags
# Choix du lag optimal (optionnel)
VARselect(VAR_data_rrshock83_07, lag.max = 10, type = "const")$selection
#AIC:1  HQ:1  SC:1 FPE:1

# Estimation VAR avec RRSHOCK
VAR_rrshock83_07 <- VAR(VAR_data_rrshock83_07, p = 1, type = "const")
summary(VAR_rrshock83_07)


#IRF rrshock83
# IRF DLIP
IRF_rrshock83_07_DLIP <- irf(VAR_rrshock83_07, impulse = "rrshock83", 
                             response = "DLIP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_07_DLIP)

# IRF DUNEMP
IRF_rrshock83_07_DUNEMP <- irf(VAR_rrshock83_07, impulse = "rrshock83", 
                             response = "DUNEMP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_07_DUNEMP)

# IRF DLCPI
IRF_rrshock83_07_DLCPI <- irf(VAR_rrshock83_07, impulse = "rrshock83", 
                               response = "DLCPI", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_07_DLCPI)

# IRF DFFR
IRF_rrshock83_07_DFFR <- irf(VAR_rrshock83_07, impulse = "rrshock83", 
                              response = "DFFR", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_07_DFFR)

