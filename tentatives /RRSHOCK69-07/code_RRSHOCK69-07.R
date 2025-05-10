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


RRSHOCK_data <- data[, c("DATES","LIP", "UNEMP", "LCPI", "FFR", "RRSHOCK")]


# Supprimer les lignes avec NA
RRSOCK_DATA69_07 <- na.omit(RRSHOCK_data)

# VISUALISATION DU SHOCK
plot(RRSOCK_DATA69_07$RRSHOCK, type = "l", main = "Romer-Romer Shock (RRSHOCK)", xlab = "Temps", ylab = "RRSHOCK")


#STATIONARITE test ADF

# Test ADF VC = -3.42 à 5%
library(tseries)
adf.test(RRSOCK_DATA69_07$LIP) #Dickey-Fuller = -3.0504
adf.test(RRSOCK_DATA69_07$UNEMP) #Dickey-Fuller = -3.6825 STATIONNAIRE 
adf.test(RRSOCK_DATA69_07$LCPI) #Dickey-Fuller = -1.2859
adf.test(RRSOCK_DATA69_07$FFR) #Dickey-Fuller = -2.2271


#Pour les variables non stationnaire les mettre en difference 
DLIP <- diff(RRSOCK_DATA69_07$LIP)
DLCPI <- diff(RRSOCK_DATA69_07$LCPI)
DFFR <- diff(RRSOCK_DATA69_07$FFR)

adf.test(DLIP) #Dickey-Fuller = -5.94
adf.test(DLCPI) #Dickey-Fuller = -3.72
adf.test(DFFR) #Dickey-Fuller = -7.02

# Ajuster pour avoir la même longueur
UNEMP <- RRSOCK_DATA69_07$UNEMP[-1]
RRSHOCK <- RRSOCK_DATA69_07$RRSHOCK[-1]

# DATA set pour VAR_RRSHOCK69_07
VAR_data_RRSHOCK69_07 <- data.frame(
  RRSHOCK = RRSHOCK,
  DLIP = DLIP,
  UNEMP = UNEMP,
  DLCPI = DLCPI,
  DFFR = DFFR
)

# Charger la librairie
library(vars)

# NOMBRE optimal de lags
# Choix du lag optimal (optionnel)
VARselect(VAR_data_RRSHOCK69_07, lag.max = 10, type = "const")$selection
#AIC:7  HQ:2  SC:1 FPE:7

# Estimation VAR avec RRSHOCK
VAR_RRSHOCK69_07 <- VAR(VAR_data_RRSHOCK69_07, p = 7, type = "const")
summary(VAR_RRSHOCK69_07)


#IRF RRSHOCK
# IRF DLIP
IRF_RRSHOCK69_07_DLIP <- irf(VAR_RRSHOCK69_07, impulse = "RRSHOCK", 
                             response = "DLIP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK69_07_DLIP)

# IRF UNEMP
IRF_RRSHOCK69_07_UNEMP <- irf(VAR_RRSHOCK69_07, impulse = "RRSHOCK", 
                              response = "UNEMP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK69_07_UNEMP)

# IRF DLCPI
IRF_RRSHOCK69_07_DLCPI <- irf(VAR_RRSHOCK69_07, impulse = "RRSHOCK", 
                              response = "DLCPI", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK69_07_DLCPI)

# IRF DFFR
IRF_RRSHOCK69_07_DFFR <- irf(VAR_RRSHOCK69_07, impulse = "RRSHOCK", 
                             response = "DFFR", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK69_07_DFFR)
