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


rrshock83_data <- data[, c("DATES","LIP", "UNEMP", "LCPI", "FFR", "rrshock83")]

#merge les data
# S'assurer que la date de petrole est au format Date
petrole$DATES <- as.Date(petrole$observation_date)
# Fusion des données sur la date
rrshock83_petrole <- merge(rrshock83_data, petrole, by = "DATES", all = FALSE)
head(rrshock83_petrole)

# les donneées se sont bien merge 
#il y avait un doubons de date 
library(dplyr)
rrshock83_petrole <- rrshock83_petrole %>% select(-observation_date)

# Supprimer les lignes avec NA
rrshock83_petrole_86_07 <- na.omit(rrshock83_petrole)

# VISUALISATION DU SHOCK
plot(rrshock83_petrole_86_07$rrshock83, type = "l", main = "Romer-Romer Shock (rrshock83)", xlab = "Temps", ylab = "rrshock83")

#STATIONARITE test ADF

# Test ADF VC = -3.42 à 5%
install.packages(tseries)
library(tseries)
adf.test(rrshock83_petrole_86_07$LIP) #Dickey-Fuller = -1.70 NON STATIONNAIRE
adf.test(rrshock83_petrole_86_07$UNEMP) #Dickey-Fuller = -2.51 NON STATIONNAIRE
adf.test(rrshock83_petrole_86_07$LCPI) #Dickey-Fuller = -2.76 NON STATIONNAIRE
adf.test(rrshock83_petrole_86_07$FFR) #Dickey-Fuller = -3.21 NON STATIONNAIRE
adf.test(rrshock83_petrole_86_07$DCOILWTICO) #Dickey-Fuller = 0.59 NON STATIONNAIRE

#Pour les variables non stationnaire les mettre en difference 
DLIP <- diff(rrshock83_petrole_86_07$LIP)
DUNEMP <- diff(rrshock83_petrole_86_07$UNEMP)
DLCPI <- diff(rrshock83_petrole_86_07$LCPI)
DFFR <- diff(rrshock83_petrole_86_07$FFR)
DDCOILWTICO <- diff(rrshock83_petrole_86_07$DCOILWTICO)

# Ajuster pour avoir la même longueur
rrshock83 <- rrshock83_petrole_86_07$rrshock83[-1]

# DATA set pour VAR_data_RRSHOCK_PETROLE
VAR_data_rrshock83_PETROLE <- data.frame(
  rrshock83 = rrshock83,
  DLIP = DLIP,
  DUNEMP = DUNEMP,
  DLCPI = DLCPI,
  DFFR = DFFR,
  DDCOILWTICO = DDCOILWTICO
)

# Charger la librairie
library(vars)

# NOMBRE optimal de lags
# Choix du lag optimal (optionnel)
VARselect(VAR_data_rrshock83_PETROLE, lag.max = 10, type = "const")$selection
#AIC:1  HQ:1  SC:1 FPE:1

# Estimation VAR avec RRSHOCK
VAR_rrshock83_petrole <- VAR(VAR_data_rrshock83_PETROLE, p = 1, type = "const")
summary(VAR_rrshock83_petrole)


#IRF rrshock83
# IRF DLIP
IRF_rrshock83_petrole_DLIP <- irf(VAR_rrshock83_petrole, impulse = "rrshock83", 
                                response = "DLIP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_petrole_DLIP)

# IRF DUNEMP
IRF_rrshock83_petrole_DUNEMP <- irf(VAR_rrshock83_petrole, impulse = "rrshock83", 
                                  response = "DUNEMP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_petrole_DUNEMP)

# IRF DLCPI
IRF_rrshock83_petrole_DLCPI <- irf(VAR_rrshock83_petrole, impulse = "rrshock83", 
                                    response = "DLCPI", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_petrole_DLCPI)

# IRF DFFR
IRF_rrshock83_petrole_DFFR <- irf(VAR_rrshock83_petrole, impulse = "rrshock83", 
                                   response = "DFFR", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_petrole_DFFR)

# IRF DDCOILWTICO
IRF_rrshock83_petrole_DDCOILWTICO <- irf(VAR_rrshock83_petrole, impulse = "rrshock83", 
                                       response = "DDCOILWTICO", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_rrshock83_petrole_DDCOILWTICO)