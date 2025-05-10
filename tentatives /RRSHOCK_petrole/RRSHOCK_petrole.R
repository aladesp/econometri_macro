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


RRSHOCK_data <- data[, c("DATES","LIP", "UNEMP", "LCPI", "FFR", "RRSHOCK")]

#merge les data
# S'assurer que la date de petrole est au format Date
petrole$DATES <- as.Date(petrole$observation_date)
# Fusion des données sur la date
RRSHOCK_petrole <- merge(RRSHOCK_data, petrole, by = "DATES", all = FALSE)
head(RRSHOCK_petrole)

# les donneées se sont bien merge 
#il y avait un doubons de date 
library(dplyr)
RRSHOCK_petrole <- RRSHOCK_petrole %>% select(-observation_date)

# Supprimer les lignes avec NA
RRSHOCK_petrole_86_07 <- na.omit(RRSHOCK_petrole)

# VISUALISATION DU SHOCK
plot(RRSHOCK_petrole_86_07$RRSHOCK, type = "l", main = "Romer-Romer Shock (RRSHOCK)", xlab = "Temps", ylab = "RRSHOCK")

#STATIONARITE test ADF

# Test ADF VC = -3.42 à 5%
library(tseries)
adf.test(RRSHOCK_petrole_86_07$LIP) #Dickey-Fuller = -1.70 NON STATIONNAIRE
adf.test(RRSHOCK_petrole_86_07$UNEMP) #Dickey-Fuller = -2.51 NON STATIONNAIRE
adf.test(RRSHOCK_petrole_86_07$LCPI) #Dickey-Fuller = -2.76 NON STATIONNAIRE
adf.test(RRSHOCK_petrole_86_07$FFR) #Dickey-Fuller = -3.21 NON STATIONNAIRE
adf.test(RRSHOCK_petrole_86_07$DCOILWTICO) #Dickey-Fuller = 0.59 NON STATIONNAIRE

#Pour les variables non stationnaire les mettre en difference 
DLIP <- diff(RRSHOCK_petrole_86_07$LIP)
DUNEMP <- diff(RRSHOCK_petrole_86_07$UNEMP)
DLCPI <- diff(RRSHOCK_petrole_86_07$LCPI)
DFFR <- diff(RRSHOCK_petrole_86_07$FFR)
DDCOILWTICO <- diff(RRSHOCK_petrole_86_07$DCOILWTICO)

adf.test(DLIP) #Dickey-Fuller = -4.96
adf.test(DUNEMP) #Dickey-Fuller = -3.55
adf.test(DLCPI) #Dickey-Fuller = -5.44
adf.test(DFFR) #Dickey-Fuller = -4.09
adf.test(DDCOILWTICO) #Dickey-Fuller = -5.99

# TOUT EST STATIONNAIRE APRES DIFFERENCE PARFAIT !
# Ajuster pour avoir la même longueur
RRSHOCK <- RRSHOCK_petrole_86_07$RRSHOCK[-1]

# DATA set pour VAR_data_RRSHOCK_PETROLE
VAR_data_RRSHOCK_PETROLE <- data.frame(
  RRSHOCK = RRSHOCK,
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
VARselect(VAR_data_RRSHOCK_PETROLE, lag.max = 10, type = "const")$selection
#AIC:1  HQ:1  SC:1 FPE:1

# Estimation VAR avec RRSHOCK
VAR_RRSHOCK_petrole <- VAR(VAR_data_RRSHOCK_PETROLE, p = 1, type = "const")
summary(VAR_RRSHOCK_petrole)


#IRF RRSHOCK
# IRF DLIP
IRF_RRSHOCK_petrole_DLIP <- irf(VAR_RRSHOCK_petrole, impulse = "RRSHOCK", 
                             response = "DLIP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK_petrole_DLIP)

# IRF DUNEMP
IRF_RRSHOCK_petrole_DUNEMP <- irf(VAR_RRSHOCK_petrole, impulse = "RRSHOCK", 
                                response = "DUNEMP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK_petrole_DUNEMP)

# IRF DLCPI
IRF_RRSHOCK_petrole_DLCPI <- irf(VAR_RRSHOCK_petrole, impulse = "RRSHOCK", 
                                  response = "DLCPI", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK_petrole_DLCPI)

# IRF DFFR
IRF_RRSHOCK_petrole_DFFR <- irf(VAR_RRSHOCK_petrole, impulse = "RRSHOCK", 
                                 response = "DFFR", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK_petrole_DFFR)

# IRF DDCOILWTICO
IRF_RRSHOCK_petrole_DDCOILWTICO <- irf(VAR_RRSHOCK_petrole, impulse = "RRSHOCK", 
                                response = "DDCOILWTICO", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(IRF_RRSHOCK_petrole_DDCOILWTICO)



