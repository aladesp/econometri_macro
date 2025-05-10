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

VAR_data_RRSHOCK_PETROLE <- data.frame(
  RRSHOCK = RRSHOCK,
  DLIP = DLIP,
  DUNEMP = DUNEMP,
  DLCPI = DLCPI,
  DFFR = DFFR,
  DDCOILWTICO = DDCOILWTICO
)

VAR_data <- VAR_data_RRSHOCK_PETROLE[, c("DLIP", "DUNEMP", "DLCPI", "DFFR", "DDCOILWTICO")]
RRSHOCK_exo <- as.matrix(VAR_data_RRSHOCK_PETROLE$RRSHOCK)

library(vars)
# Choix optimal du nombre de lags
lag_selection <- VARselect(VAR_data, lag.max = 10, type = "const")
print(lag_selection$selection)

#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 
VAR_petrole_exo <- VAR(VAR_data, p = 1, type = "const", exogen = RRSHOCK_exo)

# IRF de DLIP à un choc sur DFFR
irf_dlip <- irf(VAR_petrole_exo, impulse = "DFFR", response = "DLIP", n.ahead = 24, boot = TRUE)
plot(irf_dlip)

# IRF de DUNEMP à un choc sur DFFR
irf_dunemp <- irf(VAR_petrole_exo, impulse = "DFFR", response = "DUNEMP", n.ahead = 24, boot = TRUE)
plot(irf_dunemp)

# IRF de DLCPI à un choc sur DFFR
irf_dlcpi <- irf(VAR_petrole_exo, impulse = "DFFR", response = "DLCPI", n.ahead = 24, boot = TRUE)
plot(irf_dlcpi)

# IRF de DDCOILWTICO (pétrole) à un choc sur DFFR
irf_petrole <- irf(VAR_petrole_exo, impulse = "DFFR", response = "DDCOILWTICO", n.ahead = 24, boot = TRUE)
plot(irf_petrole)

