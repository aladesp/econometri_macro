#1. VAR avec RRSHOCK comme variable exogène
rm(list=ls())
# Charger les packages
library(vars)
library(readxl)
library(zoo)
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
RRSHOCK_69_07 <- na.omit(RRSHOCK_data)

#STATIONARITE test ADF

# Test ADF VC = -3.42 à 5%
library(tseries)
adf.test(RRSHOCK_69_07$LIP) #Dickey-Fuller = -3.05 NON STATIONNAIRE
adf.test(RRSHOCK_69_07$UNEMP) #Dickey-Fuller = -3.68 STATIONNAIRE
adf.test(RRSHOCK_69_07$LCPI) #Dickey-Fuller = -1.28 NON STATIONNAIRE
adf.test(RRSHOCK_69_07$FFR) #Dickey-Fuller = -2.22 NON STATIONNAIRE


# Créer les variables stationnaires
DLIP <- diff(RRSHOCK_69_07$LIP)
DLCPI <- diff(RRSHOCK_69_07$LCPI)
DFFR <- diff(RRSHOCK_69_07$FFR)

UNEMP <- RRSHOCK_69_07$UNEMP[-1]
RRSHOCK <- RRSHOCK_69_07$RRSHOCK[-1]

# Créer le dataset
VAR_data <- data.frame(DLIP = DLIP, UNEMP = UNEMP, DLCPI = DLCPI, DFFR = DFFR)
RRSHOCK_exo <- as.matrix(RRSHOCK)  # doit être une matrice

# Choix optimal du nombre de lags
lag_selection <- VARselect(VAR_data, lag.max = 10, type = "const")
print(lag_selection$selection)

#AIC:7  HQ:2  SC:2 FPE:7

# Estimation du VAR avec RRSHOCK en exogène
VAR_exo <- VAR(VAR_data, p = 2, type = "const", exogen = RRSHOCK_exo)

# Résumé
summary(VAR_exo)

# IRF (exemples)
irf_dlip <- irf(VAR_exo, impulse = "DFFR", response = "DLIP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(irf_dlip)

irf_unemp <- irf(VAR_exo, impulse = "DFFR", response = "UNEMP", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(irf_unemp)

irf_dlcpi <- irf(VAR_exo, impulse = "DFFR", response = "DLCPI", n.ahead = 24, boot = TRUE, ci = 0.95)
plot(irf_dlcpi)


#SVAR

# Créer à nouveau la base
VAR_data_reduit <- data.frame(DLIP = DLIP, UNEMP = UNEMP, DLCPI = DLCPI, DFFR = DFFR)

# Reprise du VAR réduit
VAR_reduit <- VAR(VAR_data_reduit, p = 2, type = "const")

library(vars)

# Nombre de variables
k <- ncol(VAR_data_reduit)

# Créer la matrice Amat pour identification de Cholesky
Amat <- diag(k)
Amat[lower.tri(Amat)] <- NA  # impose les zéros au-dessus de la diagonale

# Estimation du SVAR avec cette Amat
SVAR_model <- SVAR(VAR_reduit, Amat = Amat)

# IRF : effet d'un choc de politique monétaire (DFFR)
irf_dlip_svar <- irf(SVAR_model, impulse = "DFFR", response = "DLIP", n.ahead = 24, boot = TRUE)
plot(irf_dlip_svar)

irf_unemp_svar <- irf(SVAR_model, impulse = "DFFR", response = "UNEMP", n.ahead = 24, boot = TRUE)
plot(irf_unemp_svar)

irf_dlcpi_svar <- irf(SVAR_model, impulse = "DFFR", response = "DLCPI", n.ahead = 24, boot = TRUE)
plot(irf_dlcpi_svar)

