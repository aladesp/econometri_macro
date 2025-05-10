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
"rrshock83", "BCSHOCK", "MP1_TC", "FF4_TC", "ED2_TC", "ED3_TC", "ED4_TC",
"szshock")]"


