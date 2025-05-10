library(vars)
library(dplyr)

# === 1. Liste des chocs ===
chocs <- c("RRSHOCK", "rrshock83", "BCSHOCK", "MP1_TC", "FF4_TC",
           "ED2_TC", "ED3_TC", "ED4_TC", "szshock")

# === 2. Base macro stationnaire
macro_vars <- data.frame(
  DLIP     = base_finale$DLIP,
  D2UNEMP  = base_finale$D2UNEMP,
  LCPI     = base_finale$LCPI,
  D2FFR    = base_finale$D2FFR,
  Dpetrole = base_finale$Dpetrole
)

# === Résumé des critères de lags optimaux pour chaque choc ===
cat("\nRésumé des critères de sélection des lags :\n")
for (shock in chocs) {
  lags <- VARselect(data.frame(base_finale[[shock]], macro_vars), 
                    lag.max = 10, type = "const")$selection
  cat(shock, "→ AIC:", lags["AIC(n)"], 
      " HQ:", lags["HQ(n)"], 
      " SC:", lags["SC(n)"], 
      " FPE:", lags["FPE(n)"], "\n")
}


# Définir manuellement le nombre de lags pour chaque choc
lag_manual <- list(
  RRSHOCK   = 3,
  rrshock83 = 3,
  BCSHOCK   = 2,
  MP1_TC    = 2,
  FF4_TC    = 2,
  ED2_TC    = 2,
  ED3_TC    = 2,
  ED4_TC    = 2,
  szshock   = 1
)

# === Librairies
library(vars)

# === Variables macro à inclure comme réponses
responses <- c("DLIP", "D2UNEMP", "LCPI", "D2FFR", "Dpetrole")

# === Créer le dossier de sortie
dir.create("IRF_individuelles", showWarnings = FALSE)

# === Boucle pour chaque choc
for (shock in chocs) {
  
  cat("\n--- Traitement du choc :", shock, "---\n")
  
  # Construire la base avec le choc et les variables macro
  df_VAR <- data.frame(
    shock = base_finale[[shock]],
    DLIP = base_finale$DLIP,
    D2UNEMP = base_finale$D2UNEMP,
    LCPI = base_finale$LCPI,
    D2FFR = base_finale$D2FFR,
    Dpetrole = base_finale$Dpetrole
  )
  colnames(df_VAR)[1] <- shock  # renommer la colonne du choc
  
  # Récupérer le lag manuel
  p_lag <- lag_manual[[shock]]
  var_model <- VAR(df_VAR, p = p_lag, type = "const")
  
  # Boucle sur chaque réponse individuelle
  for (resp in responses) {
    irf_result <- irf(var_model,
                      impulse = shock,
                      response = resp,
                      n.ahead = 24,
                      boot = TRUE,
                      ci = 0.95)
    
    # Nom de fichier
    filename <- paste0("IRF_individuelles/IRF_", resp, "_vs_", shock, "_p", p_lag, ".png")
    
    # Sauvegarde
    png(filename = filename, width = 1000, height = 600, res = 150)
    plot(irf_result, main = paste("IRF:", resp, "<-", shock, "(lag =", p_lag, ")"))
    dev.off()
  }
}
