# Nettoyage
rm(list = ls())

# Packages
install.packages(c("readxl", "dplyr", "lubridate", "ggplot2", "broom"))
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)

# Chargement des données
data <- read_excel("~/Desktop/Macroeconometrics/Data3.xlsx")
data$DATES <- as.Date(data$DATES)

# Création des variables différenciées
diff_data <- data.frame(
  Dates = data$DATES[-1],
  RRSHOCK = data$RRSHOCK[-1],
  UNEMP = data$UNEMP[-1],
  dLIP = diff(data$LIP),
  dLCPI = diff(data$LCPI),
  dFFR = diff(data$FFR),
  dLPCOM = diff(data$LPCOM),
  dLRCDUR = diff(data$LRCDUR),
  dLRCND = diff(data$LRCND),
  dLRCSV = diff(data$LRCSV)
)

# Ajout des lags comme contrôles dynamiques
diff_data <- diff_data %>%
  mutate(
    dLIP_lag1 = lag(dLIP, 1), dLIP_lag2 = lag(dLIP, 2), dLIP_lag3 = lag(dLIP, 3),
    dLIP_lag4 = lag(dLIP, 4), dLIP_lag5 = lag(dLIP, 5), dLIP_lag6 = lag(dLIP, 6),
    dLCPI_lag1 = lag(dLCPI, 1), dLCPI_lag2 = lag(dLCPI, 2), dLCPI_lag3 = lag(dLCPI, 3),
    dLCPI_lag4 = lag(dLCPI, 4), dLCPI_lag5 = lag(dLCPI, 5), dLCPI_lag6 = lag(dLCPI, 6),
    dFFR_lag1 = lag(dFFR, 1), dFFR_lag2 = lag(dFFR, 2), dFFR_lag3 = lag(dFFR, 3),
    dFFR_lag4 = lag(dFFR, 4), dFFR_lag5 = lag(dFFR, 5), dFFR_lag6 = lag(dFFR, 6)
  ) %>%
  na.omit()

# Paramètres
max_horizon <- 36
variables <- c("UNEMP", "dLIP", "dLCPI", "dFFR")
results_all <- list()

# Boucle sur chaque variable macro
for (var in variables) {
  results <- list()
  for (h in 0:max_horizon) {
    df_h <- diff_data %>%
      mutate(Y_lead = lead(!!sym(var), h)) %>%
      filter(!is.na(Y_lead), !is.na(RRSHOCK))
    
    model <- lm(Y_lead ~ RRSHOCK +
                  dLIP_lag1 + dLIP_lag2 + dLIP_lag3 + dLIP_lag4 + dLIP_lag5 + dLIP_lag6 +
                  dLCPI_lag1 + dLCPI_lag2 + dLCPI_lag3 + dLCPI_lag4 + dLCPI_lag5 + dLCPI_lag6 +
                  dFFR_lag1 + dFFR_lag2 + dFFR_lag3 + dFFR_lag4 + dFFR_lag5 + dFFR_lag6, data = df_h)
    
    if ("RRSHOCK" %in% rownames(summary(model)$coefficients)) {
      results[[h + 1]] <- tibble(
        horizon = h,
        variable = var,
        coef = coef(model)["RRSHOCK"],
        se = summary(model)$coefficients["RRSHOCK", "Std. Error"]
      )
    } else {
      results[[h + 1]] <- tibble(horizon = h, variable = var, coef = NA, se = NA)
    }
  }
  results_all[[var]] <- bind_rows(results)
}

# Combinaison et calcul des bandes
irf_df <- bind_rows(results_all) %>%
  mutate(
    ci_low = coef - 1.96 * se,
    ci_high = coef + 1.96 * se
  ) %>%
  group_by(variable) %>%
  arrange(horizon) %>%
  mutate(
    coef_cum = cumsum(coef),
    ci_low_cum = cumsum(ci_low),
    ci_high_cum = cumsum(ci_high)
  ) %>%
  ungroup() %>%
  filter(!is.na(coef))

# Affichage des IRFs standards
p1 <- ggplot(irf_df, aes(x = horizon, y = coef)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "IRFs via Projections Locales (choc : RRSHOCK)",
       y = "Réponse instantanée", x = "Horizon (mois)") +
  theme_minimal()

# Affichage des IRFs cumulées
p2 <- ggplot(irf_df, aes(x = horizon, y = coef_cum)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low_cum, ymax = ci_high_cum), alpha = 0.2) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "IRFs cumulées (choc : RRSHOCK)",
       y = "Réponse cumulée", x = "Horizon (mois)") +
  theme_minimal()

# Afficher
print(p1)
print(p2)



