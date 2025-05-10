# ÉTAPE 0 : Nettoyer l’environnement
rm(list = ls())

# ÉTAPE 1 : Installer les packages nécessaires
install.packages(c("readxl", "dplyr", "lubridate", "zoo", "ggplot2", "broom"))
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(broom)

# Charger le fichier Excel
data <- read_excel("~/Desktop/FOMC_Bauer_Swanson copie.xlsx")

# ÉTAPE 2 : Ajouter les lags (1 à 3) pour IP, CPI, unemp, T2Y
data <- data %>%
  mutate(
    Date = as.Date(Dates),
    MPS_ORTH = as.numeric(MPS_ORTH),
    
    IP_lag1 = lag(IP, 1), IP_lag2 = lag(IP, 2), IP_lag3 = lag(IP, 3),
    CPI_lag1 = lag(CPI, 1), CPI_lag2 = lag(CPI, 2), CPI_lag3 = lag(CPI, 3),
    unemp_lag1 = lag(unemp, 1), unemp_lag2 = lag(unemp, 2), unemp_lag3 = lag(unemp, 3),
    T2Y_lag1 = lag(T2Y, 1), T2Y_lag2 = lag(T2Y, 2), T2Y_lag3 = lag(T2Y, 3)
  ) %>%
  arrange(Date) %>%
  na.omit()

# ÉTAPE 3 : Initialisation
max_horizon <- 36
results <- list()

# ÉTAPE 4 : Boucle projections locales
for (h in 0:max_horizon) {
  data_h <- data %>%
    mutate(
      IP_lead = lead(IP, h),
      CPI_lead = lead(CPI, h),
      unemp_lead = lead(unemp, h),
      T2Y_lead = lead(T2Y, h)
    ) %>%
    filter(
      !is.na(IP_lead), !is.na(CPI_lead), !is.na(unemp_lead), !is.na(T2Y_lead),
      !is.na(MPS_ORTH)
    )
  
  # Formule commune avec 3 lags de chaque contrôle
  formula_controls <- as.formula(
    paste("~ MPS_ORTH +",
          paste0("IP_lag", 1:3, collapse = " + "), " + ",
          paste0("CPI_lag", 1:3, collapse = " + "), " + ",
          paste0("unemp_lag", 1:3, collapse = " + "), " + ",
          paste0("T2Y_lag", 1:3, collapse = " + "))
  )
  
  model_ip <- lm(update(formula_controls, IP_lead ~ .), data = data_h)
  model_cpi <- lm(update(formula_controls, CPI_lead ~ .), data = data_h)
  model_unemp <- lm(update(formula_controls, unemp_lead ~ .), data = data_h)
  model_t2y <- lm(update(formula_controls, T2Y_lead ~ .), data = data_h)
  
  results[[h + 1]] <- bind_rows(
    if ("MPS_ORTH" %in% rownames(summary(model_ip)$coefficients)) tibble(horizon = h, variable = "IP", coef = coef(model_ip)["MPS_ORTH"], se = summary(model_ip)$coefficients["MPS_ORTH", "Std. Error"]) else tibble(horizon = h, variable = "IP", coef = NA, se = NA),
    if ("MPS_ORTH" %in% rownames(summary(model_cpi)$coefficients)) tibble(horizon = h, variable = "CPI", coef = coef(model_cpi)["MPS_ORTH"], se = summary(model_cpi)$coefficients["MPS_ORTH", "Std. Error"]) else tibble(horizon = h, variable = "CPI", coef = NA, se = NA),
    if ("MPS_ORTH" %in% rownames(summary(model_unemp)$coefficients)) tibble(horizon = h, variable = "unemp", coef = coef(model_unemp)["MPS_ORTH"], se = summary(model_unemp)$coefficients["MPS_ORTH", "Std. Error"]) else tibble(horizon = h, variable = "unemp", coef = NA, se = NA),
    if ("MPS_ORTH" %in% rownames(summary(model_t2y)$coefficients)) tibble(horizon = h, variable = "T2Y", coef = coef(model_t2y)["MPS_ORTH"], se = summary(model_t2y)$coefficients["MPS_ORTH", "Std. Error"]) else tibble(horizon = h, variable = "T2Y", coef = NA, se = NA)
  )
}

# ÉTAPE 5 : Résultats + intervalles
irf_df <- bind_rows(results) %>%
  mutate(
    ci_low = coef - 1.96 * se,
    ci_high = coef + 1.96 * se
  ) %>%
  filter(!is.na(coef)) %>%
  group_by(variable) %>%
  arrange(horizon) %>%
  mutate(
    coef_cum = cumsum(coef),
    ci_low_cum = cumsum(ci_low),
    ci_high_cum = cumsum(ci_high)
  ) %>%
  ungroup()

irf_df_mps <- irf_df 
# IRFs standard
p1 <- ggplot(irf_df, aes(x = horizon, y = coef)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "IRFs locales (choc MPS_ORTH)", y = "Effet marginal", x = "Horizon (mois)") +
  theme_minimal()

# IRFs cumulées
p2 <- ggplot(irf_df, aes(x = horizon, y = coef_cum)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low_cum, ymax = ci_high_cum), alpha = 0.2) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "IRFs cumulées (choc MPS_ORTH)", y = "Effet cumulé", x = "Horizon (mois)") +
  theme_minimal()

# Affichage
print(p1)
print(p2)

