# ÉTAPE 0 : Nettoyage
rm(list = ls())

# ÉTAPE 1 : Packages
install.packages(c("readxl", "dplyr", "lubridate", "ggplot2", "broom"))
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)

# ÉTAPE 2 : Charger les données
data <- read_excel("~/Desktop/FOMC_Bauer_Swanson copie.xlsx")
data$Date <- as.Date(data$Dates)

# ÉTAPE 3 : Créer les différences (Δ)
diff_data <- data %>%
  arrange(Date) %>%
  mutate(
    MPS_ORTH = as.numeric(MPS_ORTH),
    dIP = c(NA, diff(IP)),
    dCPI = c(NA, diff(CPI)),
    dunemp = c(NA, diff(unemp)),
    dT2Y = c(NA, diff(T2Y)),
    
    dIP_lag1 = lag(dIP, 1), dIP_lag2 = lag(dIP, 2), dIP_lag3 = lag(dIP, 3),
    dCPI_lag1 = lag(dCPI, 1), dCPI_lag2 = lag(dCPI, 2), dCPI_lag3 = lag(dCPI, 3),
    dunemp_lag1 = lag(dunemp, 1), dunemp_lag2 = lag(dunemp, 2), dunemp_lag3 = lag(dunemp, 3),
    dT2Y_lag1 = lag(dT2Y, 1), dT2Y_lag2 = lag(dT2Y, 2), dT2Y_lag3 = lag(dT2Y, 3)
  ) %>%
  na.omit()

# ÉTAPE 4 : Paramètres
max_horizon <- 36
variables <- c("dIP", "dCPI", "dunemp", "dT2Y")
results_all <- list()

# ÉTAPE 5 : Boucle de projections locales
for (var in variables) {
  results <- list()
  for (h in 0:max_horizon) {
    df_h <- diff_data %>%
      mutate(Y_lead = lead(!!sym(var), h)) %>%
      filter(!is.na(Y_lead), !is.na(MPS_ORTH))
    
    model <- lm(Y_lead ~ MPS_ORTH +
                  dIP_lag1 + dIP_lag2 + dIP_lag3 +
                  dCPI_lag1 + dCPI_lag2 + dCPI_lag3 +
                  dunemp_lag1 + dunemp_lag2 + dunemp_lag3 +
                  dT2Y_lag1 + dT2Y_lag2 + dT2Y_lag3,
                data = df_h)
    
    if ("MPS_ORTH" %in% rownames(summary(model)$coefficients)) {
      results[[h + 1]] <- tibble(
        horizon = h,
        variable = var,
        coef = coef(model)["MPS_ORTH"],
        se = summary(model)$coefficients["MPS_ORTH", "Std. Error"]
      )
    } else {
      results[[h + 1]] <- tibble(horizon = h, variable = var, coef = NA, se = NA)
    }
  }
  results_all[[var]] <- bind_rows(results)
}

# ÉTAPE 6 : Combine et calcul des intervalles
irf_df <- bind_rows(results_all) %>%
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

# ÉTAPE 7 : Graphiques

# IRFs instantanées
p1 <- ggplot(irf_df, aes(x = horizon, y = coef)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "grey", alpha = 0.2) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "IRFs locales (sur variations, choc : MPS_ORTH)",
       x = "Horizon (mois)", y = "Effet marginal") +
  theme_minimal()

# IRFs cumulées
p2 <- ggplot(irf_df, aes(x = horizon, y = coef_cum)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ci_low_cum, ymax = ci_high_cum), fill = "grey", alpha = 0.2) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "IRFs cumulées (sur variations, choc : MPS_ORTH)",
       x = "Horizon (mois)", y = "Effet cumulé") +
  theme_minimal()

# Affichage
print(p1)
print(p2)
