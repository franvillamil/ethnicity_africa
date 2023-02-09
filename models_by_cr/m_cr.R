# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr", "MASS", "stringr", "purrr", "ggplot2")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

# Functions
source("func/estimate_cr.R")

# Dataset
data = read.csv("dataset/output/data.csv")
data$country_name = as.factor(data$country_name)

# Dependent variables
dep_var_list = c("ethnic_id", "ethnic_fairtreat")

# Check DVs
if(!all(sapply(dep_var_list,
  function(x){length(unique(data[!is.na(data[, x]), x]))}) == 2)){
  stop("Check non-binary dependent variables")
}

# Country and round list
country_list = unique(data$country_name)
round_list = unique(data$round)

# Violence variables (IVs)
# violence_var_list = "expo_5yr_10km_bin"

# Violence variables (IVs)
violence_var_list_state = c("expo_5yr_10km_bin", "expo_bf5yr_10km_bin")
violence_var_list_rebel = c("expo_reb_5yr_10km_bin", "expo_reb_bf5yr_10km_bin")

# Interactions
int_list_state = c("EOSV_state", "no_int")
int_list_rebel = c("EOSV_rebel", "no_int")

# Combination data frame
c_state = expand.grid(
  country = country_list,
  round = 1:6,
  dep_var = dep_var_list,
  violence_var = violence_var_list_state,
  int = int_list_state,
  stringsAsFactors = FALSE)
c_rebel = expand.grid(
  country = country_list,
  round = 1:6,
  dep_var = dep_var_list,
  violence_var = violence_var_list_rebel,
  int = int_list_rebel,
  stringsAsFactors = FALSE)

# Join all
c = rbind(c_state, c_rebel)

# Get actor, km, time
c$actor = "1state"
# c$actor = ifelse(grepl("_all_", c$violence_var), "3any", c$actor)
c$actor = ifelse(grepl("_reb_", c$violence_var), "2rebel", c$actor)
c$time = "anytime"
c$time = ifelse(grepl("_2yr", c$violence_var), "2yr", c$time)
c$time = ifelse(grepl("_5yr", c$violence_var), "5yr", c$time)
c$time = ifelse(grepl("_bf2yr", c$violence_var), "bf2yr", c$time)
c$time = ifelse(grepl("_bf5yr", c$violence_var), "bf5yr", c$time)
c$dist = gsub(".*_((5|10|25)km)_.*", "\\1", c$violence_var)
c$int2 = ifelse(grepl("EOSV", c$int), "1int", "0noint")

# Re-order (for producing tables)
c = c %>% arrange(country, round, dep_var, dist, time, actor, int2)

# Empty data frame
effects_of_violence = data.frame()

# Create empty data frame
model_summary = data.frame(
  coef_violence = rep(NA, nrow(c)),
  coef_se_violence = rep(NA, nrow(c)),
  coef_EOSV = rep(NA, nrow(c)),
  coef_se_EOSV = rep(NA, nrow(c)),
  coef_int = rep(NA, nrow(c)),
  coef_se_int = rep(NA, nrow(c)),
  obsize_mb = rep(NA, nrow(c)),
  dep_var = rep(NA, nrow(c)),
  violence_var = rep(NA, nrow(c)),
  int = rep(NA, nrow(c)),
  n = rep(NA, nrow(c)),
  BIC = rep(NA, nrow(c)),
  AIC = rep(NA, nrow(c)),
  country = rep(NA, nrow(c)),
  rounds = rep(NA, nrow(c)))


# *The* Loop
for(i in 1:nrow(c)){

  prog = paste0("========== (", round(i / nrow(c) * 100, 0), "%) ==========")

  if(i %% 100 == 0){print(paste(i, prog))}

  if(c$int[i] == "no_int"){
    m = estimate_cr(
      country_name = c$country[i],
      round = c$round[i],
      dep_var = c$dep_var[i],
      violence_var = c$violence_var[i])
  } else {
    m = estimate_cr(
      country_name = c$country[i],
      round = c$round[i],
      dep_var = c$dep_var[i],
      violence_var = c$violence_var[i],
      interaction = c$int[i])
  }

  # If coefficient for violence variable is NA, there's no cross variation
  if(!is.null(m)){
    if(is.na(m$coefficient[[c$violence_var[i]]])){m = NULL}
  }

  # Spec info
  model_summary$dep_var[i] = c$dep_var[i]
  model_summary$violence_var[i] = c$violence_var[i]
  model_summary$int[i] = c$int[i]

  # Add model info
  if(!is.null(m)){

    # Model info
    model_summary$obsize_mb[i] = object.size(m)/1e06
    model_summary$n[i] = nobs(m)
    model_summary$country[i] = as.character(unique(m$data[,"country_name"]))
    model_summary$round[i] = unique(m$data[,"round"])
    model_summary$AIC[i] = AIC(m)
    model_summary$BIC[i] = BIC(m)

    # Get coefficient matrix
    coefs = summary(m)$coefficients

    # Extract violence coefficient
    coefs_vio = which(row.names(coefs) == c$violence_var[i])
    model_summary$coef_violence[i] = coefs[coefs_vio, "Estimate"]
    model_summary$coef_se_violence[i] = coefs[coefs_vio, "Std. Error"]

    # Extract EOSV & interaction if applies
    if(c$int[i] != "no_int"){

      coefs_EOSV = which(row.names(coefs) == c$int[i])
      coefs_int = which(grepl(":", row.names(coefs)))

      model_summary$coef_EOSV[i] = coefs[coefs_EOSV, "Estimate"]
      model_summary$coef_se_EOSV[i] = coefs[coefs_EOSV, "Std. Error"]
      if(length(coefs_int) > 0){
        model_summary$coef_int[i] = coefs[coefs_int, "Estimate"]
        model_summary$coef_se_int[i] = coefs[coefs_int, "Std. Error"]
      }

    }

  }

}


# Clean dataframe, and create upr/lwr
model_summary = model_summary %>%
  filter(!is.na(coef_violence)) %>%
  mutate(
    actor = ifelse(grepl("reb", violence_var), "rebel", "state"),
    country2 = paste0(country, " (", round, ")"),
    dep_var_l = ifelse(dep_var == "ethnic_id", "Ethnic ID", "Ethnic grievances"))

write.csv(model_summary, "models_by_cr/output/model_summary.csv", row.names = FALSE)


#### ------------------------------------------------------
#### PLOTS

## CREATE DF - for last 5 yr, and before last 5 yrs

# BASE results for STATE violence

ms_base_state = model_summary %>%
  filter(int == "no_int" & actor == "state") %>%
  filter(!grepl("bf5", violence_var)) %>%
  mutate(
    t_abs = abs(coef_violence/coef_se_violence),
    coef_violence_lwr = coef_violence - qnorm(0.975) * coef_se_violence,
    coef_violence_upr = coef_violence + qnorm(0.975) * coef_se_violence,
    coef_violence_lwr90 = coef_violence - qnorm(0.95) * coef_se_violence,
    coef_violence_upr90 = coef_violence + qnorm(0.95) * coef_se_violence)

ms_base_state_bf5 = model_summary %>%
  filter(int == "no_int" & actor == "state") %>%
  filter(grepl("bf5", violence_var)) %>%
  mutate(
    t_abs = abs(coef_violence/coef_se_violence),
    coef_violence_lwr = coef_violence - qnorm(0.975) * coef_se_violence,
    coef_violence_upr = coef_violence + qnorm(0.975) * coef_se_violence,
    coef_violence_lwr90 = coef_violence - qnorm(0.95) * coef_se_violence,
    coef_violence_upr90 = coef_violence + qnorm(0.95) * coef_se_violence)

# BASE results for REBEL violence

ms_base_reb = model_summary %>%
  filter(int == "no_int" & actor == "rebel") %>%
  filter(!grepl("bf5", violence_var)) %>%
  mutate(
    t_abs = abs(coef_violence/coef_se_violence),
    coef_violence_lwr = coef_violence - qnorm(0.975) * coef_se_violence,
    coef_violence_upr = coef_violence + qnorm(0.975) * coef_se_violence,
    coef_violence_lwr90 = coef_violence - qnorm(0.95) * coef_se_violence,
    coef_violence_upr90 = coef_violence + qnorm(0.95) * coef_se_violence)

ms_base_reb_bf5 = model_summary %>%
  filter(int == "no_int" & actor == "rebel") %>%
  filter(grepl("bf5", violence_var)) %>%
  mutate(
    t_abs = abs(coef_violence/coef_se_violence),
    coef_violence_lwr = coef_violence - qnorm(0.975) * coef_se_violence,
    coef_violence_upr = coef_violence + qnorm(0.975) * coef_se_violence,
    coef_violence_lwr90 = coef_violence - qnorm(0.95) * coef_se_violence,
    coef_violence_upr90 = coef_violence + qnorm(0.95) * coef_se_violence)

# INTERACTION results for STATE violence

ms_int_state = model_summary %>%
  filter(grepl("EOSV", int) & actor == "state") %>%
  filter(!grepl("bf5", violence_var)) %>%
  dplyr::select(country2, dep_var_l,
    coef_violence, coef_se_violence, coef_int, coef_se_int) %>%
  pivot_longer(
    cols = starts_with("coef"),
    names_to = c("stat", "variable"),
    names_pattern = "^(coef|coef_se)_(violence|int)") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(v = recode(variable,
    "violence" = "Violence without EOSV", "int" = "EOSV interaction")) %>%
  mutate(
    t_abs = abs(coef/coef_se),
    lwr = coef - qnorm(0.975) * coef_se,
    upr = coef + qnorm(0.975) * coef_se,
    lwr90 = coef - qnorm(0.95) * coef_se,
    upr90 = coef + qnorm(0.95) * coef_se)

ms_int_state_bf5 = model_summary %>%
  filter(grepl("EOSV", int) & actor == "state") %>%
  filter(grepl("bf5", violence_var)) %>%
  dplyr::select(country2, dep_var_l,
    coef_violence, coef_se_violence, coef_int, coef_se_int) %>%
  pivot_longer(
    cols = starts_with("coef"),
    names_to = c("stat", "variable"),
    names_pattern = "^(coef|coef_se)_(violence|int)") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(v = recode(variable,
    "violence" = "Violence without EOSV", "int" = "EOSV interaction")) %>%
  mutate(
    t_abs = abs(coef/coef_se),
    lwr = coef - qnorm(0.975) * coef_se,
    upr = coef + qnorm(0.975) * coef_se,
    lwr90 = coef - qnorm(0.95) * coef_se,
    upr90 = coef + qnorm(0.95) * coef_se)

# INTERACTION results for REBEL violence

ms_int_reb = model_summary %>%
  filter(grepl("EOSV", int) & actor == "rebel") %>%
  filter(!grepl("bf5", violence_var)) %>%
  dplyr::select(country2, dep_var_l,
    coef_violence, coef_se_violence, coef_int, coef_se_int) %>%
  pivot_longer(
    cols = starts_with("coef"),
    names_to = c("stat", "variable"),
    names_pattern = "^(coef|coef_se)_(violence|int)") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(v = recode(variable,
    "violence" = "Violence without EOSV", "int" = "EOSV interaction")) %>%
  mutate(
    t_abs = abs(coef/coef_se),
    lwr = coef - qnorm(0.975) * coef_se,
    upr = coef + qnorm(0.975) * coef_se,
    lwr90 = coef - qnorm(0.95) * coef_se,
    upr90 = coef + qnorm(0.95) * coef_se)

ms_int_reb_bf5 = model_summary %>%
  filter(grepl("EOSV", int) & actor == "rebel") %>%
  filter(grepl("bf5", violence_var)) %>%
  dplyr::select(country2, dep_var_l,
    coef_violence, coef_se_violence, coef_int, coef_se_int) %>%
  pivot_longer(
    cols = starts_with("coef"),
    names_to = c("stat", "variable"),
    names_pattern = "^(coef|coef_se)_(violence|int)") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(v = recode(variable,
    "violence" = "Violence without EOSV", "int" = "EOSV interaction")) %>%
  mutate(
    t_abs = abs(coef/coef_se),
    lwr = coef - qnorm(0.975) * coef_se,
    upr = coef + qnorm(0.975) * coef_se,
    lwr90 = coef - qnorm(0.95) * coef_se,
    upr90 = coef + qnorm(0.95) * coef_se)

### PLOTS, last 5 years

# Plot for base models, state
p_base_s = ggplot(ms_base_state, aes(x = reorder(country2, desc(country2)),
    y = coef_violence, color = t_abs > qnorm(0.95))) +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = coef_violence_lwr, ymax = coef_violence_upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = coef_violence_lwr90, ymax = coef_violence_upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~dep_var_l) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "", y = "\nCoefficient estimate of exposure to violence\n(5 years previous to the survey)")
ggsave("models_by_cr/output/mcountry_base_state.pdf", p_base_s, width = 5, height = 6)

# Plot for base models, rebel
p_base_r = ggplot(ms_base_reb, aes(x = reorder(country2, desc(country2)),
    y = coef_violence, color = t_abs > qnorm(0.95))) +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = coef_violence_lwr, ymax = coef_violence_upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = coef_violence_lwr90, ymax = coef_violence_upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(~dep_var_l) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "", y = "\nCoefficient estimate of exposure to violence\n(5 years previous to the survey)")
ggsave("models_by_cr/output/mcountry_base_reb.pdf", p_base_r, width = 5, height = 2.5)

# Plot for base models, state
p_int_s = ggplot(ms_int_state %>%
      filter(!country2 %in% c("Senegal (5)", "Burundi (5)")),
    aes(x = reorder(country2, desc(country2)), y = coef, color = t_abs > qnorm(0.95))) +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(v~dep_var_l) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  labs(x = "", y = "\nCoefficient estimate of exposure to violence\n(5 years previous to the survey)") +
  scale_color_manual(values = c("gray", "black"))
ggsave("models_by_cr/output/mcountry_int_state.pdf", p_int_s, width = 5, height = 5)

# Plot for base models, state
p_int_r = ggplot(ms_int_reb,
    aes(x = reorder(country2, desc(country2)), y = coef, color = t_abs > qnorm(0.95))) +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(v~dep_var_l) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  labs(x = "", y = "\nCoefficient estimate of exposure to violence\n(5 years previous to the survey)") +
  scale_color_manual(values = c("gray", "black"))
ggsave("models_by_cr/output/mcountry_int_reb.pdf", p_int_r, width = 5, height = 4.5)

### PLOTS, BEFORE last 5 years

# Plot for base models, state
p_base_s_bf5 = ggplot(ms_base_state_bf5, aes(x = reorder(country2, desc(country2)),
    y = coef_violence, color = t_abs > qnorm(0.95))) +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = coef_violence_lwr, ymax = coef_violence_upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = coef_violence_lwr90, ymax = coef_violence_upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~dep_var_l) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "", y = "\nCoefficient estimate of exposure to violence\n(before 5 last years previous to the survey)")
ggsave("models_by_cr/output/mcountry_base_state_bf5.pdf", p_base_s_bf5, width = 5, height = 9)

# Plot for base models, rebel
p_base_r_bf5 = ggplot(ms_base_reb_bf5, aes(x = reorder(country2, desc(country2)),
    y = coef_violence, color = t_abs > qnorm(0.95))) +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = coef_violence_lwr, ymax = coef_violence_upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = coef_violence_lwr90, ymax = coef_violence_upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(~dep_var_l) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "", y = "\nCoefficient estimate of exposure to violence\n(before 5 last years previous to the survey)")
ggsave("models_by_cr/output/mcountry_base_reb_bf5.pdf", p_base_r_bf5, width = 5, height = 5)

# Plot for base models, state
p_int_s_bf5 = ggplot(ms_int_state_bf5 %>%
      filter(!country2 %in% c("Mali (5)", "Mali (6)", "Niger (5)",
        "Niger (6)", "Nigeria (2)")),
    aes(x = reorder(country2, desc(country2)), y = coef, color = t_abs > qnorm(0.95))) +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(v~dep_var_l) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  labs(x = "", y = "\nCoefficient estimate of exposure to violence\n(before 5 last years previous to the survey)") +
  scale_color_manual(values = c("gray", "black"))
ggsave("models_by_cr/output/mcountry_int_state_bf5.pdf", p_int_s_bf5, width = 5, height = 7)

# Plot for base models, state
p_int_r_bf5 = ggplot(ms_int_reb_bf5 %>%
      filter(!country2 %in% c("Mali (3)", "Mali (4)", "Mali (6)", "Burundi (5)")),
    aes(x = reorder(country2, desc(country2)), y = coef, color = t_abs > qnorm(0.95))) +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(v~dep_var_l) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  labs(x = "", y = "\nCoefficient estimate of exposure to violence\n(before 5 last years previous to the survey)") +
  scale_color_manual(values = c("gray", "black"))
ggsave("models_by_cr/output/mcountry_int_reb_bf5.pdf", p_int_r_bf5, width = 5, height = 4.5)
