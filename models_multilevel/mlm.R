# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "stringr", "purrr", "broom.mixed",
  "modelsummary", "kableExtra", "lme4")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

# Dataset
data = read.csv("dataset/output/data.csv")
data$country_name = as.factor(data$country_name)
data$round = as.factor(data$round)

# Dependent variables
dep_var_list = c("ethnic_id", "ethnic_fairtreat")

# Violence variables (IVs)
violence_var_list_state = c("expo_5yr_10km_bin", "expo_bf5yr_10km_bin")
violence_var_list_rebel = c("expo_reb_5yr_10km_bin", "expo_reb_bf5yr_10km_bin")

# Interactions
int_list_state = c("EOSV_state", "no_int")
int_list_rebel = c("EOSV_rebel", "no_int")

# Combination data frame
c_state = expand.grid(dep_var = dep_var_list,
  violence_var = violence_var_list_state, int = int_list_state,
  stringsAsFactors = FALSE)
c_rebel = expand.grid(dep_var = dep_var_list,
  violence_var = violence_var_list_rebel, int = int_list_rebel,
  stringsAsFactors = FALSE)

# Join all
c = rbind(c_state, c_rebel)

# Get actor, km, time
c$actor = ifelse(grepl("_reb_", c$violence_var), "2rebel", "1state")
c$time = "5yr"
c$time = ifelse(grepl("_bf5yr", c$violence_var), "bf5yr", c$time)
c$dist = gsub(".*_((5|10|25)km)_.*", "\\1", c$violence_var)
c$int2 = ifelse(grepl("EOSV", c$int), "1int", "0noint")

# Re-order (for producing tables)
c = c %>% arrange(dist, time, dep_var, actor)


# ------------------------------

## Formulae

# List
form_list = vector("list", length = nrow(c))

# Controls
ctrl = paste(c("urban", "sex", "econ_employment", "age", "dist_capital_km"),
  collapse = " + ")

# Loop
for(i in 1:nrow(c)){
  f = paste(c$dep_var[i], "~ VIOVAR +", ctrl,
    "+ (1|country_name) + (1|round) + (1|country_name:round)")
  if(!grepl("noint", c$int2[i])){
    f = gsub("VIOVAR", paste(c$violence_var[i], c$int[i], sep = " * "), f)
  } else { f = gsub("VIOVAR", c$violence_var[i], f) }
  form_list[[i]] = f
}

# ------------------------------

## Models (only main ones)

# Last 5 years
lme_5yr = list(
  lmer(as.formula(form_list[[1]]), data = data),
  lmer(as.formula(form_list[[2]]), data = data),
  lmer(as.formula(form_list[[3]]), data = data),
  lmer(as.formula(form_list[[4]]), data = data),
  lmer(as.formula(form_list[[5]]), data = data),
  lmer(as.formula(form_list[[6]]), data = data),
  lmer(as.formula(form_list[[7]]), data = data),
  lmer(as.formula(form_list[[8]]), data = data)
)

# Name models
names(lme_5yr) = paste0("(", 1:8, ")")

# Before previous 5 years
lme_bf5yr = list(
  lmer(as.formula(form_list[[9]]), data = data),
  lmer(as.formula(form_list[[10]]), data = data),
  lmer(as.formula(form_list[[11]]), data = data),
  lmer(as.formula(form_list[[12]]), data = data),
  lmer(as.formula(form_list[[13]]), data = data),
  lmer(as.formula(form_list[[14]]), data = data),
  lmer(as.formula(form_list[[15]]), data = data),
  lmer(as.formula(form_list[[16]]), data = data)
)

# Name models
names(lme_bf5yr) = paste0("(", 1:8, ")")

# ------------------------------



## Model tables (modelsummary)

# specs
coef_recode = c(
  "expo_5yr_10km_bin" = "Violence exposure",
  "expo_reb_5yr_10km_bin" = "Violence exposure",
  "expo_bf5yr_10km_bin" = "Violence exposure",
  "expo_reb_bf5yr_10km_bin" = "Violence exposure",
  "EOSV_state" = "Collective targeting",
  "EOSV_rebel" = "Collective targeting",
  "expo_5yr_10km_bin:EOSV_state" = "Expo $\\times$ targeting",
  "expo_reb_5yr_10km_bin:EOSV_rebel" = "Expo $\\times$ targeting",
  "expo_bf5yr_10km_bin:EOSV_state" = "Expo $\\times$ targeting",
  "expo_reb_bf5yr_10km_bin:EOSV_rebel" = "Expo $\\times$ targeting"
  )


gs = list(
  list("raw" = "nobs", "clean" = "$n$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1),
  list("raw" = "bic", "clean" = "BIC", "fmt" = 1))

n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001. Control variables (not shown): urban, gender, age, employment situation, distance to national capital (in km, log). Both violence exposure and collective targeting refer to the actors defined above (state or rebels). Random intercepts by country, round and country \\times round (not shown)."

# DVs
dvs = recode(unique(c$dep_var),
  "ethnic_id" = "Ethnic ID", "ethnic_fairtreat" = "Ethnic grievances")

# Headers
h1 = c("", 4, 4)
names(h1) = c("", dvs[1], dvs[2])
h2 = c("", rep(2, 4))
names(h2) = c("", rep(c("State violence", "Rebel violence"), 2))

# Check assumption
if(!grepl("rebel", c$actor[8])){warning("Order of state and rebel might be wrong")}
if(!grepl("rebel", c$actor[16])){warning("Order of state and rebel might be wrong")}

# Extrarows
extrarows = as.data.frame(rbind(c("Controls", rep("Yes", length(lme_5yr)))))


## Table 1
modelsummary(lme_5yr,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  output = "latex",
  gof_map = gs,
  title = "Multi-level linear probability models of civilian victimization (during last 5 years) on ethnic outcomes\\label{tab:lme_5yr}",
  add_rows = extrarows,
  threeparttable = TRUE,
  escape = FALSE) %>%
add_header_above(h2) %>%
add_header_above(h1) %>%
kable_styling(latex_options = c("hold_position", "scale_down")) %>%
footnote(general = n, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
save_kable(file = "models_multilevel/output/tab_lme_5yr_10km.tex")

## Table 2
modelsummary(lme_bf5yr,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  output = "latex",
  gof_map = gs,
  title = "Multi-level linear probability models of civilian victimization (before previous 5 years) on ethnic outcomes\\label{tab:lme_bf5yr}",
  add_rows = extrarows,
  threeparttable = TRUE,
  escape = FALSE) %>%
add_header_above(h2) %>%
add_header_above(h1) %>%
kable_styling(latex_options = c("hold_position", "scale_down")) %>%
footnote(general = n, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
save_kable(file = "models_multilevel/output/tab_lme_bf5yr_10km.tex")
