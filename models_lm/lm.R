# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "MASS", "stringr", "purrr", "modelsummary", "kableExtra")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

## Functions

source("func/estimate.R")
source("func/sim.R")
source("func/effect.R")

## modelsummary specs

coef_recode = c(
  "expo" = "Violence exposure",
  "EOSV" = "Collective targeting",
  "expo:EOSV" = "Expo $\\times$ targeting"
  )

gs = list(
  list("raw" = "nobs", "clean" = "$n$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "adj.r.squared", "clean" = "Adj. $R^2$", "fmt" = 2))


n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001. Control variables (not shown): urban, gender, age, employment situation, distance to national capital (in km, log). Country-round fixed effects not shown. Both violence exposure and collective targeting refer to the actors defined above (state or rebels)."

# ------------------------------

# Dataset
data = read.csv("dataset/output/data.csv")
data$country_name = as.factor(data$country_name)
data$round = as.factor(data$round)

# Dependent variables
dep_var_list = c("ethnic_id_ord", "ethnic_fairtreat_ord")

# Violence variables (IVs)
violence_var_list_state = c(
  "expo_5km_bin", "expo_10km_bin", "expo_25km_bin",
  "expo_2yr_5km_bin", "expo_2yr_10km_bin", "expo_2yr_25km_bin",
  "expo_5yr_5km_bin", "expo_5yr_10km_bin", "expo_5yr_25km_bin",
  "expo_bf2yr_5km_bin", "expo_bf2yr_10km_bin", "expo_bf2yr_25km_bin",
  "expo_bf5yr_5km_bin", "expo_bf5yr_10km_bin", "expo_bf5yr_25km_bin")
violence_var_list_rebel = c(
  "expo_reb_5km_bin", "expo_reb_10km_bin", "expo_reb_25km_bin",
  "expo_reb_2yr_5km_bin", "expo_reb_2yr_10km_bin", "expo_reb_2yr_25km_bin",
  "expo_reb_5yr_5km_bin", "expo_reb_5yr_10km_bin", "expo_reb_5yr_25km_bin",
  "expo_reb_bf2yr_5km_bin", "expo_reb_bf2yr_10km_bin", "expo_reb_bf2yr_25km_bin",
  "expo_reb_bf5yr_5km_bin", "expo_reb_bf5yr_10km_bin", "expo_reb_bf5yr_25km_bin")

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
c$time = "anytime"
c$time = ifelse(grepl("_2yr", c$violence_var), "2yr", c$time)
c$time = ifelse(grepl("_5yr", c$violence_var), "5yr", c$time)
c$time = ifelse(grepl("_bf2yr", c$violence_var), "bf2yr", c$time)
c$time = ifelse(grepl("_bf5yr", c$violence_var), "bf5yr", c$time)
c$dist = gsub(".*_((5|10|25)km)_.*", "\\1", c$violence_var)
c$int2 = ifelse(grepl("EOSV", c$int), "1int", "0noint")

# Re-order (for producing tables)
c = c %>% arrange(dist, time, dep_var, actor)

# Empty data frame
effects_of_violence = data.frame()

# Create empty data frame
model_info = data.frame(
  obsize_mb = rep(NA, nrow(c)),
  dep_var = rep(NA, nrow(c)),
  violence_var = rep(NA, nrow(c)),
  int = rep(NA, nrow(c)),
  n = rep(NA, nrow(c)),
  BIC = rep(NA, nrow(c)),
  AIC = rep(NA, nrow(c)),
  country_rounds = rep(NA, nrow(c)),
  country_rounds_list = rep(NA, nrow(c)))

# Models per table
mods_per_tab = 8

# Model list
model_list = vector("list", mods_per_tab)

# Seed (for simulations)
set.seed(1991)


# *The* Loop
for(i in 1:nrow(c)){

  prog = paste0("========== (", round(i / nrow(c) * 100, 0), "%) ==========")

  if(i %% 10 == 0){print(paste(i, prog))}

  if(c$int[i] == "no_int"){
    m = estimate(dep_var = c$dep_var[i],
      violence_var = c$violence_var[i],
      model = "lm")
  } else {
    m = estimate(dep_var = c$dep_var[i],
      violence_var = c$violence_var[i],
      interaction = c$int[i],
      model = "lm")
  }

  # Add model info
  model_info$obsize_mb[i] = object.size(m)/1e06
  model_info$dep_var[i] = c$dep_var[i]
  model_info$violence_var[i] = c$violence_var[i]
  model_info$int[i] = c$int[i]
  model_info$n[i] = nobs(m)
  model_info$country_rounds[i] = length(unique(m$model[,"factor(country_round)"]))
  model_info$AIC[i] = AIC(m)
  model_info$BIC[i] = BIC(m)
  model_info$country_rounds_list[i] = paste(unique(m$model[,"factor(country_round)"]), collapse = ";")

  # Simulations
  m_sim = simulations(m, n_sim = 1000)
  # Effects of violence
  m_eff = violence_effects(
    dep_var = c$dep_var[i],
    violence_var = c$violence_var[i],
    estimates = m_sim,
    confidence = 0.95)

  # Add to data frame
  effects_of_violence = rbind(effects_of_violence, m_eff)

  # Change coefficient names (table maker)
  names(m$coefficients)[grepl("^expo.*bin$", names(m$coefficients))] = "expo"
  names(m$coefficients)[grepl("^EOSV", names(m$coefficients))] = "EOSV"
  names(m$coefficients)[grepl("^expo.*:", names(m$coefficients))] = "expo:EOSV"

  # Save model
  im = i %% mods_per_tab
  if(im == 0){im = mods_per_tab}
  model_list[[im]] = m

  # If batch of 8 models
  if(im == mods_per_tab){

    # Check depvars
    dvs = unique(unlist(sapply(model_list, function(x) as.character(summary(x)$terms[[2]]))))
    dvs = recode(dvs, "ethnic_id_ord" = "Ethnic ID", "ethnic_fairtreat_ord" = "Ethnic grievances")
    # Table title
    tabname = paste(c$time[i], c$dist[i], "ord", sep = "_")
    tabtitle = gsub("bf", "bf ", gsub("_", ", ", tabname))
    if(!grepl("bf", tabtitle)){tabtitle = paste0("prev ", tabtitle)}
    tabtitle = paste0("Civilian victimization and ethnicity (exposure: ", tabtitle, "), linear model on continuous dependent variable (1-5 scale)")
    # Add label
    tabtitle = paste0(tabtitle, "\\label{tab:", tabname, "}")

    # Name models
    names(model_list) = paste0("(", 1:mods_per_tab, ")")
    # Check assumption
    if(!grepl("rebel", c$actor[i])){warning("Order of state and rebel might be wrong")}

    # Headers
    h1 = c("", 4, 4)
    names(h1) = c("", dvs[1], dvs[2])
    h2 = c("", rep(2, 4))
    names(h2) = c("", rep(c("State violence", "Rebel violence"), 2))

    # Notes (Country rounds)
    crs = sapply(model_list, function(x) length(unique(x$model[, "factor(country_round)"])))
    # Extra rows
    extrarows = as.data.frame(rbind(
        c("Controls", rep("Yes", length(model_list))),
        c("Country-round FE", rep("Yes", length(model_list))),
        c("Country-rounds", crs)
      ))

    # Table
    modelsummary(
      models = model_list,
      output = "latex",
      estimate = "{estimate}{stars}",
      coef_map = coef_recode,
      coef_omit = "urban|sex|econ_employment|age|country_round|dist_capital_km",
      gof_map = gs,
      title = tabtitle,
      add_rows = extrarows,
      threeparttable = TRUE,
      escape = FALSE) %>%
    add_header_above(h2) %>%
    add_header_above(h1) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(general = n, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
    save_kable(file = paste0("models_lm/output/tab_", tabname, ".tex"))

    # Clean up
    model_list = vector("list", mods_per_tab)

  }

}

write.csv(model_info, "models_lm/output/model_info.csv", row.names = FALSE)
write.csv(effects_of_violence, "models_lm/output/effects_of_violence.csv", row.names = FALSE)
