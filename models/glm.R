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
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))

n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001. Control variables (not shown): urban, gender, age, employment situation, distance to national capital (in km, log). Country-round fixed effects not shown. Both violence exposure and collective targeting refer to the actors defined above (state or rebels)."

# ------------------------------


# Dataset
data = read.csv("dataset/output/data.csv")
data$country_name = as.factor(data$country_name)
data$round = as.factor(data$round)

# Dependent variables
dep_var_list = c("ethnic_id", "ethnic_fairtreat")

# Check DVs
if(!all(sapply(dep_var_list,
  function(x){length(unique(data[!is.na(data[, x]), x]))}) == 2)){
  stop("Check non-binary dependent variables")
}

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
c$actor = "1state"
c$actor = ifelse(grepl("_reb_", c$violence_var), "2rebel", c$actor)
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

# Model list
model_list = vector("list", 8)

# Seed (for simulations)
set.seed(1991)

# *The* Loop
for(i in 1:nrow(c)){

  # Status
  prog = paste0("========== (", round(i / nrow(c) * 100, 0), "%) ==========")
  if(i %% 10 == 0){print(paste(i, prog))}

  if(c$int[i] == "no_int"){
    m = estimate(dep_var = c$dep_var[i],
      violence_var = c$violence_var[i])
  } else {
    m = estimate(dep_var = c$dep_var[i],
      violence_var = c$violence_var[i],
      interaction = c$int[i])
  }

  # Add model info
  model_info$obsize_mb[i] = object.size(m)/1e06
  model_info$dep_var[i] = c$dep_var[i]
  model_info$violence_var[i] = c$violence_var[i]
  model_info$int[i] = c$int[i]
  model_info$n[i] = nobs(m)
  model_info$country_rounds[i] = length(unique(m$model[, grepl("country_round", names(m$model))]))
  model_info$AIC[i] = AIC(m)
  model_info$BIC[i] = BIC(m)
  model_info$country_rounds_list[i] = paste(
    unique(m$model[, grepl("country_round", names(m$model))]), collapse = ";")

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
  im = i %% 8
  if(im == 0){im = 8}
  model_list[[im]] = m

  # If batch of 8 models
  if(im == 8){

    # Check depvars
    dvs = unique(unlist(sapply(model_list, function(x) as.character(summary(x)$terms[[2]]))))
    dvs = recode(dvs, "ethnic_id" = "Ethnic ID", "ethnic_fairtreat" = "Ethnic grievances")
    # Table title
    tabname = paste(c$time[i], c$dist[i], sep = "_")
    tabtitle = gsub("bf", "bf ", gsub("_", ", ", tabname))
    if(!grepl("bf", tabtitle)){tabtitle = paste0("prev ", tabtitle)}
    tabtitle = paste0("Civilian victimization and ethnicity (exposure: ", tabtitle, ")")
    # Tables for main text
    if(tabname == "5yr_10km"){
      tabtitle = "Civilian victimization and ethnicity (exposure last 5 years, within 10km)"}
    if(tabname == "bf5yr_10km"){
      tabtitle = "Civilian victimization and ethnicity (exposure before last 5 years, within 10km)"}
    # Add label
    tabtitle = paste0(tabtitle, "\\label{tab:", tabname, "}")

    # Name models
    names(model_list) = paste0("(", 1:8, ")")
    # Check assumption
    if(!grepl("rebel", c$actor[i])){warning("Order of state and rebel might be wrong")}

    # Headers
    h1 = c("", 4, 4)
    names(h1) = c("", dvs[1], dvs[2])
    h2 = c("", rep(2, 4))
    names(h2) = c("", rep(c("State violence", "Rebel violence"), 2))

    # Notes (Country rounds)
    crs = sapply(model_list, function(x) length(unique(x$data[,"country_round"])))
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
    save_kable(file = paste0("models/output/tab_", tabname, ".tex"))

    # Clean up
    model_list = vector("list", 8)

  }

}

write.csv(model_info, "models/output/model_info.csv", row.names = FALSE)
write.csv(effects_of_violence, "models/output/effects_of_violence.csv", row.names = FALSE)
