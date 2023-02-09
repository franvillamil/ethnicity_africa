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

n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001. Control variables (not shown): urban, gender, age, and employment situation. Region-round fixed effects not shown. Both violence exposure and collective targeting refer to the actors defined above (state or rebels). Individual exposure refers to violence during early adulthood (when respondent was between 15 and 25 years old)."

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
  "expo_1525_25km_bin", "expo_1525_10km_bin", "expo_1525_5km_bin")
violence_var_list_rebel = c(
  "expo_reb_1525_25km_bin", "expo_reb_1525_10km_bin", "expo_reb_1525_5km_bin")

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
c$actor = ifelse(grepl("_all_", c$violence_var), "3any", c$actor)
c$actor = ifelse(grepl("_reb_", c$violence_var), "2rebel", c$actor)
c$dist = gsub(".*_((5|10|25)km)_.*", "\\1", c$violence_var)
c$int2 = ifelse(grepl("EOSV", c$int), "1int", "0noint")

# Re-order (for producing tables)
c = c %>% arrange(dist, dep_var, actor)

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
  region_rounds = rep(NA, nrow(c)))

# Models per table
mods_per_tab = 8

# Model list
model_list = vector("list", mods_per_tab)

# Seed (for simulations)
set.seed(2605)

# *The* Loop
for(i in 1:nrow(c)){

  # Progress status
  print(paste0(i, "/", nrow(c), " (", format(Sys.time(), "%H:%M %p"), ")"))

  if(c$int[i] == "no_int"){
    m = estimate(dep_var = c$dep_var[i],
      violence_var = c$violence_var[i],
      control_vars = c("urban", "sex", "econ_employment", "age"),
      fe = "region_round")
  } else {
    m = estimate(dep_var = c$dep_var[i],
      violence_var = c$violence_var[i],
      interaction = c$int[i],
      control_vars = c("urban", "sex", "econ_employment", "age"),
      fe = "region_round")
  }

  # Add model info
  model_info$obsize_mb[i] = object.size(m)/1e06
  model_info$dep_var[i] = c$dep_var[i]
  model_info$violence_var[i] = c$violence_var[i]
  model_info$int[i] = c$int[i]
  model_info$n[i] = nobs(m)
  model_info$region_rounds[i] = length(unique(m$model[, grepl("region_round", names(m$model))]))
  model_info$AIC[i] = AIC(m)
  model_info$BIC[i] = BIC(m)

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

  # If batch of 6 models
  if(im == mods_per_tab){

    # Check depvars
    dvs = unique(unlist(sapply(model_list, function(x) as.character(summary(x)$terms[[2]]))))
    dvs = recode(dvs, "ethnic_id" = "Ethnic ID", "ethnic_fairtreat" = "Ethnic grievances")
    # Table title
    tabtitle = paste0("Civilian victimization using exposure to violence when respondent was 15-25 years old (within ", c$dist[i], ")\\label{tab:1525_", c$dist[i], "}")

    # Name models
    names(model_list) = paste0("(", 1:8, ")")
    # Check assumption
    if(!grepl("rebel", c$actor[i])){warning("Order of state and rebel might be wrong")}

    # Headers
    h1 = c("", 4, 4)
    names(h1) = c("", dvs[1], dvs[2])
    h2 = c("", rep(2, 4))
    names(h2) = c("", rep(c("State violence", "Rebel violence"), 2))

    # Number of countries and region-rounds
    c_f = function(x){
      length(unique(sapply(str_split(unique(x$data[,"region_round"]), "/"), function(x) x[1])))}
    ctr = sapply(model_list, function(x) c_f(x))
    rr = sapply(model_list, function(x){length(unique(x$data[,"region_round"]))})
    # Extra rows
    extrarows = as.data.frame(rbind(
        c("Controls", rep("Yes", length(model_list))),
        c("Region-round FE", rep("Yes", length(model_list))),
        c("Region-rounds", rr),
        c("Countries", ctr)
      ))

    # Table
    modelsummary(
      models = model_list,
      output = "latex",
      estimate = "{estimate}{stars}",
      coef_map = coef_recode,
      coef_omit = "urban|sex|econ_employment|age|region_round",
      gof_map = gs,
      title = tabtitle,
      add_rows = extrarows,
      threeparttable = TRUE,
      escape = FALSE) %>%
    add_header_above(h2) %>%
    add_header_above(h1) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(general = n, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
    save_kable(file = paste0("models_1525/output/tab_1525_", c$dist[i], ".tex"))

    # Clean up
    model_list = vector("list", mods_per_tab)

  }

}

write.csv(model_info, "models_1525/output/model_info_1525.csv", row.names = FALSE)
write.csv(effects_of_violence, "models_1525/output/effects_of_violence_1525.csv", row.names = FALSE)


########################################

# ## Slightly more restrictive version: location FE

data$round_longlat = paste(data$round, data$longlat, sep = "_")

t = table(data[, "round_longlat"], data[, "expo_1525_10km_bin"])
no_violence_variation = names(t[,2])[prop.table(t, 1)[,2] < 0.03]
no_violence_variation = c(no_violence_variation, names(t[,2])[prop.table(t, 1)[,1] == 0])
df = data[!data[, "round_longlat"] %in% no_violence_variation, ]
# at least 8 people by location
t2 = table(df[, "round_longlat"])
df = df[!df$round_longlat %in% unique(names(t2[t2<8])),]

print("-- Running base models with round_longlat --")

m1a = glm(ethnic_id ~ expo_1525_10km_bin +
  sex + econ_employment + age + factor(round_longlat),
  data = df, family = "binomial")
m1b = glm(ethnic_fairtreat ~ expo_1525_10km_bin +
  sex + econ_employment + age + factor(round_longlat),
  data = df, family = "binomial")

t_int = table(data[, "longlat"], data[, "EOSV_state"])
no_variation_int = names(t_int[, 1])[t_int[,2] == 0 | t_int[,1] == 0]
df = df[!df[, "longlat"] %in% no_variation_int, ]

print("-- Running interaction models with round_longlat --")

m2a = glm(ethnic_id ~ expo_1525_10km_bin * EOSV_state +
  sex + econ_employment + age + factor(round_longlat),
  data = df, family = "binomial")
m2b = glm(ethnic_fairtreat ~ expo_1525_10km_bin * EOSV_state +
  sex + econ_employment + age + factor(round_longlat),
  data = df, family = "binomial")

# Simulations
print("-- Running simulations with round_longlat --")
m1a_sim = simulations(m1a, n_sim = 1000)
m1b_sim = simulations(m1b, n_sim = 1000)
m2a_sim = simulations(m2a, n_sim = 1000)
m2b_sim = simulations(m2b, n_sim = 1000)

# Effects of violence
m1a_eff = violence_effects(dep_var = "ethnic_id", violence_var = "expo_1525_10km_bin",
  estimates = m1a_sim, confidence = 0.95)
m1b_eff = violence_effects(dep_var = "ethnic_fairtreat", violence_var = "expo_1525_10km_bin",
  estimates = m1b_sim, confidence = 0.95)
m2a_eff = violence_effects(dep_var = "ethnic_id", violence_var = "expo_1525_10km_bin",
  estimates = m2a_sim, confidence = 0.95)
m2b_eff = violence_effects(dep_var = "ethnic_fairtreat", violence_var = "expo_1525_10km_bin",
  estimates = m2b_sim, confidence = 0.95)

# Save
write.csv(rbind(m1a_eff, m1b_eff, m2a_eff, m2b_eff),
  "models_1525/output/effects_of_violence_1525_location.csv", row.names = FALSE)
