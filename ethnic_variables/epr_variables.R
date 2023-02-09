# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "stringr", "countrycode", "parallel")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

# Input
epr = read.csv("input_data/epr_groupdata.csv") %>%
  mutate(iso3c = countrycode(countries_gwid, "gwn", "iso3c", warn = FALSE))
ab2epr = read.csv("epr_match/output/ab_epr_match.csv")
eosv_rebel = read.csv("eosv_list/output/eosv_rebel.csv")
eosv_state = read.csv("eosv_list/output/eosv_state.csv")


## EOSV

# Baseline
ab2epr$OSV_state = 0
ab2epr$OSV_rebel = 0
ab2epr$EOSV_state = 0
ab2epr$EOSV_rebel = 0

# Assign state-led EOSV
for(i in 1:nrow(eosv_state)){
  ab2epr$EOSV_state[ ab2epr$country_iso3c == eosv_state[i, "iso3c"] &
    grepl(eosv_state[i, "gwgroupid"], ab2epr$gwgroupid2) &
    ab2epr$year > eosv_state[i, "earliest_EOSV"] ] = 1
  ab2epr$OSV_state[ ab2epr$country_iso3c == eosv_state[i, "iso3c"] &
    grepl(eosv_state[i, "gwgroupid"], ab2epr$gwgroupid2) &
    ab2epr$year > eosv_state[i, "earliest_OSV"] ] = 1
}

# Assign rebel-led EOSV
for(i in 1:nrow(eosv_rebel)){
  ab2epr$EOSV_rebel[ ab2epr$country_iso3c == eosv_rebel[i, "iso3c"] &
    grepl(eosv_rebel[i, "gwgroupid"], ab2epr$gwgroupid2) &
    ab2epr$year > eosv_rebel[i, "earliest_EOSV"] ] = 1
  ab2epr$OSV_rebel[ ab2epr$country_iso3c == eosv_rebel[i, "iso3c"] &
    grepl(eosv_rebel[i, "gwgroupid"], ab2epr$gwgroupid2) &
    ab2epr$year > eosv_rebel[i, "earliest_OSV"] ] = 1
}

## Status name

# Function
get_statusname = function(g, c, y){
  g_input = unlist(str_split(g, ";"))
  status = subset(epr, iso3c == c & year == y & gwgroupid %in% g_input)$statusname
  result = paste(status, collapse = ";")
  return(result)
}

# Number of cores
nc = detectCores()

# Apply function
ab2epr$statusname = mcmapply(get_statusname,
  g = ab2epr$gwgroupid2, c = ab2epr$country_iso3c, y = ab2epr$year,
  mc.cores = nc)

## Group size

# Function
get_groupsize = function(g, c, y){
  g_input = unlist(str_split(g, ";"))
  gsize = subset(epr, iso3c == c & year == y & gwgroupid %in% g_input)$groupsize
  result = ifelse(length(gsize) == 1, gsize, paste0(min(gsize), "/", max(gsize)))
  return(result)
}

# Apply function
ab2epr$groupsize = mcmapply(get_groupsize,
  g = ab2epr$gwgroupid2, c = ab2epr$country_iso3c, y = ab2epr$year,
  mc.cores = nc)

# Get min and max for multiple links
ab2epr$groupsize_min = as.numeric(sapply(ab2epr$groupsize, function(x) str_split(x, "/")[[1]][1]))
ab2epr$groupsize_max = as.numeric(sapply(ab2epr$groupsize, function(x) str_split(x, "/")[[1]][2]))
ab2epr$groupsize_max = ifelse(!grepl("/", ab2epr$groupsize),
  ab2epr$groupsize_min, ab2epr$groupsize_max)
ab2epr$groupsize_min[ab2epr$groupsize_min %in% c(-Inf, Inf)] = NA
ab2epr$groupsize_max[ab2epr$groupsize_max %in% c(-Inf, Inf)] = NA

# Get marker for multiple link
ab2epr$epr_multiple = grepl(";", ab2epr$gwgroupid2)
ab2epr$epr_multiple = ifelse(is.na(ab2epr$gwgroupid2), NA, ab2epr$epr_multiple)

## Limit variables
ab2epr = ab2epr[, c("round", "year", "respno", "gwgroupid2", "epr_multiple",
  "groupsize_max", "groupsize_min", "statusname", "OSV_state", "EOSV_state", "OSV_rebel", "EOSV_rebel")]

## SAVE
write.csv(ab2epr, "ethnic_variables/output/ab_epr_variables.csv", row.names = FALSE)
