# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "stringr", "sf")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# Switching off spherical geometry (for overlay)
sf::sf_use_s2(FALSE)

# ------------------------------

# Afrobarometer core dataset
ab = read.csv("initial_subset/output/ab.csv")

# Coded variables
ab_dist_capital = readRDS("dist_nat_capital/output/dist_capital.rds")
ab_violence = read.csv("exposure_to_violence/output/ab_violence_exposure.csv")
ab_epr = read.csv("ethnic_variables/output/ab_epr_variables.csv")

# ---------------------------------
# Merging
ab = merge(ab, ab_dist_capital)
ab = merge(ab, ab_epr)
ab = merge(ab, ab_violence)
if(nrow(ab) != nrow(ab_dist_capital)){stop("Something is wrong")}

# ---------------------------------
## Get ADM1-level region using geolocation
# Create spatial version of ab
ab$longlatID = paste0(ab$longitude, ab$latitude)
ab_locs = unique(ab[, c("longlatID", "longitude", "latitude")])
ab_sp = st_as_sf(x = ab_locs, coords = c("longitude", "latitude"), crs = 4326)
# Read geopackage
ADM1 = st_read("input_data/africa_ADM1.shp") %>%
  # st_transform(4326) %>%
  select(COUNTRY, NAME_1)
# Spatial overlay
ab_in_ADM1 = st_join(ab_sp, ADM1, join = st_within)
ab_overlay = ab_in_ADM1 %>%
  as.data.frame() %>%
  rename(region = NAME_1) %>%
  select(longlatID, region)
ab = merge(ab, ab_overlay, all.x = TRUE)

# NOTE: not doing it because result is the same
# try again with https://epsg.io/32732 and compare
# (https://gis.stackexchange.com/a/396685/41500)
# ab_spUTM = ab_sp %>%
#   st_transform(32732)
# ADM1UTM = ADM1 %>%
#   st_transform(32732)
# ab_in_ADM1UTM = st_join(ab_spUTM, ADM1UTM, join = st_within)
# ab_overlayUTM = ab_in_ADM1UTM %>%
#   as.data.frame() %>%
#   rename(regionUTM = NAME_1) %>%
#   select(longlatID, regionUTM)
# ab = merge(ab, ab_overlayUTM, all.x = TRUE)

# ---------------------------------
## Creating/modifying variables
data = ab %>%
  # Country- and region-round
  mutate(country_round = paste(country_iso3c, round, sep = "/")) %>%
  mutate(region_round = paste(country_iso3c, region, round, sep = "/")) %>%
  # Ethnic status - link to group in govt? to group discriminated?
  mutate(
    link_epr_included = ifelse(
      grepl("(SENIOR)|(JUNIOR) PARTNER|DOMINANT|MONOPOLY", statusname), 1, 0),
    link_epr_excluded = ifelse(grepl("DISCRIMINATED|POWERLESS", statusname), 1, 0)) %>%
  # Dist to national capital to log
  mutate(dist_capital_km = log(dist_capital_km + 1)) %>%
  # Dvs: Ethnic
  mutate(ethnic_id = identity_choice_ethnic) %>%
  mutate(ethnic_id = ifelse(subj_ethid_o %in% 1:2, 1, ethnic_id)) %>%
  mutate(ethnic_id = ifelse(subj_ethid_o %in% 3:5, 0, ethnic_id)) %>%
  mutate(ethnic_id_ord = abs(subj_ethid_o - 6)) %>%
  mutate(ethnic_fairtreat = ifelse(grief_ethfairtreat >= 2, 1, 0)) %>%
  mutate(ethnic_fairtreat_ord = grief_ethfairtreat) %>%
  mutate(
  # DVs: Trust
    trust_bin_general = trust_general,
    trust_bin_president = ifelse(trust_president >= 2, 1, 0),
    trust_bin_police = ifelse(trust_police >= 2, 1, 0),
    trust_bin_army = ifelse(trust_army >= 2, 1, 0),
    trust_bin_tradleaders = ifelse(trust_tradleaders >= 2, 1, 0)
  ) %>%
  mutate(
  # DVs: Participation
    part_discuss_politics = ifelse(politics_discuss > 1, 1, 0),
    part_contact_locgov = ifelse(contact_locgov > 0, 1, 0),
    part_contact_govoff = ifelse(contact_govoff > 0, 1, 0),
    part_contact_tradauth = ifelse(contact_tradauth > 0, 1, 0),
    part_contact_relauth = ifelse(contact_relauth > 0, 1, 0)
  ) %>%
  mutate(
  # DVs: Attitudes
    att_use_of_violence = ifelse(att_violence > 3, 1, 0),
    att_legit_courts = ifelse(legit_courts > 3, 1, 0),
    att_legit_police = ifelse(legit_police > 3, 1, 0),
    att_legit_gov = ifelse(legit_gov > 3, 1, 0),
    att_approval_president = ifelse(approval_president > 2, 1, 0),
    att_approval_locgovcouncil = ifelse(approval_locgovcouncil > 2, 1, 0)
  ) %>%
  mutate(
  # Violence variables
    expo_5km_bin = ifelse(expo_5km > 0, 1, 0),
    expo_10km_bin = ifelse(expo_10km > 0, 1, 0),
    expo_25km_bin = ifelse(expo_25km > 0, 1, 0),
    expo_50km_bin = ifelse(expo_50km > 0, 1, 0),
    expo_1525_5km_bin = ifelse(expo_1525_5km > 0, 1, 0),
    expo_1525_10km_bin = ifelse(expo_1525_10km > 0, 1, 0),
    expo_1525_25km_bin = ifelse(expo_1525_25km > 0, 1, 0),
    expo_1525_50km_bin = ifelse(expo_1525_50km > 0, 1, 0),
    expo_2yr_5km_bin = ifelse(expo_2yr_5km > 0, 1, 0),
    expo_2yr_10km_bin = ifelse(expo_2yr_10km > 0, 1, 0),
    expo_2yr_25km_bin = ifelse(expo_2yr_25km > 0, 1, 0),
    expo_2yr_50km_bin = ifelse(expo_2yr_50km > 0, 1, 0),
    expo_5yr_5km_bin = ifelse(expo_5yr_5km > 0, 1, 0),
    expo_5yr_10km_bin = ifelse(expo_5yr_10km > 0, 1, 0),
    expo_5yr_25km_bin = ifelse(expo_5yr_25km > 0, 1, 0),
    expo_5yr_50km_bin = ifelse(expo_5yr_50km > 0, 1, 0),
    expo_bf2yr_5km_bin = ifelse(expo_bf2yr_5km > 0, 1, 0),
    expo_bf2yr_10km_bin = ifelse(expo_bf2yr_10km > 0, 1, 0),
    expo_bf2yr_25km_bin = ifelse(expo_bf2yr_25km > 0, 1, 0),
    expo_bf2yr_50km_bin = ifelse(expo_bf2yr_50km > 0, 1, 0),
    expo_bf5yr_5km_bin = ifelse(expo_bf5yr_5km > 0, 1, 0),
    expo_bf5yr_10km_bin = ifelse(expo_bf5yr_10km > 0, 1, 0),
    expo_bf5yr_25km_bin = ifelse(expo_bf5yr_25km > 0, 1, 0),
    expo_bf5yr_50km_bin = ifelse(expo_bf5yr_50km > 0, 1, 0),
    expo_reb_5km_bin = ifelse(expo_reb_5km > 0, 1, 0),
    expo_reb_10km_bin = ifelse(expo_reb_10km > 0, 1, 0),
    expo_reb_25km_bin = ifelse(expo_reb_25km > 0, 1, 0),
    expo_reb_50km_bin = ifelse(expo_reb_50km > 0, 1, 0),
    expo_reb_1525_5km_bin = ifelse(expo_reb_1525_5km > 0, 1, 0),
    expo_reb_1525_10km_bin = ifelse(expo_reb_1525_10km > 0, 1, 0),
    expo_reb_1525_25km_bin = ifelse(expo_reb_1525_25km > 0, 1, 0),
    expo_reb_1525_50km_bin = ifelse(expo_reb_1525_50km > 0, 1, 0),
    expo_reb_2yr_5km_bin = ifelse(expo_reb_2yr_5km > 0, 1, 0),
    expo_reb_2yr_10km_bin = ifelse(expo_reb_2yr_10km > 0, 1, 0),
    expo_reb_2yr_25km_bin = ifelse(expo_reb_2yr_25km > 0, 1, 0),
    expo_reb_2yr_50km_bin = ifelse(expo_reb_2yr_50km > 0, 1, 0),
    expo_reb_5yr_5km_bin = ifelse(expo_reb_5yr_5km > 0, 1, 0),
    expo_reb_5yr_10km_bin = ifelse(expo_reb_5yr_10km > 0, 1, 0),
    expo_reb_5yr_25km_bin = ifelse(expo_reb_5yr_25km > 0, 1, 0),
    expo_reb_5yr_50km_bin = ifelse(expo_reb_5yr_50km > 0, 1, 0),
    expo_reb_bf2yr_5km_bin = ifelse(expo_reb_bf2yr_5km > 0, 1, 0),
    expo_reb_bf2yr_10km_bin = ifelse(expo_reb_bf2yr_10km > 0, 1, 0),
    expo_reb_bf2yr_25km_bin = ifelse(expo_reb_bf2yr_25km > 0, 1, 0),
    expo_reb_bf2yr_50km_bin = ifelse(expo_reb_bf2yr_50km > 0, 1, 0),
    expo_reb_bf5yr_5km_bin = ifelse(expo_reb_bf5yr_5km > 0, 1, 0),
    expo_reb_bf5yr_10km_bin = ifelse(expo_reb_bf5yr_10km > 0, 1, 0),
    expo_reb_bf5yr_25km_bin = ifelse(expo_reb_bf5yr_25km > 0, 1, 0),
    expo_reb_bf5yr_50km_bin = ifelse(expo_reb_bf5yr_50km > 0, 1, 0),
    expo_1yr_10km_bin = ifelse(expo_1yr_10km > 0, 1, 0),
    expo_3yr_10km_bin = ifelse(expo_3yr_10km > 0, 1, 0),
    expo_4yr_10km_bin = ifelse(expo_4yr_10km > 0, 1, 0),
    expo_6yr_10km_bin = ifelse(expo_6yr_10km > 0, 1, 0),
    expo_7yr_10km_bin = ifelse(expo_7yr_10km > 0, 1, 0),
    expo_8yr_10km_bin = ifelse(expo_8yr_10km > 0, 1, 0),
    expo_9yr_10km_bin = ifelse(expo_9yr_10km > 0, 1, 0),
    expo_10yr_10km_bin = ifelse(expo_10yr_10km > 0, 1, 0)
  ) %>%
  mutate(
    # Combined violence variables
    expo_all_5km_bin = ifelse(expo_5km_bin|expo_reb_5km_bin, 1, 0),
    expo_all_10km_bin = ifelse(expo_10km_bin|expo_reb_10km_bin, 1, 0),
    expo_all_25km_bin = ifelse(expo_25km_bin|expo_reb_25km_bin, 1, 0),
    expo_all_50km_bin = ifelse(expo_50km_bin|expo_reb_50km_bin, 1, 0),
    expo_all_1525_5km_bin = ifelse(expo_1525_5km_bin|expo_reb_1525_5km_bin, 1, 0),
    expo_all_1525_10km_bin = ifelse(expo_1525_10km_bin|expo_reb_1525_10km_bin, 1, 0),
    expo_all_1525_25km_bin = ifelse(expo_1525_25km_bin|expo_reb_1525_25km_bin, 1, 0),
    expo_all_1525_50km_bin = ifelse(expo_1525_50km_bin|expo_reb_1525_50km_bin, 1, 0),
    expo_all_2yr_5km_bin = ifelse(expo_2yr_5km_bin|expo_reb_2yr_5km_bin, 1, 0),
    expo_all_2yr_10km_bin = ifelse(expo_2yr_10km_bin|expo_reb_2yr_10km_bin, 1, 0),
    expo_all_2yr_25km_bin = ifelse(expo_2yr_25km_bin|expo_reb_2yr_25km_bin, 1, 0),
    expo_all_2yr_50km_bin = ifelse(expo_2yr_50km_bin|expo_reb_2yr_50km_bin, 1, 0),
    expo_all_5yr_5km_bin = ifelse(expo_5yr_5km_bin|expo_reb_5yr_5km_bin, 1, 0),
    expo_all_5yr_10km_bin = ifelse(expo_5yr_10km_bin|expo_reb_5yr_10km_bin, 1, 0),
    expo_all_5yr_25km_bin = ifelse(expo_5yr_25km_bin|expo_reb_5yr_25km_bin, 1, 0),
    expo_all_5yr_50km_bin = ifelse(expo_5yr_50km_bin|expo_reb_5yr_50km_bin, 1, 0),
    expo_all_bf2yr_5km_bin = ifelse(expo_bf2yr_5km_bin|expo_reb_bf2yr_5km_bin, 1, 0),
    expo_all_bf2yr_10km_bin = ifelse(expo_bf2yr_10km_bin|expo_reb_bf2yr_10km_bin, 1, 0),
    expo_all_bf2yr_25km_bin = ifelse(expo_bf2yr_25km_bin|expo_reb_bf2yr_25km_bin, 1, 0),
    expo_all_bf2yr_50km_bin = ifelse(expo_bf2yr_50km_bin|expo_reb_bf2yr_50km_bin, 1, 0),
    expo_all_bf5yr_5km_bin = ifelse(expo_bf5yr_5km_bin|expo_reb_bf5yr_5km_bin, 1, 0),
    expo_all_bf5yr_10km_bin = ifelse(expo_bf5yr_10km_bin|expo_reb_bf5yr_10km_bin, 1, 0),
    expo_all_bf5yr_25km_bin = ifelse(expo_bf5yr_25km_bin|expo_reb_bf5yr_25km_bin, 1, 0),
    expo_all_bf5yr_50km_bin = ifelse(expo_bf5yr_50km_bin|expo_reb_bf5yr_50km_bin, 1, 0)
  ) %>%
  mutate(
    # EOSV state OR rebel
    OSV_any = ifelse(OSV_state|OSV_rebel, 1, 0),
    EOSV_any = ifelse(EOSV_state|EOSV_rebel, 1, 0)
  ) %>%
  # Coordinates (specific place)
  mutate(longlat = paste(latitude, longitude, sep = ","))

# ---------------------------------
# Limit dataset size
vars = c(
  # IDs
  "respno", "year", "round", "country_name", "country_id", "longlat",
  "country_iso3c", "region", "country_round", "region_round",
  # Controls
  "urban", "sex", "econ_employment", "age", "dist_capital_km",
  # Dependent variables
  "ethnic_id", "ethnic_fairtreat", "ethnic_id_ord", "ethnic_fairtreat_ord",
  # "trust_bin_general", "trust_bin_president",
  # "trust_bin_police", "trust_bin_army", "trust_bin_tradleaders",
  # "part_discuss_politics", "part_contact_locgov", "part_contact_govoff",
  # "part_contact_tradauth", "part_contact_relauth",
  # "att_use_of_violence", "att_legit_courts", "att_legit_police",
  # "att_legit_gov", "att_approval_president", "att_approval_locgovcouncil",
  # EPR
  "epr_multiple", "link_epr_included", "link_epr_excluded", "groupsize_max", "groupsize_min")
# Add all OSV/EOSV/exposure variables
vars = c(vars, names(data)[grepl("^expo|EOSV|OSV", names(data))])
# Limit
data = data[, vars]

# ---------------------------------
## Remove exposure variables by BOTH actors
data = data[, !names(data) %in%
  c(names(data)[grepl("^expo_all", names(data))], "OSV_any", "EOSV_any")]

# ---------------------------------
## Remove non-binary violence variables to save space
data = data[, names(data)[!grepl("\\dkm$", names(data))]]

# ---------------------------------
# ---------------------------------
## SAVE
write.csv(data, "dataset/output/data.csv", row.names = FALSE)
