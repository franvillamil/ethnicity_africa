# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "stringr", "countrycode")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# muniSpain and elecciones (Github)
if(!all(c("muniSpain", "LEDA") %in% rownames(installed.packages()))){
  if(!"devtools" %in% rownames(installed.packages())){install.packages("devtools")}
  library(devtools)
  install_github("franvillamil/muniSpain")
  install_github("carl-mc/LEDA")
}
# Load
lapply(c(pkg, "muniSpain", "LEDA"), library, character.only = TRUE)

# ------------------------------

# Inputs
ab = read.csv("initial_subset/output/ab.csv") %>%
  mutate(language = adapt(language), iso3c = country_iso3c)
# epr = read.csv("input/epr_groupdata.csv") %>%
epr = read.csv("input_data/epr_groupdata.csv") %>%
  filter(isactive == 1 &
    countries_gwid %in% ab$country_id &
    year %in% ab$year) %>%
  select(countries_gwid, countryname, year, groupname, gwgroupid) %>%
  mutate(iso3c = countrycode(countries_gwid, "gwn", "iso3c"))


## Get ethnic links to EPR data from LEDA package

# Set countries and years included
clist = unique(ab$iso3c)
ylist = unique(ab$year)

# Initialize LEDA object
leda = LEDA$new()

# Get links - using 'language' at the dialect level (see Müller-Crepon et al 2020)
leda_links = leda$link_set(
  lists.a = list(type = "Afrobarometer", marker = "language", iso3c = clist),
  lists.b = list(type = "EPR", iso3c = clist, year = ylist),
  link.level = "dialect",
  by.country = TRUE)
# Filter ab_year == NA (no matchs) & relevant variables
leda_links = leda_links %>%
  filter(!is.na(b.year)) %>%
  # NOTE: Problem (LEDA coding?) - not subsetting EPR
  filter(paste(b.group, b.year) %in% paste(epr$groupname, epr$year)) %>%
  select(iso3c, a.round, b.year, a.group, b.group) %>%
  # Get gwgroupid
  mutate(gwgroupid = epr$gwgroupid[match(
    paste0(iso3c, b.group), paste0(epr$iso3c, epr$groupname))]) %>%
  # NOTE: Problem - multiple matches
  group_by(iso3c, a.round, b.year, a.group) %>%
  summarize(
    iso3c = unique(iso3c),
    a.round = unique(a.round),
    b.year = unique(b.year),
    a.group = unique(a.group),
    b.group_n = length(b.group),
    b.group = paste(b.group, collapse = ";"),
    gwgroupid = paste(gwgroupid, collapse = ";"))

# Merge
ab = merge(ab, leda_links,
  by.x = c("iso3c", "round", "year", "language"),
  by.y = c("iso3c", "a.round", "b.year", "a.group"),
  all.x = TRUE)

## NOTE:
# ft = round(prop.table(table(ab$country_name, ab$b.group_n, useNA="always"), 1), 2)
# ft[order(ft[,1]),]
# ft[order(ft[,2],decreasing=T),]

#               Good  2/3match None
# Burundi       0.00 0.99 0.00 0.01
# Morocco       0.13 0.87 0.00 0.00
# Zimbabwe      0.35 0.62 0.00 0.03
# South Africa  0.67 0.32 0.00 0.01
# ...
# Liberia       0.22 0.00 0.00 0.78
# Mozambique    0.25 0.00 0.00 0.75
# Togo          0.37 0.00 0.00 0.63
# Cameroon      0.41 0.06 0.00 0.53
# Tanzania      0.41 0.05 0.00 0.55

# Manual fix for countries with many double-matches
# Get info from ethnicity or turn to NA
ab = ab %>%
  mutate(gwgroupid2 = gwgroupid) %>%
  # Burundi (info for round 5 only)
  mutate(gwgroupid2 = ifelse(
    ethnicity == "Hutu" & iso3c == "BDI", "51601000", gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(
    ethnicity == "Tutsi" & iso3c == "BDI", "51602000", gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(
    gwgroupid2 == "51601000;51602000", NA, gwgroupid2)) %>%
  # Morocco (limited info for round 6)
  mutate(gwgroupid2 = ifelse(
    ethnicity %in% c("Rifi", "Soussi", "Chalh", "Amazigh") & iso3c == "MAR",
    "60001000", gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(
    ethnicity == "Sahraoui" & iso3c == "MAR", "60003000", gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(
    ethnicity == "Arab" & iso3c == "MAR", "60002000", gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(gwgroupid2 == "60002000;60003000", NA, gwgroupid2)) %>%
  # Zimbabwe (remove Shona/Ndebele double match, and assign Shona based on language)
  mutate(gwgroupid2 = ifelse(
    gwgroupid2 %in% c("55201200;55201100", "55201100;55201200"), NA, gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(
    language == "Shona" & iso3c == "ZWE", "55201200", gwgroupid2)) %>%
  # South Africa (Colores/Afrikaners/English Speakers)
  mutate(gwgroupid2 = ifelse(
    ethnicity == "English" & iso3c == "ZAF", "56004000", gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(
    ethnicity == "Coloured" & iso3c == "ZAF", "56003000", gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(
    ethnicity %in% c("Afrikaaner", "Afrikaans/ afrikaner/ boer",
      "Afrikaans/Afrikaner/Boer", "Afrikaner") & iso3c == "ZAF", "56001000", gwgroupid2)) %>%
  mutate(gwgroupid2 = ifelse(
    gwgroupid2 %in% c("56003000;56004000", "56001000;56003000"), NA, gwgroupid2)) %>%
  # Recalculate number of matches
  mutate(link_n = sapply(gwgroupid2, function(x) length(str_split(x, ";")[[1]]))) %>%
  mutate(link_n = ifelse(is.na(gwgroupid2), NA, link_n))

# Select variables
ab = ab %>%
  select(round, year, respno, country_name, country_iso3c, b.group, gwgroupid2, link_n)

## SAVE
write.csv(ab, "epr_match/output/ab_epr_match.csv", row.names = FALSE)


# # Discarded
#
# mutate(b.group2 = ifelse(ethnicity == "Hutu" & iso3c == "BDI", "Hutu", b.group2)) %>%
# mutate(b.group2 = ifelse(ethnicity == "Tutsi" & iso3c == "BDI", "Tutsi", b.group2)) %>%
# mutate(b.group2 = ifelse(b.group2 == "Hutu;Tutsi" & iso3c == "BDI", NA, b.group2)) %>%
# # Morocco (limited info for round 6)
# mutate(b.group2 = ifelse(ethnicity %in% c("Rifi", "Soussi", "Chalh", "Amazigh") &
#   iso3c == "MAR", "Berbers", b.group2)) %>%
# mutate(b.group2 = ifelse(ethnicity == "Sahraoui" & iso3c == "MAR", "Sahrawis", b.group2)) %>%
# mutate(b.group2 = ifelse(ethnicity == "Arab" & iso3c == "MAR", "Arabs", b.group2)) %>%
# mutate(b.group2 = ifelse(b.group2 == "Arabs;Sahrawis" & iso3c == "MAR", NA, b.group2)) %>%
# # Zimbabwe (remove Shona/Ndebele double match, and assign Shona based on language)
# mutate(b.group2 = ifelse(b.group2 %in% c("Ndebele-Kalanga-(Tonga);Shona",
#   "Shona;Ndebele-Kalanga-(Tonga)") & iso3c == "ZWE", NA, b.group2)) %>%
# mutate(b.group2 = ifelse(language == "Shona" & iso3c == "ZWE", "Shona", b.group2)) %>%
# # South Africa (Colores/Afrikaners/English Speakers)
# mutate(b.group2 = ifelse(ethnicity == "English" & iso3c == "ZAF",
#   "English Speakers", b.group2)) %>%
# mutate(b.group2 = ifelse(ethnicity == "Coloured" & iso3c == "ZAF",
#   "Coloreds", b.group2)) %>%
# mutate(b.group2 = ifelse(ethnicity %in% c("Afrikaaner", "Afrikaans/ afrikaner/ boer",
#   "Afrikaans/Afrikaner/Boer", "Afrikaner") & iso3c == "ZAF",
#   "Afrikaners", b.group2)) %>%
# mutate(b.group2 = ifelse(b.group2 %in%
#   c("Afrikaners;Coloreds", "Coloreds;English Speakers") & iso3c == "ZAF",
#   NA, b.group2)) %>%
# # Recalculate number of matches
#   mutate(b.group2_n = sapply(b.group2, function(x) length(str_split(x, ";")[[1]]))) %>%
#   mutate(b.group2_n = ifelse(is.na(b.group2), NA, b.group2_n))
