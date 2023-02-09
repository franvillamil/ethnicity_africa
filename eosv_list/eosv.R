# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "countrycode")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

# EPR for references
epr = read.csv("input_data/epr_groupdata.csv") %>%
  mutate(region = countrycode(countries_gwid, "gwn", "un.region.name", warn = FALSE)) %>%
  filter(region == "Africa") %>%
  mutate(iso3c = countrycode(countries_gwid, "gwn", "iso3c")) %>%
  select(gwgroupid, groupname, countryname, iso3c) %>%
  unique()

# EOSV
eosv = read.csv("input_data/eosv_group_year.csv") %>%
  filter(EthnGrID %in% epr$gwgroupid) %>%
  rename(gwgroupid = EthnGrID, year = Year) %>%
  mutate(OSV_Gov_Est = ifelse(is.na(OSV_Gov_Est), 0, OSV_Gov_Est)) %>%
  mutate(OSV_Reb_Est = ifelse(is.na(OSV_Reb_Est), 0, OSV_Reb_Est)) %>%
  select(gwgroupid, year, OSV_Gov_Est, Target_Gov, OSV_Reb_Est, Target_Reb)

# Get earliest & all years with state violence for each group
eosv_state = eosv %>%
  filter(OSV_Gov_Est > 0) %>%
  group_by(gwgroupid) %>%
  summarize(
    earliest_OSV = min(year, na.rm = TRUE),
    earliest_EOSV = ifelse(any(Target_Gov == 1),
      min(year[Target_Gov == 1]), NA),
    years_OSV = paste(year, collapse = ";"),
    years_EOSV = paste(year[Target_Gov == 1], collapse = ";")) %>%
  mutate(only_EOSV = ifelse(years_OSV == years_EOSV, 1, 0)) %>%
  as.data.frame()

# Merge with groupname & country info
eosv_state = merge(eosv_state,
  epr[, c("gwgroupid", "groupname", "iso3c")], all.x = TRUE)

# Repeat for rebel violence
eosv_rebel = eosv %>%
  filter(OSV_Reb_Est > 0) %>%
  group_by(gwgroupid) %>%
  summarize(
    earliest_OSV = min(year, na.rm = TRUE),
    earliest_EOSV = ifelse(any(Target_Reb == 1),
      min(year[Target_Reb == 1]), NA),
    years_OSV = paste(year, collapse = ";"),
    years_EOSV = paste(year[Target_Reb == 1], collapse = ";")) %>%
  mutate(only_EOSV = ifelse(years_OSV == years_EOSV, 1, 0)) %>%
  as.data.frame()

# Merge with groupname & country info
eosv_rebel = merge(eosv_rebel,
  epr[, c("gwgroupid", "groupname", "iso3c")], all.x = TRUE)

## Produce tables
eosv_state_tab = subset(eosv_state, only_EOSV == 1,
  select = c("groupname", "iso3c", "earliest_EOSV"))
eosv_state_tab$iso3c = countrycode(eosv_state_tab$iso3c, "iso3c", "country.name")

if(nrow(eosv_state_tab) != 37){stop("nrow changed!!!")}

eosv_state_tex = c(
  paste(eosv_state_tab[1,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[2,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[3,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[4,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[5,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[6,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[7,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[8,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[9,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[10,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[11,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[12,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[13,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[14,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[15,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[16,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[17,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[18,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[19,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[20,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[21,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[22,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[23,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[24,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[25,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[26,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[27,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[28,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[29,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[30,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[31,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[32,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[33,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[34,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[35,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[36,], collapse = " & "), " \\\\", "\n",
  paste(eosv_state_tab[37,], collapse = " & "), " \\\\", "\n")

filecon = file("eosv_list/output/eosv_state_tab.tex")
writeLines(eosv_state_tex, filecon)
close(filecon)

# Same for rebel EOSV
eosv_rebel_tab = subset(eosv_rebel, only_EOSV == 1,
  select = c("groupname", "iso3c", "earliest_EOSV"))
eosv_rebel_tab$iso3c = countrycode(eosv_rebel_tab$iso3c, "iso3c", "country.name")

if(nrow(eosv_rebel_tab) != 16){stop("nrow changed!!!")}

eosv_rebel_tex = c(
  paste(eosv_rebel_tab[1,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[2,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[3,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[4,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[5,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[6,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[7,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[8,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[9,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[10,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[11,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[12,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[13,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[14,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[15,], collapse = " & "), " \\\\", "\n",
  paste(eosv_rebel_tab[16,], collapse = " & "), " \\\\", "\n")

filecon = file("eosv_list/output/eosv_rebel_tab.tex")
writeLines(eosv_rebel_tex, filecon)
close(filecon)

## SAVE
write.csv(eosv_state, "eosv_list/output/eosv_state.csv", row.names = FALSE)
write.csv(eosv_rebel, "eosv_list/output/eosv_rebel.csv", row.names = FALSE)
