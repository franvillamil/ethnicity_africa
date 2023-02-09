# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "tidyr", "ggplot2", "rgdal",
  "countrycode", "RColorBrewer", "stringr")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------
# Inputs

# Afrobaro
ab = read.csv("initial_subset/output/ab.csv") %>%
  select(respno, round, latitude, longitude, year, country_id)
# Data and merge (exposure)
data = read.csv("dataset/output/data.csv")
ab = merge(ab, data)
# GED
ged = read.csv("initial_subset/output/ged_state.csv") %>%
  filter(!country %in% c("Botswana", "Morocco", "Kingdom of eSwatini (Swaziland)", "Zambia"))
ged_r = read.csv("initial_subset/output/ged_rebel.csv") %>%
  filter(!country %in% c("Botswana", "Morocco", "Kingdom of eSwatini (Swaziland)", "Zambia"))
ged_all = rbind(ged, ged_r)
# Shapefile
gadm = readOGR("input_data/gadm36_africa.shp", layer = "gadm36_africa")
# EOSV
eosv = read.csv("input_data/eosv_group_year.csv")

# ------------------------------
# Basic descriptives

# # List of regions by country
# regions = ab %>%
#   select(country_name, region) %>%
#   unique() %>%
#   group_by(country_name) %>%
#   summarize(Regions = paste(region, collapse = "; ")) %>%
#   rename(Country = country_name)
#
# regions_t = kable(regions, "latex") %>%
#   kable_styling(latex_options = c("hold_position", "scale_down"))

# Table of observations by country and round
obscr = as.data.frame(table(data$country_name, data$round)) %>%
  pivot_wider(names_from = Var2, values_from = Freq) %>%
  mutate(Var1 = as.character(Var1)) %>%
  mutate(Var1 = ifelse(Var1 == "Cote D'Ivoire", "C\\^{o}te d'Ivoire", Var1))

obscr_tex = apply(obscr, 1, function(x) paste(paste(x, collapse = " & "), " \\\\"))

filecon = file("descriptives/output/obs_country_round.tex")
writeLines(obscr_tex, filecon)
close(filecon)

# Table of exposed individuals by country and round (10km)
t = prop.table(with(data, table(country_round, expo_10km_bin)), 1)
# round(t, 2)[order(t[,1]),]

## Main variables summary

covs = data[, c("ethnic_id", "ethnic_fairtreat",
  "expo_5yr_10km_bin", "expo_bf5yr_10km_bin", "expo_2yr_10km_bin",
  "expo_bf2yr_10km_bin", "expo_1525_10km_bin",
  "sex", "urban", "age", "econ_employment", "dist_capital_km")] %>%
  rename(
    `Ethnic identification` = `ethnic_id`,
    `Ethnic grievances` = `ethnic_fairtreat`,
    `Violence previous 5 years, 10km` = `expo_5yr_10km_bin`,
    `Violence before last 5 years, 10km` = `expo_bf5yr_10km_bin`,
    `Violence previous 2 years, 10km` = `expo_2yr_10km_bin`,
    `Violence before last 2 years, 10km` = `expo_bf2yr_10km_bin`,
    `Violence 15-25yr old, 10km` = `expo_1525_10km_bin`,
    `Female` = `sex`,
    `Urban resident` = `urban`,
    `Age` = `age`,
    `Employment situation (0-2)` = `econ_employment`,
    `Dist to national capital (km, log)` = `dist_capital_km`)

# Create table df
descs = vector()
for(i in 1:ncol(covs)){
  stats = round(summary(covs[, i]), 2)
  if(length(stats) == 6){stats = c(stats, 0)}
  descs = rbind(descs, stats)
}
descs = as.data.frame(descs)
descs = cbind(names(covs), descs)
names(descs) = c("Variable", "Min", "Q1", "Median", "Mean", "Q3", "Max", "NA")

# Write tex table
fileconnection = file("descriptives/output/tab_summary_stats.tex")
writeLines(
  paste0(
    "\\begin{table}[!htbp] \\centering", "\n",
    "\\caption{Summary statistics for main variables}", "\n",
    "\\label{tab:sumstats}", "\n",
    "\\small", "\n",
    paste0("\\begin{tabular}{l", strrep("c", ncol(descs)-1), "}"), "\n",
    "\\\\[-1.8ex]\\hline", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    "\\\\[-1.8ex]", "\n",
    paste(names(descs), collapse = " & "), " \\\\", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    paste(descs[1,], collapse = " & "), " \\\\", "\n",
    paste(descs[2,], collapse = " & "), " \\\\", "\n",
    paste(descs[3,], collapse = " & "), " \\\\", "\n",
    paste(descs[4,], collapse = " & "), " \\\\", "\n",
    paste(descs[5,], collapse = " & "), " \\\\", "\n",
    paste(descs[6,], collapse = " & "), " \\\\", "\n",
    paste(descs[7,], collapse = " & "), " \\\\", "\n",
    paste(descs[8,], collapse = " & "), " \\\\", "\n",
    paste(descs[9,], collapse = " & "), " \\\\", "\n",
    paste(descs[10,], collapse = " & "), " \\\\", "\n",
    paste(descs[11,], collapse = " & "), " \\\\", "\n",
    paste(descs[12,], collapse = " & "), " \\\\", "\n",
    "\\hline", "\n",
    "\\hline \\\\[-1.8ex]", "\n",
    "\\end{tabular}", "\n",
    "\\end{table}", "\n"
  ), fileconnection)
close(fileconnection)

## Dependent variable descriptives (by country/round)

dv_desc = data %>%
  group_by(country_round) %>%
  summarize(
    eth_id = mean(ethnic_id, na.rm = TRUE),
    eth_griev = mean(ethnic_fairtreat, na.rm = TRUE)) %>%
  filter(!(is.nan(eth_id) & is.nan(eth_griev))) %>%
  mutate(
    country = countrycode(str_sub(country_round, 1, 3), "iso3c", "country.name"),
    round = str_sub(country_round, 5, 5)) %>%
  mutate(crlab = factor(paste0(country, " (Round ", round, ")")))

# Ethnic ID
pdf("descriptives/output/ethnic_id.pdf", width = 8, height = 12)
ggplot(dv_desc, aes(x = crlab, y = eth_id)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(y = "% Ethnic over national ID", x = "") +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 10),
        strip.background = element_blank()) +
  coord_flip()
dev.off()

# Ethnic grievances
pdf("descriptives/output/ethnic_grievances.pdf", width = 8, height = 12)
ggplot(dv_desc, aes(x = crlab, y = eth_griev)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(y = "% Express ethnic grievances", x = "") +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 10),
        strip.background = element_blank()) +
  coord_flip()
dev.off()

# ------------------------------
# Maps

# Coordinates (AB + GED)
coordinates(ab) = ~longitude+latitude
proj4string(ab) = proj4string(gadm)
coordinates(ged) = ~longitude+latitude
proj4string(ged) = proj4string(gadm)
coordinates(ged_r) = ~longitude+latitude
proj4string(ged_r) = proj4string(gadm)

# Countries included, and delete islands etc
gadm$country_id = countrycode(gadm$NAME_0, "country.name", "cown")
gadm$in_dataset = ifelse(gadm$country_id %in% ab$country_id, TRUE, FALSE)
gadm$in_dataset = ifelse(gadm$country_id %in% countrycode(
  c("Morocco", "Swaziland", "Zambia"), "country.name", "cown"), FALSE, gadm$in_dataset)
gadm = gadm[!gadm$GID_0 %in% c("STP", "STP", "COM", "CPV", "MUS", "MYT", "SHN"),]

# GED (All)
pdf("descriptives/output/map_ged_all.pdf", width = 7, height = 7)
par(mar=c(0,0,0,0))
plot(gadm, col = "white", border = "gray")
plot(gadm[!gadm$in_dataset,], col = grey(0.9), border = "gray", add = TRUE)
points(rbind(ged, ged_r), pch = 20, cex = 0.5, col = alpha("red", 0.5))
dev.off()

# GED (State)
pdf("descriptives/output/map_ged_state.pdf", width = 7, height = 7)
par(mar=c(0,0,0,0))
plot(gadm, col = "white", border = "gray")
plot(gadm[!gadm$in_dataset,], col = grey(0.9), border = "gray", add = TRUE)
points(ged, pch = 20, cex = 0.5, col = alpha("red", 0.5))
dev.off()

# GED (Rebel)
pdf("descriptives/output/map_ged_rebel.pdf", width = 7, height = 7)
par(mar=c(0,0,0,0))
plot(gadm, col = "white", border = "gray")
plot(gadm[!gadm$in_dataset,], col = grey(0.9), border = "gray", add = TRUE)
points(ged_r, pch = 20, cex = 0.5, col = alpha("red", 0.5))
dev.off()

# Afrobarometer
pdf("descriptives/output/map_afrob.pdf", width = 7, height = 7)
par(mar=c(0,0,0,0))
plot(gadm, col = "white", border = "gray")
plot(gadm[!gadm$in_dataset,], col = grey(0.9), border = "gray", add = TRUE)
points(ab, pch = ".")
dev.off()

# ------------------------------
# Coding example (slides)

# Limit by coordinates
p = c(7.556681857385519, -8.466901053703749)
ycoords = c(p[1]-2, p[1]+2)
xcoords = c(p[2]-2, p[2]+2)
ab_s = ab[
  coordinates(ab)[, "longitude"] >= xcoords[1] &
  coordinates(ab)[, "longitude"] <= xcoords[2] &
  coordinates(ab)[, "latitude"] >= ycoords[1] &
  coordinates(ab)[, "latitude"] <= ycoords[2],]
# Choose exposure variable
names(ab_s)[names(ab_s) == "expo_2yr_10km_bin"] = "expo"

# Create color variabels
cb = brewer.pal(12, "Paired")
ab_s$cr = paste(ab_s$country_name, ab_s$year)
ab_s$cr_col = cb[as.factor(ab_s$cr)]
ab_s$region_col = rep(cb, 3)[as.factor(ab_s$region)]

pdf("descriptives/output/map_coding_example1.pdf", width = 8, height = 8)
plot(gadm, ylim = ycoords, xlim = xcoords)
points(ab_s, pch = 20, col = alpha("black", 0.1))
text(y = 7.6, x = -7.6, label = "COTE D'IVOIRE")
text(y = 8.15, x = -8.75, label = "GUINEA")
text(y = 6.75, x = -9.2, label = "LIBERIA")
dev.off()

pdf("descriptives/output/map_coding_example2.pdf", width = 8, height = 8)
plot(gadm, ylim = ycoords, xlim = xcoords)
points(ab_s, pch = 20, col = alpha("black", 0.1))
points(ged, pch = 8, col = "blue")
text(y = 7.6, x = -7.6, label = "COTE D'IVOIRE")
text(y = 8.15, x = -8.75, label = "GUINEA")
text(y = 6.75, x = -9.2, label = "LIBERIA")
dev.off()

pdf("descriptives/output/map_coding_example3.pdf", width = 8, height = 8)
plot(gadm, ylim = ycoords, xlim = xcoords)
points(ab_s[ab_s$expo == 1,], pch = 20, col = "red")
points(ab_s[ab_s$expo == 0,], pch = 20, col = alpha("black", 0.1))
points(ged, pch = 8, col = alpha("blue", 0.5))
text(y = 7.6, x = -7.6, label = "COTE D'IVOIRE")
text(y = 8.15, x = -8.75, label = "GUINEA")
text(y = 6.75, x = -9.2, label = "LIBERIA")
dev.off()

pdf("descriptives/output/map_coding_example4.pdf", width = 8, height = 8)
plot(gadm, ylim = ycoords, xlim = xcoords)
points(ab_s[ab_s$expo == 1 & ab_s$EOSV_state == 1,], pch = 20, col = alpha("red", 0.5))
points(ab_s[ab_s$expo == 1 & ab_s$EOSV_state == 0,], pch = 20, col = alpha("blue", 0.5))
points(ab_s[ab_s$expo == 0,], pch = 20, col = "gray")
text(y = 7.6, x = -7.6, label = "COTE D'IVOIRE")
text(y = 8.15, x = -8.75, label = "GUINEA")
text(y = 6.75, x = -9.2, label = "LIBERIA")
legend(p[2], p[1]+2,
  legend = c("Ethnic group previously targeted (EOSV)", "Ethnic group not targeted",
    "(Not exposed to violence)"),
  fill = c("red", "blue", "gray"), cex = 0.8, bg = "white")
dev.off()

pdf("descriptives/output/map_coding_example_cr.pdf", width = 8, height = 8)
plot(gadm, ylim = ycoords, xlim = xcoords)
points(ab_s, pch = 20, col = ab_s$cr_col)
text(y = 7.6, x = -7.6, label = "COTE D'IVOIRE")
text(y = 8.15, x = -8.75, label = "GUINEA")
text(y = 6.75, x = -9.2, label = "LIBERIA")
legend(p[2]+0.5, p[1]+2, legend = levels(as.factor(ab_s$cr)),
  fill = cb[1:10], bg = "white")
dev.off()

pdf("descriptives/output/map_coding_example_region.pdf", width = 8, height = 8)
plot(gadm, ylim = ycoords, xlim = xcoords)
points(ab_s, pch = 20, col = ab_s$region_col)
text(y = 7.6, x = -7.6, label = "COTE D'IVOIRE")
text(y = 8.15, x = -8.75, label = "GUINEA")
text(y = 6.75, x = -9.2, label = "LIBERIA")
# legend(p[2]+1, p[1]+2, legend = levels(as.factor(ab_s$region)),
#   fill = rep(cb, 3), cex = 0.75)
dev.off()

# ------------------------------
# EOSV map

# Prepare
eosv$GID_0 = countrycode(str_sub(eosv$EthnGrID, 1, 3), "cown", "iso3c")
eosv$continent = countrycode(str_sub(eosv$EthnGrID, 1, 3), "cown", "continent")

# Assign colors
EOSV_cs = c("white", brewer.pal(4, "Reds"))

# Base map number of groups that suffered EOSV
eosv_ctry = eosv %>%
  filter(continent == "Africa") %>%
  group_by(GID_0) %>%
  summarize(
    g_EOSV = length(unique(EthnGrID[Target_Gov == 1])),
    g_EOSV_reb = length(unique(EthnGrID[Target_Reb == 1])))
# Add to GADM
gadm@data = left_join(gadm@data, eosv_ctry) %>%
  mutate(g_EOSV = ifelse(is.na(g_EOSV), 0, g_EOSV)) %>%
  mutate(g_EOSV_reb = ifelse(is.na(g_EOSV_reb), 0, g_EOSV_reb)) %>%
  # cats: 0, 1, 2, 3-5, >5
  mutate(g_EOSV_c = ifelse(g_EOSV > 0, EOSV_cs[2], "white")) %>%
  mutate(g_EOSV_c = ifelse(g_EOSV > 1, EOSV_cs[3], g_EOSV_c)) %>%
  mutate(g_EOSV_c = ifelse(g_EOSV > 2, EOSV_cs[4], g_EOSV_c)) %>%
  mutate(g_EOSV_c = ifelse(g_EOSV > 5, EOSV_cs[5], g_EOSV_c))

# Map
pdf("descriptives/output/map_EOSV_state.pdf", width = 7, height = 7)
par(mar=c(0,0,0,0))
plot(gadm, col = gadm$g_EOSV_c, border = "gray")
plot(gadm[!gadm$in_dataset,], col = grey(0.9), border = "gray", add = TRUE)
legend(-15, -1,
  title = "Previous state-led EOSV",
  legend = c("0 groups", "1", "2", "3-5", ">5", "Not in AfroBaro"),
  fill = c(EOSV_cs, "gray"))
dev.off()
