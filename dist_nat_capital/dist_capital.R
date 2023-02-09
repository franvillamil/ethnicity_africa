# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("countrycode", "dplyr", "sf", "stringr", "CoordinateCleaner")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------


# Get AB data
ab = read.csv("initial_subset/output/ab.csv")

# Limit to just ID variable and coordinates
ab = ab[, c("respno", "country_iso3c", "round", "latitude", "longitude")]
ab_crd = unique(ab[, c("longitude", "latitude", "country_iso3c")])

# Capitals and coordinates, limit to those in AB data
data(countryref)
caps = countryref %>%
  filter(iso3 %in% ab$country_iso3c, type == "country") %>%
  select(iso3, name, capital.lon, capital.lat) %>%
  rename(latitude = capital.lat, longitude = capital.lon) %>%
  unique

## Go spatial

# Turn into spatial object (AB)
ab_crd_sp = st_as_sf(x = ab_crd, coords = c("longitude", "latitude"), crs = 4326)

# Turn into spatial object (capitals)
caps_sp = st_as_sf(x = caps, coords = c("longitude", "latitude"), crs = 4326)

# Distance matrix (could be more efficient)
ab_crd$dist_capital = NA
for(i in 1:nrow(ab_crd)){
  if(i %% 500 == 0){print(i)}
  c = which(caps$iso3 == ab_crd$country_iso3c[i])
  ab_crd$dist_capital[i] = st_distance(st_geometry(caps_sp)[c], ab_crd_sp[i,])
}

# Select
ab = ab %>%
  left_join(ab_crd) %>%
  mutate(dist_capital_km = dist_capital / 1000) %>%
  select(respno, round, dist_capital_km)

# Save
saveRDS(ab, "dist_nat_capital/output/dist_capital.rds")
