# setwd("~/Desktop/eosv")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "sf")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------


# Get initial datasets
ab = read.csv("initial_subset/output/ab.csv")
ged_state = read.csv("initial_subset/output/ged_state.csv")
ged_reb = read.csv("initial_subset/output/ged_rebel.csv")

# Turn into spatial objects
ab_sp = st_as_sf(x = ab, coords = c("longitude", "latitude"), crs = 4326)
ged_state_sp = st_as_sf(ged_state, coords = c("longitude", "latitude"), crs = 4326)
ged_reb_sp = st_as_sf(ged_reb, coords = c("longitude", "latitude"), crs = 4326)

# Distances
dist_mat_state = st_distance(ged_state_sp, ab_sp)
dist_mat_reb = st_distance(ged_reb_sp, ab_sp)

# Transpose (one row for each AB respondent)
dist_mat_state = t(dist_mat_state)
dist_mat_reb = t(dist_mat_reb)

# Save
saveRDS(dist_mat_state, "distance_matrix/output/dist_mat_state.rds")
saveRDS(dist_mat_reb, "distance_matrix/output/dist_mat_rebel.rds")
