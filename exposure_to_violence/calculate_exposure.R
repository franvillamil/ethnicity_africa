# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

# NOTE: Doing it first for state events and then for rebel events
# because of memory issues

# Initial files
ged = read.csv("initial_subset/output/ged_state.csv")
ged_reb = read.csv("initial_subset/output/ged_rebel.csv")
ab = read.csv("initial_subset/output/ab.csv")

# Limit AB variables
ab = ab[, c("round", "respno", "country_name", "country_id", "year", "age")]

# Mark upper and lower year limit for events
lwr_age = 15
upr_age = 25
ab$lwr_1525 = (ab$year-ab$age) + lwr_age
ab$upr_1525 = (ab$year-ab$age) + upr_age
ab$lwr_1525[ab$upr_1525 < 1989] = NA
ab$upr_1525[ab$upr_1525 < 1989] = NA

# Mark upper and lower year limit for events
ab$lwr_1yr = ab$year - 1
ab$lwr_2yr = ab$year - 2
ab$lwr_3yr = ab$year - 3
ab$lwr_4yr = ab$year - 4
ab$lwr_5yr = ab$year - 5
ab$lwr_6yr = ab$year - 6
ab$lwr_7yr = ab$year - 7
ab$lwr_8yr = ab$year - 8
ab$lwr_9yr = ab$year - 9
ab$lwr_10yr = ab$year - 10


## STATE OSV ##

# Get distance matrix
dist_mat = readRDS("distance_matrix/output/dist_mat_state.rds")

# Remove all events taking place same year or after each survey
dist_mat = dist_mat * NA^(ged$year[col(dist_mat)] >= ab$year[row(dist_mat)])

# Remove all events that took place in a different country
dist_mat = dist_mat * NA^(ged$country_id[col(dist_mat)] != ab$country_id[row(dist_mat)])

## Exposure variables

# Within 5km, 10km, 25km, 50km
ab$expo_5km = rowSums(dist_mat < 5000, na.rm = TRUE)
ab$expo_10km = rowSums(dist_mat < 10000, na.rm = TRUE)
ab$expo_25km = rowSums(dist_mat < 25000, na.rm = TRUE)
ab$expo_50km = rowSums(dist_mat < 50000, na.rm = TRUE)

# Events that took place when respondent was 15-25yo
# Exclude events in distance matrix, later than upr OR earlier than lwr
dist_mat1525 = dist_mat * NA^(
  ged$year[col(dist_mat)] < ab$lwr_1525[row(dist_mat)] |
  ged$year[col(dist_mat)] > ab$upr_1525[row(dist_mat)])
# Use same distance thresholds
ab$expo_1525_5km = rowSums(dist_mat1525 < 5000, na.rm = TRUE)
ab$expo_1525_10km = rowSums(dist_mat1525 < 10000, na.rm = TRUE)
ab$expo_1525_25km = rowSums(dist_mat1525 < 25000, na.rm = TRUE)
ab$expo_1525_50km = rowSums(dist_mat1525 < 50000, na.rm = TRUE)
ab$expo_1525_5km[is.na(ab$age)] = NA
ab$expo_1525_10km[is.na(ab$age)] = NA
ab$expo_1525_25km[is.na(ab$age)] = NA
ab$expo_1525_50km[is.na(ab$age)] = NA
# Clear up space
rm(dist_mat1525)

# Events during the last 2/5 years before survey

# EXCLUDE events in distance matrix, earlier than 2/5 years before
dist_mat2yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_2yr[row(dist_mat)])
dist_mat5yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_5yr[row(dist_mat)])
# Or EXCLUDE within last 2/5 years
dist_mat_bf2yr = dist_mat * NA^(ged$year[col(dist_mat)] >= ab$lwr_2yr[row(dist_mat)])
dist_mat_bf5yr = dist_mat * NA^(ged$year[col(dist_mat)] >= ab$lwr_5yr[row(dist_mat)])
# Use same distance thresholds
ab$expo_2yr_5km = rowSums(dist_mat2yr < 5000, na.rm = TRUE)
ab$expo_2yr_10km = rowSums(dist_mat2yr < 10000, na.rm = TRUE)
ab$expo_2yr_25km = rowSums(dist_mat2yr < 25000, na.rm = TRUE)
ab$expo_2yr_50km = rowSums(dist_mat2yr < 50000, na.rm = TRUE)
ab$expo_5yr_5km = rowSums(dist_mat5yr < 5000, na.rm = TRUE)
ab$expo_5yr_10km = rowSums(dist_mat5yr < 10000, na.rm = TRUE)
ab$expo_5yr_25km = rowSums(dist_mat5yr < 25000, na.rm = TRUE)
ab$expo_5yr_50km = rowSums(dist_mat5yr < 50000, na.rm = TRUE)
ab$expo_bf2yr_5km = rowSums(dist_mat_bf2yr < 5000, na.rm = TRUE)
ab$expo_bf2yr_10km = rowSums(dist_mat_bf2yr < 10000, na.rm = TRUE)
ab$expo_bf2yr_25km = rowSums(dist_mat_bf2yr < 25000, na.rm = TRUE)
ab$expo_bf2yr_50km = rowSums(dist_mat_bf2yr < 50000, na.rm = TRUE)
ab$expo_bf5yr_5km = rowSums(dist_mat_bf5yr < 5000, na.rm = TRUE)
ab$expo_bf5yr_10km = rowSums(dist_mat_bf5yr < 10000, na.rm = TRUE)
ab$expo_bf5yr_25km = rowSums(dist_mat_bf5yr < 25000, na.rm = TRUE)
ab$expo_bf5yr_50km = rowSums(dist_mat_bf5yr < 50000, na.rm = TRUE)
# Clear up space
rm(dist_mat_bf2yr, dist_mat_bf5yr, dist_mat2yr, dist_mat5yr)

## ONLY FOR STATE VIOLENCE - BANDWIDTHS
# EXCLUDE events in distance matrix, earlier than 1-10 years before
dist_mat1yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_1yr[row(dist_mat)])
ab$expo_1yr_10km = rowSums(dist_mat1yr < 10000, na.rm = TRUE)
rm(dist_mat1yr)
dist_mat3yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_3yr[row(dist_mat)])
ab$expo_3yr_10km = rowSums(dist_mat3yr < 10000, na.rm = TRUE)
rm(dist_mat3yr)
dist_mat4yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_4yr[row(dist_mat)])
ab$expo_4yr_10km = rowSums(dist_mat4yr < 10000, na.rm = TRUE)
rm(dist_mat4yr)
dist_mat6yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_6yr[row(dist_mat)])
ab$expo_6yr_10km = rowSums(dist_mat6yr < 10000, na.rm = TRUE)
rm(dist_mat6yr)
dist_mat7yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_7yr[row(dist_mat)])
ab$expo_7yr_10km = rowSums(dist_mat7yr < 10000, na.rm = TRUE)
rm(dist_mat7yr)
dist_mat8yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_8yr[row(dist_mat)])
ab$expo_8yr_10km = rowSums(dist_mat8yr < 10000, na.rm = TRUE)
rm(dist_mat8yr)
dist_mat9yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_9yr[row(dist_mat)])
ab$expo_9yr_10km = rowSums(dist_mat9yr < 10000, na.rm = TRUE)
rm(dist_mat9yr)
dist_mat10yr = dist_mat * NA^(ged$year[col(dist_mat)] < ab$lwr_10yr[row(dist_mat)])
ab$expo_10yr_10km = rowSums(dist_mat10yr < 10000, na.rm = TRUE)
rm(dist_mat10yr)

# Clear up
rm(dist_mat)

## REBEL OSV ##

# Get distance matrix
dist_mat_reb = readRDS("distance_matrix/output/dist_mat_rebel.rds")

# Remove all events taking place before the survey of each respondent
dist_mat_reb = dist_mat_reb * NA^(ged_reb$year[col(dist_mat_reb)] >= ab$year[row(dist_mat_reb)])

# Remove all events that took place in a different country
dist_mat_reb = dist_mat_reb *
  NA^(ged_reb$country_id[col(dist_mat_reb)] != ab$country_id[row(dist_mat_reb)])

## Exposure variables (rebel OSV)

# Within 10km, 25km, 50km
ab$expo_reb_5km = rowSums(dist_mat_reb < 5000, na.rm = TRUE)
ab$expo_reb_10km = rowSums(dist_mat_reb < 10000, na.rm = TRUE)
ab$expo_reb_25km = rowSums(dist_mat_reb < 25000, na.rm = TRUE)
ab$expo_reb_50km = rowSums(dist_mat_reb < 50000, na.rm = TRUE)

# Events that took place when respondent was 15-30yo
# Limit events in distance matrix
dist_mat_reb1525 = dist_mat_reb * NA^(
  ged$year[col(dist_mat_reb)] < ab$lwr_1525[row(dist_mat_reb)] |
    ged$year[col(dist_mat_reb)] > ab$upr_1525[row(dist_mat_reb)])
# Use same distance thresholds
ab$expo_reb_1525_5km = rowSums(dist_mat_reb1525 < 5000, na.rm = TRUE)
ab$expo_reb_1525_10km = rowSums(dist_mat_reb1525 < 10000, na.rm = TRUE)
ab$expo_reb_1525_25km = rowSums(dist_mat_reb1525 < 25000, na.rm = TRUE)
ab$expo_reb_1525_50km = rowSums(dist_mat_reb1525 < 50000, na.rm = TRUE)
ab$expo_reb_1525_5km[is.na(ab$age)] = NA
ab$expo_reb_1525_10km[is.na(ab$age)] = NA
ab$expo_reb_1525_25km[is.na(ab$age)] = NA
ab$expo_reb_1525_50km[is.na(ab$age)] = NA
# Clear up space
rm(dist_mat_reb1525)

# Exclude events in distance matrix, earlier than 2/5 years before
dist_mat_reb2yr = dist_mat_reb * NA^(ged$year[col(dist_mat_reb)] < ab$lwr_2yr[row(dist_mat_reb)])
dist_mat_reb5yr = dist_mat_reb * NA^(ged$year[col(dist_mat_reb)] < ab$lwr_5yr[row(dist_mat_reb)])
# Use same distance thresholds
ab$expo_reb_2yr_5km = rowSums(dist_mat_reb2yr < 5000, na.rm = TRUE)
ab$expo_reb_2yr_10km = rowSums(dist_mat_reb2yr < 10000, na.rm = TRUE)
ab$expo_reb_2yr_25km = rowSums(dist_mat_reb2yr < 25000, na.rm = TRUE)
ab$expo_reb_2yr_50km = rowSums(dist_mat_reb2yr < 50000, na.rm = TRUE)
ab$expo_reb_5yr_5km = rowSums(dist_mat_reb5yr < 5000, na.rm = TRUE)
ab$expo_reb_5yr_10km = rowSums(dist_mat_reb5yr < 10000, na.rm = TRUE)
ab$expo_reb_5yr_25km = rowSums(dist_mat_reb5yr < 25000, na.rm = TRUE)
ab$expo_reb_5yr_50km = rowSums(dist_mat_reb5yr < 50000, na.rm = TRUE)
# Make up space
rm(dist_mat_reb2yr, dist_mat_reb5yr)

# Exclude events within last 2/5 years
dist_mat_reb_bf2yr = dist_mat_reb *
  NA^(ged$year[col(dist_mat_reb)] >= ab$lwr_5yr[row(dist_mat_reb)])
dist_mat_reb_bf5yr = dist_mat_reb *
  NA^(ged$year[col(dist_mat_reb)] >= ab$lwr_5yr[row(dist_mat_reb)])
# Use same distance thresholds
ab$expo_reb_bf2yr_5km = rowSums(dist_mat_reb_bf2yr < 5000, na.rm = TRUE)
ab$expo_reb_bf2yr_10km = rowSums(dist_mat_reb_bf2yr < 10000, na.rm = TRUE)
ab$expo_reb_bf2yr_25km = rowSums(dist_mat_reb_bf2yr < 25000, na.rm = TRUE)
ab$expo_reb_bf2yr_50km = rowSums(dist_mat_reb_bf2yr < 50000, na.rm = TRUE)
ab$expo_reb_bf5yr_5km = rowSums(dist_mat_reb_bf5yr < 5000, na.rm = TRUE)
ab$expo_reb_bf5yr_10km = rowSums(dist_mat_reb_bf5yr < 10000, na.rm = TRUE)
ab$expo_reb_bf5yr_25km = rowSums(dist_mat_reb_bf5yr < 25000, na.rm = TRUE)
ab$expo_reb_bf5yr_50km = rowSums(dist_mat_reb_bf5yr < 50000, na.rm = TRUE)
# Clear up space
rm(dist_mat_reb, dist_mat_reb_bf2yr, dist_mat_reb_bf5yr)

## SAVE
ab = ab[, -which(names(ab) %in% c("age", "lwr_1525", "upr_1525", "lwr_2yr", "lwr_5yr"))]
write.csv(ab, "exposure_to_violence/output/ab_violence_exposure.csv", row.names = FALSE)
