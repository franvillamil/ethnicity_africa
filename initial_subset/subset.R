# setwd("~/Documents/Projects/ethnic_violence_africa")
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

# Get AB (NOTE: previously subsetted to avoid permission issues)
ab = read.csv("input_data/ab.csv")

# Same for UCDP GED
ged = read.csv("input_data/ged201.csv") %>%
  filter(country != "Botswana") %>%
  filter(type_of_violence == 3 &
  country_id %in% ab$country_id &
  year < max(ab$year))

# Subset again AB
ab = subset(ab, country_id %in% ged$country_id)

# Split into govt / rebel violence
ged_reb = subset(ged, !grepl("Government of", side_a))
ged = subset(ged, grepl("Government of", side_a))

# SAVE
write.csv(ged, "initial_subset/output/ged_state.csv", row.names = FALSE)
write.csv(ged_reb, "initial_subset/output/ged_rebel.csv", row.names = FALSE)
write.csv(ab, "initial_subset/output/ab.csv", row.names = FALSE)
