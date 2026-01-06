

# Soundscape Baselines PCA of soundscape similarity
# one row per SITE
# created 3 April 2025
# last updated 5 January 2026

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(data.table)
library(vroom)
library(bigmemory)
library(lubridate)
library(missMDA)
library(suncalc)
library(dplyr)
library(purrr)

# set wd -----------------------------------------------------------------------
setwd("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Soundscape Baselines/Soundscape_Baselines_GIT")

# read in data (vroom MUCH faster than read.csv) -------------------------------

indices_10min <- vroom("data/zenodo/PCAinput_allIndices_allCountries_10minAvg.csv", col_select = -1)

# choose country subset --------------------------------------------------------(options) 
# run the version of this line with the country set you want

# ALL countries
indices_subset <- indices_10min[indices_10min$Country %in% c("USA", "Germany", "Ecuador", "Peru", "Gabon", "Sierra Leone", "Brunei", "Singapore"), ]
# TROPICAL countries
indices_subset <- indices_10min[indices_10min$Country %in% c("Ecuador", "Peru", "Gabon", "Sierra Leone", "Brunei", "Singapore"), ]
# TEMPERATE countries
indices_subset <- indices_10min[indices_10min$Country %in% c("USA", "Germany"), ]
# All, except Singapore
indices_subset <- indices_10min[indices_10min$Country %in% c("USA", "Germany", "Ecuador", "Peru", "Gabon", "Sierra Leone", "Brunei"), ]
# TROPICAL countries, except SG
indices_subset <- indices_10min[indices_10min$Country %in% c("Ecuador", "Peru", "Gabon", "Sierra Leone", "Brunei"), ]



# remove sites with too much missing data ---------------------------------------

indices_subset <- subset(indices_subset, indices_subset$Site != "GRNP_811")

# pivot wider ------------------------------------------------------------------

indices_wide <- indices_subset %>%
  pivot_wider(names_from = c(frequency, Time_10min), values_from = c(ACI, ENT, EVN, PMN))

# run PCA ----------------------------------------------------------------------

# Select only numerical columns for PCA
pca_data <- indices_wide %>%
  dplyr::select(-Site, -Country)

# Remove columns with NAs
pca_data_clean <- pca_data[ , colSums(is.na(pca_data)) == 0]

# Run PCA
pca_result <- prcomp(pca_data_clean, center=TRUE, scale=TRUE)

# Manually define color palette
country_colors <- c(
  "USA" = "#E6194B", 
  "Germany" = "#F58231",
  "Ecuador" = "#4363D8", 
  "Peru" = "#46F0F0",
  "Gabon" = "#911EB4" ,
  "Sierra Leone" = "#F032E6",
  "Brunei" = "#3CB44B",
  "Singapore" = "#BCF60C"
)


# Define shape mapping 
country_shapes <- c(
  "USA" = 15,           # Square
  "Germany" = 16,       # Circle
  "Ecuador" = 17,       # Triangle
  "Peru" = 18,          # Diamond
  "Gabon" = 8,          # Star
  "Sierra Leone" = 3,   # Plus
  "Brunei" = 4,         # Cross
  "Singapore" = 7       # Square cross
)


# Visualize the PCA
fviz_pca_ind(pca_result,
             label= "none",
             habillage = indices_wide$Country,
             addEllipses=TRUE, ellipse.level=0.85,
             title = "",
             palette = country_colors) + 
  scale_shape_manual(values = country_shapes)



################################################################################
# Diurnal / Nocturnal PCA
################################################################################


# split nocturnal/diurnal times ------------------------------------------------

indices_10min_diurnal <- subset(indices_subset, indices_subset$Time_10min >= hms("07:00:00") & indices_subset$Time_10min <= hms("18:00:00"))
indices_10min_nocturnal <- subset(indices_subset, indices_subset$Time_10min >= hms("21:00:00") | indices_subset$Time_10min <= hms("03:00:00"))


# pivot wider ------------------------------------------------------------------

indices_wide_diurnal <- indices_10min_diurnal %>%
  pivot_wider(names_from = c(frequency, Time_10min), values_from = c(ACI, ENT, EVN, PMN))

indices_wide_nocturnal <- indices_10min_nocturnal %>%
  pivot_wider(names_from = c(frequency, Time_10min), values_from = c(ACI, ENT, EVN, PMN))


# run diurnal PCA --------------------------------------------------------------

# Select only numerical columns for PCA
pca_data_diurnal <- indices_wide_diurnal %>%
  dplyr::select(-Site, -Country)

# Remove columns with NAs
pca_data_diurnal_clean <- pca_data_diurnal[ , colSums(is.na(pca_data_diurnal)) == 0]

# Run PCA
pca_diurnal_result <- prcomp(pca_data_diurnal_clean, center=TRUE, scale=TRUE)


# plot diurnal PCA -------------------------------------------------------------

fviz_pca_ind(pca_diurnal_result,
             label= "none",
             habillage = indices_wide_diurnal$Country,
             addEllipses=TRUE, ellipse.level=0.85,
             title = "",
             palette = country_colors) + 
  scale_shape_manual(values = country_shapes)


# run nocturnal PCA --------------------------------------------------------------

# Select only numerical columns for PCA
pca_data_nocturnal <- indices_wide_nocturnal %>%
  dplyr::select(-Site, -Country)

# Remove columns with NAs
pca_data_nocturnal_clean <- pca_data_nocturnal[ , colSums(is.na(pca_data_nocturnal)) == 0]

# Run PCA
pca_nocturnal_result <- prcomp(pca_data_nocturnal_clean, center=TRUE, scale=TRUE)


# plot nocturnal PCA -------------------------------------------------------------

fviz_pca_ind(pca_nocturnal_result,
             label= "none",
             habillage = indices_wide_nocturnal$Country,
             addEllipses=TRUE, ellipse.level=0.85,
             title = "",
             palette = country_colors) + 
  scale_shape_manual(values = country_shapes)



################################################################################
# Plot dawn and dusk (sensitivity analysis)
################################################################################

# load site details ------------------------------------------------------------
siteDetails <- vroom("data/zenodo/Table1_site_details.csv")

# convert date format
siteDetails$StartDate <- dmy(siteDetails$StartDate)
siteDetails$EndDate <- dmy(siteDetails$EndDate)
siteDetails$halfwayDate <- siteDetails$StartDate + as.numeric(difftime(siteDetails$EndDate, siteDetails$StartDate, units = "days")) / 2

# add sunrise/sunset
siteDetails <- siteDetails %>%
  rowwise() %>%
  mutate(sunrise_utc = getSunlightTimes(date = halfwayDate, lat = Lat, lon = Lon, keep = "sunrise")$sunrise,
         sunrise_local = map2(sunrise_utc, timeZone, ~ with_tz(.x, tzone = .y)),
         sunrise_time = format(as.POSIXct(unlist(sunrise_local)), "%H:%M:%S"))

siteDetails <- siteDetails %>%
  rowwise() %>%
  mutate(sunset_utc = getSunlightTimes(date = halfwayDate, lat = Lat, lon = Lon, keep = "sunset")$sunset,
         sunset_local = map2(sunset_utc, timeZone, ~ with_tz(.x, tzone = .y)),
         sunset_time = format(as.POSIXct(unlist(sunset_local)), "%H:%M:%S"))

# match names
names(siteDetails)[names(siteDetails) == "ID"] <- "Site"
siteDetails <- siteDetails[c("Site", "sunrise_time", "sunset_time")]

# select sites -----------------------------------------------------------------(options)

# ALL countries
indices_subset <- indices_10min[indices_10min$Country %in% c("USA", "Germany", "Ecuador", "Peru", "Gabon", "Sierra Leone", "Brunei", "Singapore"), ]
# TROPICAL countries
indices_subset <- indices_10min[indices_10min$Country %in% c("Ecuador", "Peru", "Gabon", "Sierra Leone", "Brunei", "Singapore"), ]
# TEMPERATE countries
indices_subset <- indices_10min[indices_10min$Country %in% c("USA", "Germany"), ]


# merge with indices -----------------------------------------------------------
indices_10min_sunrise <- indices_subset %>%
  left_join(siteDetails, by = "Site") %>%
  mutate(time_since_sunrise = lubridate::hms(Time_10min) - lubridate::hms(sunrise_time),
         time_since_sunrise_rounded = hms::hms(round(time_length(time_since_sunrise, unit = "seconds") / 600) * 600))

indices_10min_sunset <- indices_subset %>%
  left_join(siteDetails, by = "Site") %>%
  mutate(time_since_sunset = lubridate::hms(Time_10min) - lubridate::hms(sunset_time),
         time_since_sunset_rounded = hms::hms(round(time_length(time_since_sunset, unit = "seconds") / 600) * 600))


# filter times close to sunrise/sunset
indices_10min_sunrise <- indices_10min_sunrise %>%
  filter(time_since_sunrise_rounded >= lubridate::hms("-02:00:00") & time_since_sunrise_rounded <= lubridate::hms("02:00:00"))

indices_10min_sunset <- indices_10min_sunset %>%
  filter(time_since_sunset_rounded >= lubridate::hms("-02:00:00") & time_since_sunset_rounded <= lubridate::hms("02:00:00"))

# remove sites with too much missing data 
indices_10min_sunrise <- subset(indices_10min_sunrise, indices_10min_sunrise$Site != "GRNP_811")
indices_10min_sunset <- subset(indices_10min_sunset, indices_10min_sunset$Site != "GRNP_811")

# remove extra columns
indices_10min_sunrise$time_since_sunrise <- NULL
indices_10min_sunrise$Time_10min <- NULL
indices_10min_sunrise$sunrise_time <- NULL 

indices_10min_sunset$time_since_sunset <- NULL
indices_10min_sunset$Time_10min <- NULL
indices_10min_sunset$sunset_time <- NULL 

# pivot wider 
indices_10min_sunrise_wide <- indices_10min_sunrise %>%
  pivot_wider(names_from = c(frequency, time_since_sunrise_rounded), values_from = c(ACI, ENT, EVN, PMN))

indices_10min_sunset_wide <- indices_10min_sunset %>%
  pivot_wider(names_from = c(frequency, time_since_sunset_rounded), values_from = c(ACI, ENT, EVN, PMN))

# run PCA sunrise --------------------------------------------------------------
# Select only numerical columns for PCA
pca_data <- indices_10min_sunrise_wide %>%
  dplyr::select(-Site, -Country, -sunset_time)

# Remove columns with NAs
pca_data_clean <- pca_data[ , colSums(is.na(pca_data)) == 0]

# Run PCA
pca_result <- prcomp(pca_data_clean, center=TRUE, scale=TRUE)

# Visualize the PCA
fviz_pca_ind(pca_result,
             label= "none",
             habillage = indices_10min_sunrise_wide$Country,
             addEllipses=TRUE, ellipse.level=0.85,
             title = "",
             palette = country_colors) + 
  scale_shape_manual(values = country_shapes)


# run PCA sunset ---------------------------------------------------------------
# Select only numerical columns for PCA
pca_data <- indices_10min_sunset_wide %>%
  dplyr::select(-Site, -Country, -sunrise_time)

# Remove columns with NAs
pca_data_clean <- pca_data[ , colSums(is.na(pca_data)) == 0]

# Run PCA
pca_result <- prcomp(pca_data_clean, center=TRUE, scale=TRUE)

# Visualize the PCA
fviz_pca_ind(pca_result,
             label= "none",
             habillage = indices_10min_sunset_wide$Country,
             addEllipses=TRUE, ellipse.level=0.85,
             title = "",
             palette = country_colors) + 
  scale_shape_manual(values = country_shapes)






