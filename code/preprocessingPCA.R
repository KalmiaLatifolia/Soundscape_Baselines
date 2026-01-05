
# data preprocessing
# for PCA
# last edited 23 Dec 2025

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
setwd("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Soundscape Baselines/SoundscapeBaselines_GIT")


################################################################################
# Preprocessing. Already completed
################################################################################

# These steps are done only once. Skip for now.

# load data --------------------------------------------------------------------
# ENT <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Singapore/ENT_Singapore_20200623_20200708.csv")
# ACI <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Singapore/ACI_Singapore_20200623_20200708.csv")
# EVN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Singapore/EVN_Singapore_20200623_20200708.csv")
# PMN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Singapore/PMN_Singapore_20200623_20200708.csv")

# ENT <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Baraboo/ENT_Baraboo_20240506_20240601.csv")
# ACI <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Baraboo/ACI_Baraboo_20240506_20240601.csv")
# EVN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Baraboo/EVN_Baraboo_20240506_20240601.csv")
# PMN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Baraboo/PMN_Baraboo_20240506_20240601.csv")

# ENT <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Ecuador/ENT_Ecuador_20231028_20231128.csv")
# ACI <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Ecuador/ACI_Ecuador_20231028_20231128.csv")
# EVN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Ecuador/EVN_Ecuador_20231028_20231128.csv")
# PMN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Ecuador/PMN_Ecuador_20231028_20231128.csv")

# ENT <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Gabon/ENT_Gabon_20221015_20221111.csv")
# ACI <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Gabon/ACI_Gabon_20221015_20221111.csv")
# EVN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Gabon/EVN_Gabon_20221015_20221111.csv")
# PMN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Gabon/PMN_Gabon_20221015_20221111.csv")

# ENT <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Germany/ENT_Germany_20240501_20240530.csv")
# ACI <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Germany/ACI_Germany_20240501_20240530.csv")
# EVN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Germany/EVN_Germany_20240501_20240530.csv")
# PMN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Germany/PMN_Germany_20240501_20240530.csv")

# ENT <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Liberia_SierraLeone/ENT_Liberia_20211212_20211225.csv")
# ACI <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Liberia_SierraLeone/ACI_Liberia_20211212_20211225.csv")
# EVN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Liberia_SierraLeone/EVN_Liberia_20211212_20211225.csv")
# PMN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Liberia_SierraLeone/PMN_Liberia_20211212_20211225.csv")

# ENT <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Peru/ENT_Peru_20230518_20230617.csv")
# ACI <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Peru/ACI_Peru_20230518_20230617.csv")
# EVN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Peru/EVN_Peru_20230518_20230617.csv")
# PMN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Peru/PMN_Peru_20230518_20230617.csv")

# ENT <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Brunei/ENT_Brunei20240101_20240201.csv")
# ACI <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Brunei/ACI_Brunei20240101_20240201.csv")
# EVN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Brunei/EVN_Brunei20240101_20240201.csv")
# PMN <- read.csv("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Acoustic indices/2_tidy_indices/Brunei/PMN_Brunei20240101_20240201.csv")


# clean data -------------------------------------------------------------------

# List of dataframes
df_list <- list(ENT=ENT, ACI=ACI, EVN=EVN, PMN=PMN)

# Column names to remove
cols_to_remove <- c("X", "X.1", "Source", "Coordinates")

# Apply column removal to each dataframe in the list
df_list <- lapply(df_list, function(df) df[, !names(df) %in% cols_to_remove, drop = FALSE])

# Explicitly assign back to original names
ACI <- df_list$ACI
ENT <- df_list$ENT
EVN <- df_list$EVN
PMN <- df_list$PMN

rm(df_list)

# pivot longer -----------------------------------------------------------------

ENT_long <- ENT %>%
  pivot_longer(cols = starts_with("c000"),
               names_to = "frequency",
               names_prefix = "c",
               values_to = "ENT")
ENT_long$frequency <- as.numeric(ENT_long$frequency)*43.1/1000

ACI_long <- ACI %>%
  pivot_longer(cols = starts_with("c000"),
               names_to = "frequency",
               names_prefix = "c",
               values_to = "ACI")
ACI_long$frequency <- as.numeric(ACI_long$frequency)*43.1/1000

EVN_long <- EVN %>%
  pivot_longer(cols = starts_with("c000"),
               names_to = "frequency",
               names_prefix = "c",
               values_to = "EVN")
EVN_long$frequency <- as.numeric(EVN_long$frequency)*43.1/1000

PMN_long <- PMN %>%
  pivot_longer(cols = starts_with("c000"),
               names_to = "frequency",
               names_prefix = "c",
               values_to = "PMN")
PMN_long$frequency <- as.numeric(PMN_long$frequency)*43.1/1000


# fix time ---------------------------------------------------------------------


ACI_long$Time <- as.POSIXct(substr(ACI_long$Datetime, 12, 19), format= "%H:%M:%S")
ACI_long$Time[is.na(ACI_long$Time)] <- as.POSIXct("00:00:00", format= "%H:%M:%S")
# ACI_long$Time <- ACI_long$Time + ACI_long$Index*60 # only if needed (check first)
ACI_long$Time <- format(round_date(ACI_long$Time, unit = "minute"), "%H:%M:%S")

ENT_long$Time <- as.POSIXct(substr(ENT_long$Datetime, 12, 19), format= "%H:%M:%S")
ENT_long$Time[is.na(ENT_long$Time)] <- as.POSIXct("00:00:00", format= "%H:%M:%S")
# ENT_long$Time <- ENT_long$Time + ENT_long$Index*60 # only if needed (check first)
ENT_long$Time <- format(round_date(ENT_long$Time, unit = "minute"), "%H:%M:%S")

EVN_long$Time <- as.POSIXct(substr(EVN_long$Datetime, 12, 19), format= "%H:%M:%S")
EVN_long$Time[is.na(EVN_long$Time)] <- as.POSIXct("00:00:00", format= "%H:%M:%S")
# EVN_long$Time <- EVN_long$Time + EVN_long$Index*60 # only if needed (check first)
EVN_long$Time <- format(round_date(EVN_long$Time, unit = "minute"), "%H:%M:%S")

PMN_long$Time <- as.POSIXct(substr(PMN_long$Datetime, 12, 19), format= "%H:%M:%S")
PMN_long$Time[is.na(PMN_long$Time)] <- as.POSIXct("00:00:00", format= "%H:%M:%S")
# PMN_long$Time <- PMN_long$Time + PMN_long$Index*60 # only if needed (check first)
PMN_long$Time <- format(round_date(PMN_long$Time, unit = "minute"), "%H:%M:%S")


# merge indices ----------------------------------------------------------------

indices_long <- merge(ACI_long, ENT_long)
indices_long <- merge(indices_long, EVN_long)
indices_long <- merge(indices_long, PMN_long)

# summarize by site ------------------------------------------------------------

indices_SiteAvg <- indices_long %>% 
  group_by(Time, frequency, Site) %>% 
  dplyr::summarise(
    ENT = mean(ENT, na.rm = TRUE),
    ACI = mean(ACI, na.rm = TRUE),
    EVN = mean(EVN, na.rm = TRUE),
    PMN = mean(PMN, na.rm = TRUE)
  )



# clip off edge frequencies ----------------------------------------------------

indices_SiteAvg <- subset(indices_SiteAvg, indices_SiteAvg$frequency < 10.7 & indices_SiteAvg$frequency > 0.2)

# add country name -------------------------------------------------------------

indices_SiteAvg$Country <- "Singapore"

# 10-minute averages -----------------------------------------------------------

indices_SiteAvg$Time_10min <- floor_date(as.POSIXct(indices_SiteAvg$Time, format = "%H:%M:%S", tz = "UTC"), unit = "10 minutes")
indices_SiteAvg$Time_10min <- format(indices_SiteAvg$Time_10min, "%H:%M:%S")

indices_10min <- indices_SiteAvg %>%
  group_by(Site, frequency, Time_10min) %>%
  summarize(
    ENT = mean(ENT, na.rm = TRUE),
    ACI = mean(ACI, na.rm = TRUE),
    EVN = mean(EVN, na.rm = TRUE),
    PMN = mean(PMN, na.rm = TRUE),
    .groups = "drop"
  )

indices_10min$Country <- "Singapore"

# save it ----------------------------------------------------------------------

write_csv(indices_SiteAvg, "AllIndices_SiteAvg_ForPCA_Singapore.csv")
write_csv(indices_10min, "AllIndices_SiteAvg_10min_ForPCA_Singapore.csv")


