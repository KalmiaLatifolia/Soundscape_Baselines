
# RGB panel plot
# for soundscape baselines manuscript

library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

# set wd
setwd("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Soundscape Baselines/SoundscapeBaselines_GIT")


# load avgDay files
ACI_Baraboo_avgDay <- readRDS("data/ACI_Baraboo_avgDay_20240506_20240601.rds")
ACI_Germany_avgDay <- read_rds("data/ACI_Germany_avgDay_20240501_20240530.rds")
ACI_Ecuador_avgDay <- read_rds("data/ACI_Ecuador_avgDay_20231028_20231128.rds")
ACI_Peru_avgDay <- read_rds("data/ACI_Peru_avgDay_20230518_20230617.rds")
ACI_Gabon_avgDay <- read_rds("data/ACI_Gabon_avgDay_20221015_20221111.rds")
ACI_Liberia_avgDay <- read_rds("data/ACI_Liberia_avgDay_20211212_20211225.rds")
ACI_Brunei_avgDay <- read_rds("data/ACI_Brunei_avgDay20240101_20240201.rds")
ACI_Singapore_avgDay <- read_rds("data/ACI_Singapore_avgDay_20200623_20200708.rds")

EVN_Baraboo_avgDay <- read_rds("data/EVN_Baraboo_avgDay_20240506_20240601.rds")
EVN_Germany_avgDay <- read_rds("data/EVN_Germany_avgDay_20240501_20240530.rds")
EVN_Ecuador_avgDay <- read_rds("data/EVN_Ecuador_avgDay_20231028_20231128.rds")
EVN_Peru_avgDay <- read_rds("data/EVN_Peru_avgDay_20230518_20230617.rds")
EVN_Gabon_avgDay <- read_rds("data/EVN_Gabon_avgDay_20221015_20221111.rds")
EVN_Liberia_avgDay <- read_rds("data/EVN_Liberia_avgDay_20211212_20211225.rds")
EVN_Brunei_avgDay <- read_rds("data/EVN_Brunei_avgDay20240101_20240201.rds")
EVN_Singapore_avgDay <- read_rds("data/EVN_Singapore_avgDay_20200623_20200708.rds")

ENT_Baraboo_avgDay <- read_rds("data/ENT_Baraboo_avgDay_20240506_20240601.rds")
ENT_Germany_avgDay <- read_rds("data/ENT_Germany_avgDay_20240501_20240530.rds")
ENT_Ecuador_avgDay <- read_rds("data/ENT_Ecuador_avgDay_20231028_20231128.rds")
ENT_Peru_avgDay <- read_rds("data/ENT_Peru_avgDay_20230518_20230617.rds")
ENT_Gabon_avgDay <- read_rds("data/ENT_Gabon_avgDay_20221015_20221111.rds")
ENT_Liberia_avgDay <- read_rds("data/ENT_Liberia_avgDay_20211212_20211225.rds")
ENT_Brunei_avgDay <- read_rds("data/ENT_Brunei_avgDay20240101_20240201.rds")
ENT_Singapore_avgDay <- read_rds("data/ENT_Singapore_avgDay_20200623_20200708.rds")


# add Country column
ACI_Baraboo_avgDay$Country <- "USA"
ACI_Germany_avgDay$Country <- "Germany"
ACI_Ecuador_avgDay$Country <- "Ecuador"
ACI_Peru_avgDay$Country <- "Peru"
ACI_Gabon_avgDay$Country <- "Gabon"
ACI_Liberia_avgDay$Country <- "Sierra Leone"
ACI_Brunei_avgDay$Country <- "Brunei"
ACI_Singapore_avgDay$Country <- "Singapore"

EVN_Baraboo_avgDay$Country <- "USA"
EVN_Germany_avgDay$Country <- "Germany"
EVN_Ecuador_avgDay$Country <- "Ecuador"
EVN_Peru_avgDay$Country <- "Peru"
EVN_Gabon_avgDay$Country <- "Gabon"
EVN_Liberia_avgDay$Country <- "Sierra Leone"
EVN_Brunei_avgDay$Country <- "Brunei"
EVN_Singapore_avgDay$Country <- "Singapore"

ENT_Baraboo_avgDay$Country <- "USA"
ENT_Germany_avgDay$Country <- "Germany"
ENT_Ecuador_avgDay$Country <- "Ecuador"
ENT_Peru_avgDay$Country <- "Peru"
ENT_Gabon_avgDay$Country <- "Gabon"
ENT_Liberia_avgDay$Country <- "Sierra Leone"
ENT_Brunei_avgDay$Country <- "Brunei"
ENT_Singapore_avgDay$Country <- "Singapore"

# add realm column
ACI_Baraboo_avgDay$Realm <- "Holarctic"
ACI_Germany_avgDay$Realm <- "Holarctic"
ACI_Ecuador_avgDay$Realm <- "Neotropic"
ACI_Peru_avgDay$Realm <- "Neotropic"
ACI_Gabon_avgDay$Realm <- "Afrotropic"
ACI_Liberia_avgDay$Realm <- "Afrotropic"
ACI_Brunei_avgDay$Realm <- "Indomalayan"
ACI_Singapore_avgDay$Realm <- "Indomalayan"

EVN_Baraboo_avgDay$Realm <- "Holarctic"
EVN_Germany_avgDay$Realm <- "Holarctic"
EVN_Ecuador_avgDay$Realm <- "Neotropic"
EVN_Peru_avgDay$Realm <- "Neotropic"
EVN_Gabon_avgDay$Realm <- "Afrotropic"
EVN_Liberia_avgDay$Realm <- "Afrotropic"
EVN_Brunei_avgDay$Realm <- "Indomalayan"
EVN_Singapore_avgDay$Realm <- "Indomalayan"

ENT_Baraboo_avgDay$Realm <- "Holarctic"
ENT_Germany_avgDay$Realm <- "Holarctic"
ENT_Ecuador_avgDay$Realm <- "Neotropic"
ENT_Peru_avgDay$Realm <- "Neotropic"
ENT_Gabon_avgDay$Realm <- "Afrotropic"
ENT_Liberia_avgDay$Realm <- "Afrotropic"
ENT_Brunei_avgDay$Realm <- "Indomalayan"
ENT_Singapore_avgDay$Realm <- "Indomalayan"


# merge avgDay
ACI_avgDay <- rbind(ACI_Baraboo_avgDay, ACI_Germany_avgDay, ACI_Ecuador_avgDay, ACI_Peru_avgDay, ACI_Gabon_avgDay, ACI_Liberia_avgDay, ACI_Brunei_avgDay, ACI_Singapore_avgDay)
EVN_avgDay <- rbind(EVN_Baraboo_avgDay, EVN_Germany_avgDay, EVN_Ecuador_avgDay, EVN_Peru_avgDay, EVN_Gabon_avgDay, EVN_Liberia_avgDay, EVN_Brunei_avgDay, EVN_Singapore_avgDay)
ENT_avgDay <- rbind(ENT_Baraboo_avgDay, ENT_Germany_avgDay, ENT_Ecuador_avgDay, ENT_Peru_avgDay, ENT_Gabon_avgDay, ENT_Liberia_avgDay, ENT_Brunei_avgDay, ENT_Singapore_avgDay)

# fix time
ACI_avgDay$Time <- as.POSIXct(substr(ACI_avgDay$Time, 12, 19), format= "%H:%M:%S")
ACI_avgDay$Time[is.na(ACI_avgDay$Time)] <- as.POSIXct("00:00:00", format= "%H:%M:%S")

EVN_avgDay$Time <- as.POSIXct(substr(EVN_avgDay$Time, 12, 19), format= "%H:%M:%S")
EVN_avgDay$Time[is.na(EVN_avgDay$Time)] <- as.POSIXct("00:00:00", format= "%H:%M:%S")

ENT_avgDay$Time <- as.POSIXct(substr(ENT_avgDay$Time, 12, 19), format= "%H:%M:%S")
ENT_avgDay$Time[is.na(ENT_avgDay$Time)] <- as.POSIXct("00:00:00", format= "%H:%M:%S")

# merge avg_day
avgDay <- merge(ACI_avgDay, EVN_avgDay)
avgDay <- merge(avgDay, ENT_avgDay)

# order the factors
avgDay$Country <- factor(avgDay$Country, levels = c("USA", "Ecuador", "Gabon", "Brunei",
                                                            "Germany", "Peru", "Sierra Leone", "Singapore"))

# normalize rgb
avgDay$rgb <- rgb((avgDay$avgACI-min(avgDay$avgACI)) / (max(avgDay$avgACI)-min(avgDay$avgACI)), 
                  (avgDay$avgEVN-min(avgDay$avgEVN)) / (max(avgDay$avgEVN)-min(avgDay$avgEVN)), 
                  (avgDay$avgENT-min(avgDay$avgENT)) / (max(avgDay$avgENT)-min(avgDay$avgENT)))

# write it
#write_csv(avgDay, "avgDay_20250414.csv")

# plot it
ggplot(avgDay, aes(x=Time, y=frequency, fill=rgb)) +
  geom_raster() +
  facet_wrap(~Country, scales="free_x", ncol=4) +
  scale_fill_identity() +
  scale_x_datetime(date_labels ="%H:%M") +
  ylab("Frequency (kHz)") +
  xlab("Time (hours:minutes)") +
  theme_minimal()
ggsave("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Soundscape Baselines/Draft 2/RGB_panelplot_20250711.PDF", 
       height=6, width=12)



ggplot(avgDay, aes(x=Time, y=frequency, fill=rgb)) +
  geom_raster() +
  facet_wrap(~Country, scales="free_x", ncol=4) +
  scale_fill_identity() +
  scale_x_datetime(date_labels ="%H:%M") +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  ylab("Frequency (kHz)") +
  xlab("Time (hours:minutes)") +
  ggtitle("ACI/EVN/ENT RGB - Averaged") +
  theme_minimal()


