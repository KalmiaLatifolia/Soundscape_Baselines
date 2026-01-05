
# PMN panel plot
# for soundscape baselines manuscript


library(readr)
library(lubridate)

# set wd -----------------------------------------------------------------------
setwd("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Soundscape Baselines/SoundscapeBaselines_GIT")


# load avgDay files ------------------------------------------------------------
PMN_Baraboo_avgDay <- read_rds("data/PMN_Baraboo_avgDay_20240506_20240601.rds")
PMN_Germany_avgDay <- read_rds("data/PMN_Germany_avgDay_20240501_20240530.rds")
PMN_Ecuador_avgDay <- read_rds("data/PMN_Ecuador_avgDay_20231028_20231128.rds")
PMN_Peru_avgDay <- read_rds("data/PMN_Peru_avgDay_20230518_20230617.rds")
PMN_Gabon_avgDay <- read_rds("data/PMN_Gabon_avgDay_20221015_20221111.rds")
PMN_Liberia_avgDay <- read_rds("data/PMN_Liberia_avgDay_20211212_20211225.rds")
PMN_Brunei_avgDay <- read_rds("data/PMN_Brunei_avgDay20240101_20240201.rds")
PMN_Singapore_avgDay <- read_rds("data/PMN_Singapore_avgDay_20200623_20200708.rds")

# add Country column -----------------------------------------------------------
PMN_Baraboo_avgDay$Country <- "USA"
PMN_Germany_avgDay$Country <- "Germany"
PMN_Ecuador_avgDay$Country <- "Ecuador"
PMN_Peru_avgDay$Country <- "Peru"
PMN_Gabon_avgDay$Country <- "Gabon"
PMN_Liberia_avgDay$Country <- "Sierra Leone"
PMN_Brunei_avgDay$Country <- "Brunei"
PMN_Singapore_avgDay$Country <- "Singapore"


# add realm column -------------------------------------------------------------
PMN_Baraboo_avgDay$Realm <- "Holarctic"
PMN_Germany_avgDay$Realm <- "Holarctic"
PMN_Ecuador_avgDay$Realm <- "Neotropic"
PMN_Peru_avgDay$Realm <- "Neotropic"
PMN_Gabon_avgDay$Realm <- "Afrotropic"
PMN_Liberia_avgDay$Realm <- "Afrotropic"
PMN_Brunei_avgDay$Realm <- "Indomalayan"
PMN_Singapore_avgDay$Realm <- "Indomalayan"


# merge PMN_avgDay -------------------------------------------------------------
PMN_avgDay <- rbind(PMN_Baraboo_avgDay, PMN_Germany_avgDay, PMN_Ecuador_avgDay, PMN_Peru_avgDay, PMN_Gabon_avgDay, PMN_Liberia_avgDay, PMN_Brunei_avgDay, PMN_Singapore_avgDay)

# order the factors ------------------------------------------------------------
PMN_avgDay$Country <- factor(PMN_avgDay$Country, levels = c("USA", "Ecuador", "Gabon", "Brunei",
                                                            "Germany", "Peru", "Sierra Leone", "Singapore"))

# fix time ---------------------------------------------------------------------
PMN_avgDay$Time <- as.POSIXct(substr(PMN_avgDay$Time, 12, 19), format= "%H:%M:%S")
PMN_avgDay$Time[is.na(PMN_avgDay$Time)] <- as.POSIXct("00:00:00", format= "%H:%M:%S")

min(PMN_avgDay$Time)
max(PMN_avgDay$Time)

# plot it ----------------------------------------------------------------------
ggplot(PMN_avgDay, aes(x=Time, y=frequency, fill=avgPMN)) +
  geom_raster() +
  facet_wrap(~Country, scales="free_x", ncol=4) +
  scale_fill_viridis_c() +
  scale_x_datetime(date_labels ="%H:%M") +
  ylab("Frequency (kHz)") +
  xlab("Time (hours:minutes)") +
  theme_minimal()
ggsave("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Soundscape Baselines/Draft 2/PMN_panelplot_20250711.PDF", 
       height=6, width=12)



