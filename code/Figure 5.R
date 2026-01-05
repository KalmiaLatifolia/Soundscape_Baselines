
# Figure 5
# Soundscape Baselines manuscript
# created 16 May 2025
# last edited 16 July 2025
# Laura Berman

# data and code required to generate Figure 5
# RGB panel plots by site

library(vroom)
library(ggplot2)
library(lubridate)

# set wd -----------------------------------------------------------------------
setwd("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Soundscape Baselines/SoundscapeBaselines_GIT")


# read in data (vroom MUCH faster than read.csv) -------------------------------

USA <- vroom("data/AllIndices_SiteAvg_10min_ForPCA_USA.csv")
Germany <- vroom("data/AllIndices_SiteAvg_10min_ForPCA_Germany.csv")
Ecuador <- vroom("data/AllIndices_SiteAvg_10min_ForPCA_Ecuador.csv")
Peru <- vroom("data/AllIndices_SiteAvg_10min_ForPCA_Peru.csv")
Gabon <- vroom("data/AllIndices_SiteAvg_10min_ForPCA_Gabon.csv")
Liberia <- vroom("data/AllIndices_SiteAvg_10min_ForPCA_Liberia.csv")
Singapore <- vroom("data/AllIndices_SiteAvg_10min_ForPCA_Singapore.csv")
Brunei <- vroom("data/AllIndices_SiteAvg_10min_ForPCA_Brunei.csv")

# add missing country info -----------------------------------------------------

USA$Country <- "USA"
Germany$Country <- "Germany"
Ecuador$Country <- "Ecuador"
Peru$Country <- "Peru"
Gabon$Country <- "Gabon"
Liberia$Country <- "Sierra Leone"
Singapore$Country <- "Singapore"
Brunei$Country <- "Brunei"

# merge countries -----------------------------------------------------(options) 

indices_10min <- rbind(USA, Germany, Ecuador, Peru, Gabon, Liberia, Brunei, Singapore) #ALL countries


# order the factors ------------------------------------------------------------

indices_10min$Site <- factor(indices_10min$Site, levels = c("W1","W2","W3","W4","W5","W6",
                                                               "D1","D2","D3","D4","D5",
                                                               "E1","E2","E3","E4","E5","E6",
                                                               "P1","P2","P3","P4","P5","P6",
                                                               "GAB1","GAB2","GAB3","GAB4","GAB5","GAB6",
                                                               "GRNP_1","GRNP_12","GRNP_176","GRNP_369","GRNP_397","GRNP_811",
                                                               "CCNR","DAFA","SBWR",
                                                               "Kiudang2","KiudangC"))

# remove sites with too much missing data ---------------------------------------

indices_10min <- subset(indices_10min, indices_10min$Site != "GRNP_811" & indices_10min$Site != "GRNP_176")

# normalize rgb ----------------------------------------------------------------

indices_10min$rgb <- rgb((indices_10min$ACI-min(indices_10min$ACI, na.rm = TRUE)) / (max(indices_10min$ACI, na.rm = TRUE)-min(indices_10min$ACI, na.rm = TRUE)), 
                  (indices_10min$EVN-min(indices_10min$EVN, na.rm = TRUE)) / (max(indices_10min$EVN, na.rm = TRUE)-min(indices_10min$EVN, na.rm = TRUE)), 
                  (indices_10min$ENT-min(indices_10min$ENT, na.rm = TRUE)) / (max(indices_10min$ENT, na.rm = TRUE)-min(indices_10min$ENT, na.rm = TRUE)))

# plot it ----------------------------------------------------------------------

# design faceting
design <- matrix(c(1:11, NA, 12:33, NA, NA, 34:36, NA, NA, NA, 37:38, NA, NA, NA, NA), 8, 6, byrow = TRUE)

# plot
ggplot(indices_10min, aes(x=hms(Time_10min), y=frequency, fill=rgb)) +
  geom_raster() +
  scale_fill_identity() +
  ggh4x::facet_manual(~Site, design) +
  scale_x_time(labels = scales::label_time(format = "%H:%M"),
               breaks = hms(c("06:00:00", "12:00:00", "18:00:00"))) +
  theme_minimal() +
  xlab("Time") +
  ylab("Frequency (kHz)")

# save
ggsave("Figure5_faceted_allSites_RGB_20250515.pdf", height=20, width=15)





