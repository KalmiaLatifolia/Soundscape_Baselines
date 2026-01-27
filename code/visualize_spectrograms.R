
# visualize spectrograms in R

library(seewave)
library(tuneR)

setwd("/Users/lauraberman/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Documents/Wisconsin/Sound Forest Lab/Soundscape Baselines/Representative samples/Zenodo")

gs<-readWave("CCNR_20200627_230000.wav")


png(filename = "CCNR_20200627_230000.png",
    width = 3,
    height = 5,
    units = "in",
    res = 1000)

spectro(wave=gs, flim=c(0, 7.7), tlim = c(0, 3), collevels = seq(-50,0,1), scale = F, flab="", tlab="")

dev.off()

spectro(wave=gs, flim=c(0, 7.7), tlim = c(0, 3), collevels = seq(-50,0,1))


