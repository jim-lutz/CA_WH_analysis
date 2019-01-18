# load_RASS.R
# script to read RASS data from 
# started by Jim Lutz "Thu Nov 22 14:14:35 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
wd_RASS <- "../2009 RASS/"

# read the Survdata.csv
DT_RASS <-
  fread(file = paste0(wd_RASS,"Survdata.csv"))

# see what's there
length(names(DT_RASS))
# [1] 564
nrow(DT_RASS)
# [1] 25721

# save as an .Rdata file
save(DT_RASS, file = "data/DT_RASS.Rdata")

