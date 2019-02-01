# slab_garage.R
# script to explore CA RECS 2009 data for occurences of slab foundations 
# and attached garages

# started by Jim Lutz "Fri Feb  1 07:46:23 2019"

# set packages & etc
source("setup.R")

# load data
load(file = paste0('data/', "DT_RECS_CA.Rdata"))

# factors for 
# TYPEHUQ	Type of housing unit	
# 1 2 3 4 5	
# Mobile Home Single-Family Detached Single-Family Attached Apartment in Building with 2 - 4 Units Apartment in Building with 5+ Units


# pie chart number of housing units by type of building
ggplot(data = DT_RECS_CA) +
  https://www.r-bloggers.com/pie-charts-in-ggplot2/
  
  
