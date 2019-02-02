# slab_garage.R
# script to explore CA RECS 2009 data for occurences of slab foundations 
# and attached garages
# started by Jim Lutz "Fri Feb  1 07:46:23 2019"

# set packages & etc
source("setup.R")

# load data
load(file = paste0('data/', "DT_RECS_CA.Rdata"))

# see ../2009 RECS/recs2009_public_codebook.xlsx, ../2009 RECS/public_layout.csv and 
# ../2009 RECS/using-microdata-022613.pdf for information about data

# factors for 
# TYPEHUQ	Type of housing unit	
# 1 2 3 4 5	
# Mobile Home Single-Family Detached Single-Family Attached Apartment in Building with 2 - 4 Units Apartment in Building with 5+ Units

# chart number of housing units by type of building
ggplot(data = DT_RECS_CA) +
  geom_bar(aes(TYPEHUQ, weight=NWEIGHT))
# Error in FUN(X[[i]], ...) : object 'NWEIGHT' not found
  
# what's with the weight?  
grep("WEIGHT|weight", names(DT_RECS_CA), value = TRUE)

DT_RECS_CA[NWEIGHT.x != NWEIGHT.y, list(NWEIGHT.x, NWEIGHT.y)]
# NWEIGHT.y is 3 decimals, NWEIGHT.x is 2 decimals?

ggplot(data = DT_RECS_CA,
       aes(x=NWEIGHT.x, y=NWEIGHT.y)) + geom_point()

with(DT_RECS_CA,
     summary.lm(lm(NWEIGHT.y ~ NWEIGHT.x))
     )



#  https://www.r-bloggers.com/pie-charts-in-ggplot2/
  
  
