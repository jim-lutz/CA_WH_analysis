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
DT_RECS_CA[ , 
            F_TYPEHUQ:= factor(x=TYPEHUQ,
                               levels = 1:5,
                               labels = c('Mobile Home', 
                                           'Single-Family Detached', 
                                           'Single-Family Attached', 
                                           'Apartment in Building with 2 - 4 Units', 
                                           'Apartment in Building with 5+ Units')
                                    )
            ]

# make a simple data.table to plot
DT_TYPEHUQ <-
  DT_RECS_CA[ , list(fTYPEHUQ = sum(NWEIGHT.y)/sum(DT_RECS_CA$NWEIGHT.y),
                     nTYPEHUQ = sum(NWEIGHT.y)),
            by=c("TYPEHUQ","F_TYPEHUQ")][order(TYPEHUQ)]
#    TYPEHUQ                              F_TYPEHUQ   fTYPEHUQ  nTYPEHUQ
# 1:       1                            Mobile Home 0.03226220  394079.3
# 2:       2                 Single-Family Detached 0.57783682 7058213.8
# 3:       3                 Single-Family Attached 0.07013561  856698.9
# 4:       4 Apartment in Building with 2 - 4 Units 0.08466972 1034231.5
# 5:       5    Apartment in Building with 5+ Units 0.23509564 2871667.6

# compare to Figure 1.10 California Housing Stock by Type
# in /home/jiml/library/to read/California's-Housing-Future-Main-Document-Draft_2017.pdf

DT_TYPEHUQ[str_detect(F_TYPEHUQ,"Mobile"),]
#    TYPEHUQ   F_TYPEHUQ  fTYPEHUQ nTYPEHUQ
# 1:       1 Mobile Home 0.0322622 394079.3
#                           4%      500,000 

DT_TYPEHUQ[str_detect(F_TYPEHUQ,"Single-Family"),
           list(fTYPEHUQ=sum(fTYPEHUQ),
                nTYPEHUQ=sum(nTYPEHUQ))]
#     fTYPEHUQ nTYPEHUQ
# 1: 0.6479724  7914913
#     65%       9000000

DT_TYPEHUQ[str_detect(F_TYPEHUQ,"Apartment"),
           list(fTYPEHUQ=sum(fTYPEHUQ),
                nTYPEHUQ=sum(nTYPEHUQ))]
#     fTYPEHUQ nTYPEHUQ
# 1: 0.3197654  3905899
#      31%      4300000

# probably close enough

# chart number of housing units by type of building
ggplot(data = DT_RECS_CA) +
  geom_bar(aes(x="", y=NWEIGHT.y/sum(NWEIGHT.y), fill=F_TYPEHUQ)) + 
  coord_polar(theta = "x")




#  https://www.r-bloggers.com/pie-charts-in-ggplot2/
  
  
