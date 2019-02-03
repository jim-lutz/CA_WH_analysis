# house_age.R
# script to plot type of housing unit by age from CA RECS 2009 data
# started by Jim Lutz "Sat Feb  2 14:02:36 2019"

# set packages & etc
source("setup.R")

# setup  working directories
# use this for scripts 
wd <- getwd()
wd_data    <- paste(wd,"/data/",sep="")      # use this for interim data files
wd_charts  <-paste(wd,"/charts/",sep="")     # use this for charts, ggsave puts in /

# load data
load(file = paste0('data/', "DT_RECS_CA.Rdata"))

# see ../2009 RECS/recs2009_public_codebook.xlsx, ../2009 RECS/public_layout.csv and 
# ../2009 RECS/using-microdata-022613.pdf for information about data

# factors for TYPEHUQ	Type of housing unit	
DT_RECS_CA[ ,F_TYPEHUQ:= factor(x=TYPEHUQ,
                                levels = c(2,3,4,5,1), # Single-Family first
                                labels = c('Single-Family Detached', 
                                          'Single-Family Attached', 
                                          'Apartment in Building with 2 - 4 Units', 
                                          'Apartment in Building with 5+ Units',
                                          'Mobile Home')
                               )
            ]

# factors for YEARMADERANGE	Year range when housing unit was built
DT_RECS_CA[ , 
            F_YEARMADERANGE:= factor(x=YEARMADERANGE,
                                     levels = c('1', '2', '3', '4', '5', '6', '7', '8'),
                                     labels = c('Before 1950', '1950 to 1959', '1960 to 1969',
                                                '1970 to 1979', '1980 to 1989', '1990 to 1999',
                                                '2000 to 2004', '2005 to 2009')
                                     )
            ]

# make a simple data.table to plot
DT_HOUSE_AGE <-
  DT_RECS_CA[ , list(fTYPEHUQ = sum(NWEIGHT.y)/sum(DT_RECS_CA$NWEIGHT.y), # fraction
                     nTYPEHUQ = sum(NWEIGHT.y), # number
                     TYPEHUQ  = unique(TYPEHUQ),
                     YEARMADERANGE = unique(YEARMADERANGE)
                     ),
              by=c("F_TYPEHUQ","F_YEARMADERANGE")][order(YEARMADERANGE,TYPEHUQ)]

# date at end of bin
DT_HOUSE_AGE[,list(F_YEARMADERANGE = unique(F_YEARMADERANGE)), by=YEARMADERANGE ]

# YEAREND
DT_YEAREND <-
  data.table(YEARMADERANGE = c(1:8),
             YEAREND = c(1950, 1959, 1969, 1979, 1989, 1999, 2004, 2009)
             )

# add YEAREND to DT_HOUSE_AGE
DT_HOUSE_AGE <-
  merge(DT_HOUSE_AGE, DT_YEAREND, by='YEARMADERANGE')

names(DT_HOUSE_AGE)
unique(DT_HOUSE_AGE$F_TYPEHUQ)

# dcast (long to wide), 
DT_nTYPEHUQ_YEAREND <-
  dcast(DT_HOUSE_AGE,
        YEAREND + F_YEARMADERANGE ~ TYPEHUQ, 
        value.var = c("nTYPEHUQ"))

# cleanup the names
setnames(DT_nTYPEHUQ_YEAREND, 
         old = 3:7,
         new = paste("TYPEHUQ",1:5,sep = "_") )

# cummulative number of house by type
DT_nTYPEHUQ_YEAREND[ , `:=` (TYPEHUQ_1c = cumsum(TYPEHUQ_1),
                             TYPEHUQ_2c = cumsum(TYPEHUQ_2),
                             TYPEHUQ_3c = cumsum(TYPEHUQ_3),
                             TYPEHUQ_4c = cumsum(TYPEHUQ_4),
                             TYPEHUQ_5c = cumsum(TYPEHUQ_5))
                     ]

DT_HOUSE_AGE[,list(F_TYPEHUQ=unique(F_TYPEHUQ)),by=TYPEHUQ]
#    TYPEHUQ                              F_TYPEHUQ
# 1:       1                            Mobile Home
# 2:       2                 Single-Family Detached
# 3:       3                 Single-Family Attached
# 4:       4 Apartment in Building with 2 - 4 Units
# 5:       5    Apartment in Building with 5+ Units

# want stacked areas in this order from top to bottom
# Mobile Home, (1+5+4+3+2) 
# Apartment in Building with 5+ Units, (5+4+3+2) 
# Apartment in Building with 2 - 4 Units, (4+3+2) 
# Single-Family Attached, (3+2) 
# Single-Family Detached, (2)                

# build the cumulative geom_areas to plot
DT_nTYPEHUQ_YEAREND[ , area5 :=         TYPEHUQ_2c ] # Single-Family Detached
DT_nTYPEHUQ_YEAREND[ , area4 := area5 + TYPEHUQ_3c ] # += Single-Family Attached
DT_nTYPEHUQ_YEAREND[ , area3 := area4 + TYPEHUQ_4c ] # += Apartment in Building with 2 - 4 Units
DT_nTYPEHUQ_YEAREND[ , area2 := area3 + TYPEHUQ_5c ] # += Apartment in Building with 5+ Units
DT_nTYPEHUQ_YEAREND[ , area1 := area2 + TYPEHUQ_1c ] # += Mobile Home

names(DT_nTYPEHUQ_YEAREND)
# cleanup 
DT_nTYPEHUQ_YEAREND <- 
  DT_nTYPEHUQ_YEAREND[ , list(YEAREND,F_YEARMADERANGE,
                              area1, area2, area3, area4, area5)]

# plot of number of houses by type by age bin
# see http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Stacked%20Area%20Chart
ggplot(data = DT_HOUSE_AGE, aex(x=YEAREND)) +
  
  # [1] Mobile Home                            Single-Family Detached                
  [3] Single-Family Attached                 Apartment in Building with 2 - 4 Units
[5] Apartment in Building with 5+ Units   

     geom_bar(stat="identity", position="identity", color="black") +
  scale_fill_manual(values=c("deepskyblue4","deepskyblue4",
                             "lightskyblue","lightskyblue","chartreuse4")) 
length(DT_HOUSE_AGE$F_YEARMADERANGE)  
  

