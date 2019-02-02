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
  DT_RECS_CA[ , list(fTYPEHUQ = sum(NWEIGHT.y)/sum(DT_RECS_CA$NWEIGHT.y),
                     nTYPEHUQ = sum(NWEIGHT.y),
                     TYPEHUQ  = unique(TYPEHUQ),
                     YEARMADERANGE = unique(YEARMADERANGE)
                     ),
              by=c("F_TYPEHUQ","F_YEARMADERANGE")][order(YEARMADERANGE,TYPEHUQ)]


