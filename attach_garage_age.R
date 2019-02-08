# attach_garage_age.R
# script to plot attached garages (by number of cars?) by age for 'Single-Family Detached'
# from CA RECS 2009 data
# started by Jim Lutz "Thu Feb  7 17:11:09 2019"

# set packages & etc
source("setup.R")

# setup  working directories
# use this for scripts 
wd <- getwd()
wd_data    <- paste(wd,"/data/",sep="")      # use this for interim data files
wd_charts  <-paste(wd,"/charts/",sep="")     # use this for charts, ggsave puts in /

# load data 2009 RECS California data
load(file = paste0('data/', "DT_RECS_CA.Rdata"))

# see ../2009 RECS/recs2009_public_codebook.xlsx, ../2009 RECS/public_layout.csv and 
# ../2009 RECS/using-microdata-022613.pdf for information about data

# add factors for TYPEHUQ	Type of housing unit	
DT_RECS_CA[ ,F_TYPEHUQ:= factor(x=TYPEHUQ,
                                levels = c(2,3,4,5,1), # Single-Family first
                                labels = c('Single-Family Detached', 
                                          'Single-Family Attached', 
                                          'Apartment in Building with 2 - 4 Units', 
                                          'Apartment in Building with 5+ Units',
                                          'Mobile Home')
                               )
            ]

# add factors for YEARMADERANGE	Year range when housing unit was built
DT_RECS_CA[ , 
            F_YEARMADERANGE:= factor(x=YEARMADERANGE,
                                     levels = c('1', '2', '3', '4', '5', '6', '7', '8'),
                                     labels = c('Before 1950', '1950 to 1959', '1960 to 1969',
                                                '1970 to 1979', '1980 to 1989', '1990 to 1999',
                                                '2000 to 2004', '2005 to 2009')
                                     )
            ]


# add factors for PRKGPLC1	Attached garage	
DT_RECS_CA[ , 
            F_PRKGPLC1:= factor(x=PRKGPLC1,
                                levels = c(0, 1, -2),
                                labels = c('No', 'Yes', 'Not Applicable')
            )
            ]

# how many 'Single-Family Detached' have attached garages?
DT_RECS_CA[ TYPEHUQ==2,
            list(TYPEHUQ = unique(TYPEHUQ),
                 nTYPEHUQ = sum(NWEIGHT.y), # number housing units
                Attached_Garage = unique(F_PRKGPLC1)),
            by=c("PRKGPLC1")
            ]
#    PRKGPLC1 TYPEHUQ nTYPEHUQ attach_Garage
# 1:        1       2  4976652           Yes
# 2:        0       2  2081562            No
# nearly 5 million Single-Family Detached houses with attached garages

# reset 'No Attached Garage'
DT_RECS_CA[SIZEOFGARAGE ==-2, SIZEOFGARAGE:=0 ]

# add factors for SIZEOFGARAGE	Size of attached garage
DT_RECS_CA[ , 
            F_SIZEOFGARAGE:= factor(x=SIZEOFGARAGE,
                                levels = c(0, 1, 2, 3),
                                labels = c('No Attached Garage',
                                           'One-car garage',
                                           'Two-car garage',
                                           'Three-or-more-car'
                                           )
                                )
            ]

# check this did what was expected
DT_RECS_CA[, list(F_SIZEOFGARAGE = unique(F_SIZEOFGARAGE)), 
           by=SIZEOFGARAGE]

# how many 'Single-Family Detached' with attached garages by Size of attached garage?
DT_RECS_CA[ TYPEHUQ==2 ,
            list(nTYPEHUQ = sum(NWEIGHT.y) # number housing units
                 ),
            by=c("SIZEOFGARAGE")
            ][order(SIZEOFGARAGE)]
#    SIZEOFGARAGE  nTYPEHUQ
# 1:            0 2081562.1
# 2:            1  756816.7
# 3:            2 3545159.5
# 4:            3  674675.6

# make a simple data.table to plot
DT_GARAGE_AGE <-
  DT_RECS_CA[ TYPEHUQ==2, # 'Single-Family Detached' only
              list(nTYPEHUQ = sum(NWEIGHT.y), # number
                   YEARMADERANGE = unique(YEARMADERANGE),
                   SIZEOFGARAGE = unique(SIZEOFGARAGE)
                   ),
              by=c("F_TYPEHUQ","F_YEARMADERANGE", "F_PRKGPLC1", "F_SIZEOFGARAGE")][order(YEARMADERANGE, SIZEOFGARAGE)]

# date at end of bin
DT_GARAGE_AGE[,list(F_YEARMADERANGE = unique(F_YEARMADERANGE)), by=YEARMADERANGE ]

# YEAREND
DT_YEAREND <-
  data.table(YEARMADERANGE = c(1:8),
             YEAREND = c(1950, 1959, 1969, 1979, 1989, 1999, 2004, 2009)
             )

# add YEAREND to DT_GARAGE_AGE
DT_GARAGE_AGE <-
  merge(DT_GARAGE_AGE, DT_YEAREND, by='YEARMADERANGE')

names(DT_GARAGE_AGE)
str(DT_GARAGE_AGE)
DT_GARAGE_AGE

# dcast (long to wide)
DT_ATTACH_GARAGE <-
dcast(DT_GARAGE_AGE[, list(YEAREND,
                           SIZEOFGARAGE,
                           nTYPEHUQ)
                    ],
      YEAREND ~ SIZEOFGARAGE, 
      value.var = c("nTYPEHUQ"),
      fill = 0)

# list of values of SIZEOFGARAGE that become column names
cols.SIZEOFGARAGE <- as.character(sort(unique(DT_GARAGE_AGE$SIZEOFGARAGE)))

# accumulate prior years
DT_ATTACH_GARAGE[, (cols.SIZEOFGARAGE) := lapply(.SD, cumsum), 
                 .SDcols=cols.SIZEOFGARAGE]

# cleanup the names
setnames(DT_ATTACH_GARAGE, 
         old = 2:5,
         new = paste("SIZEOFGARAGE",c(0:3),sep = "_") )

# ymin and ymax for ribbons
# Three-or-more-car, ymin = 0, ymax = nTYPEHUQ, Y0:Y1
DT_ATTACH_GARAGE[ , Y0 := 0]
DT_ATTACH_GARAGE[ , Y1 := Y0 + SIZEOFGARAGE_3]

# Two-car garage, ymin = ymax(Three-or-more-car), ymax = ymin + nTYPEHUQ, Y1:Y2
DT_ATTACH_GARAGE[ , Y2 := Y1 + SIZEOFGARAGE_2 ]

# One-car garage, ymin = ymax(Two-car garage), ymax = ymin + nTYPEHUQ, Y2:Y3
DT_ATTACH_GARAGE[ , Y3 := Y2 + SIZEOFGARAGE_1]

# No Attached Garage, ymin = ymax(One-car garage), ymax = ymin + nTYPEHUQ, Y3:Y4
DT_ATTACH_GARAGE[ , Y4 := Y3 + SIZEOFGARAGE_0 ]

names(DT_ATTACH_GARAGE)

# see about melt (wide to long) for geom_ribbon
DT_Y <-
  melt(DT_ATTACH_GARAGE[,list(YEAREND,
                              Y0, Y1, Y2, Y3, Y4)], 
       id=c("YEAREND"),
       measure.vars = c("Y0", "Y1", "Y2", "Y3", "Y4"),
       variable.name = "Y",
       value.name = "n")

# want stacked ribbons in this order from top to bottom
# 'No Attached Garage', Y0:Y1
# 'One-car garage', Y1:Y2
# 'Two-car garage', Y2:Y3
# 'Three-or-more-car', Y3:Y4

# define YMIN and YMAX as n[1:32] and n[9:40]
DT_YRIBBONS <-
  with(DT_Y,
       data.table(YEAREND=YEAREND[1:32],
                  SIZEOFGARAGE=str_sub(Y,2)[1:32],
                  YMIN=n[1:32],
                  YMAX=n[9:40])
       ) 

# # bogus data to plot for legend
# DT_bogus <- data.table(
#   x=c(1950:1954),
#   y=c(-5e6,-5.2e6,-5.4e6,-5.6e6,-5.8e6),
#   type=c("Single-Family Detached",
#          "Single-Family Attached",
#          "Apartment in Building with 2 - 4 Units",
#          "Apartment in Building with 5+ Units",
#          "Mobile Home"
#          ),
#   colors= c("chartreuse4", "lightskyblue", "lightskyblue", "deepskyblue4", "deepskyblue4")  
#   )
# 

# plot of number of houses by type of attached garage by age bin
# see https://ggplot2.tidyverse.org/reference/geom_ribbon.html
ggplot(data = DT_YRIBBONS, aes(x=YEAREND)) +
  geom_ribbon(aes(ymin=YMIN, ymax=YMAX, fill=SIZEOFGARAGE), 
              color='black', show.legend = TRUE )


+
  geom_area(aes(y=area2), fill="lightskyblue", color='black' ) +
  geom_area(aes(y=area3), fill="lightskyblue", color='black' ) +
  geom_area(aes(y=area4), fill="deepskyblue4", color='black' ) +
  geom_area(aes(y=area5), fill="deepskyblue4", color='black' ) +
  labs(title="Housing Units by Type in California", # title, axes labels,  caption 
     # subtitle="", 
     caption="Source: RECS 2009", 
     y="number of housing units (million)",
     x="year built") +  
  scale_y_continuous(breaks=c(0,2.5e6,5.0e6,7.5e6,10.0e6,12.5e6),
                     labels = c("0","2.5","5.0","7.5","10.0","12.5"),
                     limits = c(0,12.5e6)) +
  # this is plotting the hand crafted data just to get the right legends
  geom_point(data = DT_bogus, aes(x=x,y=y,color=type) ) +
  scale_color_manual(values = c("chartreuse4", 
                                "lightskyblue", 
                                "lightskyblue", 
                                "deepskyblue4", 
                                "deepskyblue4") ,
                     labels = c("Mobile Home",
                                "Apartment in Building with 5+ Units",
                                "Apartment in Building with 2 - 4 Units",
                                "Single-Family Attached",
                                "Single-Family Detached")
                     ) + 
  guides(color = guide_legend(title = "type of building") )

names(DT_nTYPEHUQ_YEAREND)

# cleanup data for sharing
DT_nTYPEHUQ_YEAREND <- 
  DT_nTYPEHUQ_YEAREND[ , list(YEAREND,F_YEARMADERANGE,
                              `Mobile Home`            = round(TYPEHUQ_1),
                              `Single-Family Detached` = round(TYPEHUQ_2),
                              `Single-Family Attached` = round(TYPEHUQ_3),
                              `Apartment in Building with 2 - 4 Units` = round(TYPEHUQ_4),
                              `Apartment in Building with 5+ Units` = round(TYPEHUQ_5)
                              )
                       ]

#    TYPEHUQ                              F_TYPEHUQ
# 1:       1                            Mobile Home
# 2:       2                 Single-Family Detached
# 3:       3                 Single-Family Attached
# 4:       4 Apartment in Building with 2 - 4 Units
# 5:       5    Apartment in Building with 5+ Units


# get date to include in file name
d <- format(Sys.time(), "%F")

# save chart
ggsave(filename = paste0("type_by_year","_",d,".png"), 
       path=wd_charts, scale = 1.5) 

# save data
fwrite(DT_nTYPEHUQ_YEAREND, file = paste0(wd_data,"type_by_year","_",d,"csv") )




