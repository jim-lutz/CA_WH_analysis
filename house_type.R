# house_type.R
# script to get type of housing unit from CA RECS 2009 data
# started by Jim Lutz "Fri Feb  1 07:46:23 2019"

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

# factors for 
# TYPEHUQ	Type of housing unit	
# 1 2 3 4 5	
# Mobile Home Single-Family Detached Single-Family Attached Apartment in Building with 2 - 4 Units Apartment in Building with 5+ Units
DT_RECS_CA[ , 
            F_TYPEHUQ:= factor(x=TYPEHUQ,
                               levels = c(2,3,4,5,1),
                               labels = c('Single-Family Detached', 
                                           'Single-Family Attached', 
                                           'Apartment in Building with 2 - 4 Units', 
                                           'Apartment in Building with 5+ Units',
                                          'Mobile Home')
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
# https://www.r-bloggers.com/how-to-make-a-pie-chart-in-r/
ggplot(data = DT_TYPEHUQ,
       aes(x="", y=fTYPEHUQ, fill=F_TYPEHUQ,)) +
  geom_bar(stat="identity", width=1, color="black") + 
  coord_polar(theta = "y", start=0, direction = -1) + 
  geom_text(aes(label = paste0(round(fTYPEHUQ*100), 
                               "%\n", round(nTYPEHUQ/1000000,1),"M")), 
            position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("deepskyblue4","deepskyblue4",
                             "lightskyblue","lightskyblue","chartreuse4")) + 
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "California Housing Stock by Type",
       subtitle = "from 2009 RECS") + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# get date to include in file name
d <- format(Sys.time(), "%F")

# save chart
ggsave(filename = paste0("type_of_housing","_",d,".png"), 
       path=wd_charts, scale = 1.5) 

# save data
fwrite(DT_TYPEHUQ, file = paste0(wd_data,"type_of_housing","_",d,"csv") )

                                                                                                              
  
  
