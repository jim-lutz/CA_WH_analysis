# load_RECS.R
# script to read RECS 2009 data from ../2009 RECS/recs2009_public.csv
# and ../2009 RECS/recs2009_public_repweights.csv
# see ../2009 RECS/public_layout.csv for variable names
# and  ../2009 RECS/recs2009_public_codebook.xlsx for values of variables
# started by Jim Lutz "Fri Feb  1 07:30:01 2019"

# set packages & etc
source("setup.R")

# set up paths to working directories
wd_RECS <- "../2009 RECS/"

# read the recs2009_public.csv
DT_RECS <-
  fread(file = paste0(wd_RECS,"recs2009_public.csv"))

# see what's there
length(names(DT_RECS))
# [1] 940
nrow(DT_RECS)
# [1] 12083

# read the recs2009_public_repweights.csv
DT_RECS_repweights <-
  fread(file = paste0(wd_RECS,"recs2009_public_repweights.csv"))

# see what's there
length(names(DT_RECS_repweights))
# [1] 246
nrow(DT_RECS_repweights)
# [1] 12083

# merge on DOEID
DT_RECS <-
  merge(DT_RECS, DT_RECS_repweights, by='DOEID')

# see what's there
length(names(DT_RECS))
# [1] 1185
nrow(DT_RECS)
# [1] 12083

# keep only California data
DT_RECS_CA <-
  DT_RECS[ REPORTABLE_DOMAIN==26,]

# see what's there
length(names(DT_RECS_CA))
# [1] 1185
nrow(DT_RECS_CA)
# [1] 1606

# save as an .Rdata file
save(DT_RECS_CA, file = "data/DT_RECS_CA.Rdata")

++++++++++++++++
# look for anything like weight
grep("^W",names(DT_RASS), value = TRUE)
grep("^w",names(DT_RASS), value = TRUE)

# 'wt' is in there.
summary(DT_RASS$wt)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.3   119.7   201.9   448.0   248.4 14703.3 
# looks about right

sum(DT_RASS$wt)
# [1] 11523719
# looks about right

# look at distribution of wt
ggplot(data = DT_RASS, aes(x=1:25721, y=sort(wt))) +
  geom_step() + scale_y_log10() 

# look for pwhfuel3, Cleaned primary water heater fuel
DT_RASS[,list(weight = sum(wt)), by=pwhfuel3 ][order(-weight)]

# does it match Table 4-11: Water Heating Fuel Data Cleaning
# in Volume 1: Methodology
DT_RASS[,list(count = length(wt),
              percent = length(wt)/nrow(DT_RASS)),
        by=pwhfuel3 ][order(pwhfuel3)]
#    pwhfuel3 count      percent
# 1:        1 20047 0.7794020450
# 2:        2  2036 0.0791571090
# 3:        3  1180 0.0458769099
# 4:        4     8 0.0003110299
# 5:        5    20 0.0007775747
# 6:       97   717 0.0278760546
# 7:       99  1713 0.0665992769
# looks close enough to use. 

# look for SEASOCC
grep("SEASOCC",names(DT_RASS), value = TRUE)
# it's there
grep("SEAS",names(DT_RASS), value = TRUE)

# examine SEASOCC responses
DT_RASS[,list(count = length(wt)), by=SEASOCC]
              
