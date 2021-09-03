###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 3 of x 
# The purpose of this script is to merge the data into approperiate groupings for  
# data checking and processing
# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: Nicola Walker (Cefas) Jonas Hentati Sundberg (SLU)
# Edits by: Ruth Kelly (AFBI), June 2021 - R version 4.0.4 (2021-02-15) -- "Lost Library Book"
####################
# Check str of data#
####################
# using data tables is better than data frames
# better able to deal with larger data files.
# convert all HH data frames to data tables

# cut each dataset to the start date used by Moriarty et al.2017 - RK 2021
HH_EVHOE<-as.data.table(HH_EVHOE)
table(HH_EVHOE$Year)

HH_FRCGFS<-as.data.table(HH_FRCGFS)
table(HH_FRCGFS$Year)

HH_IGFS<-as.data.table(HH_IGFS)
table(HH_IGFS$Year)

HH_NSIBTS<-as.data.table(HH_NSIBTS)
table(HH_NSIBTS$Year, HH_NSIBTS$Quarter)

HH_NSIBTS <- HH_NSIBTS[HH_NSIBTS$Year >1982,]
## remove earlier years in Q 3/4

x1 <- which(HH_NSIBTS$Year <1998 & HH_NSIBTS$Quarter %in% c(3,4))
length(x1)

HH_NSIBTS <- HH_NSIBTS[-x1,]

table(HH_NSIBTS$Year, HH_NSIBTS$Quarter)

HH_ROCK<-as.data.table(HH_ROCK)
table(HH_ROCK$Year)


HH_SWC<-as.data.table(HH_SWC)
table(HH_SWC$Year)

HH_NIGFS<-as.data.table(HH_NIGFS)

HH_BTS<-as.data.table(HH_BTS)

HH_BTS <- HH_BTS[HH_BTS$Year> 1986,]

HH_BTS_DE <- HH_BTS[HH_BTS$Country == "DE",]
table(HH_BTS_DE$Ship, HH_BTS_DE$Year)
table(HH_BTS_DE$DoorSpread, HH_BTS_DE$Year)

x2 <- which(HH_BTS$Year <2002 & HH_BTS$Country == "DE")
length(x2)

HH_BTS <- HH_BTS[-x2,]

## Remove DE prior to 2002 - this wasn't on Datras yet when 
# Moriarty did there work. It does appear to be the same ship, 
# but the gear type and other params would need to be error checked 
# before inclusion

HH_PT<-as.data.table(HH_PT)
table(HH_PT$Year) ## 2002-2018 present on Datras - Moriarty had 2001, but that's not on Datras anymore


## added Spain 2021
## There are know to have been big changes in the Spannish data on DATRAS 
## User caution advised

HH_SP_PORC <- as.data.table(HH_SP_PORC)
table(HH_SP_PORC$Year)
HH_SP_NORTH <- as.data.table(HH_SP_NORTH)
table(HH_SP_NORTH$Year)
HH_SP_ARSA <- as.data.table(HH_SP_ARSA)
table(HH_SP_ARSA$Year, HH_SP_ARSA$Ship) ### remove 1996 to give continous series from 2000
HH_SP_ARSA <- HH_SP_ARSA[HH_SP_ARSA$Year >1999,]




# merge DATRAS HH data files


HH<-rbind(HH_EVHOE, HH_FRCGFS, HH_IGFS, HH_NSIBTS, 
          HH_ROCK, HH_SWC, HH_NIGFS, HH_PT, HH_BTS, 
          HH_SP_ARSA,HH_SP_PORC, HH_SP_NORTH)#, fill=TRUE)

# check - should return TRUE
nrow(HH) == nrow(HH_EVHOE)+nrow(HH_FRCGFS)+nrow(HH_IGFS)+
  nrow(HH_NSIBTS)+nrow(HH_ROCK)+nrow(HH_SWC)+
  nrow(HH_NIGFS)+nrow(HH_PT)+nrow(HH_BTS)  +
  nrow(HH_SP_PORC) + nrow(HH_SP_NORTH) + nrow(HH_SP_ARSA) 

# convert HL data trames to data tables
# cut to same years
HL_EVHOE<-as.data.table(HL_EVHOE)

HL_FRCGFS<-as.data.table(HL_FRCGFS)
HL_IGFS<-as.data.table(HL_IGFS)
HL_NSIBTS<-as.data.table(HL_NSIBTS)

table(HL_NSIBTS$Year)
HL_NSIBTS <- HL_NSIBTS[HL_NSIBTS$Year >1982,]
## remove earlier years in Q 3/4

x3 <- which(HL_NSIBTS$Year <1998 & HL_NSIBTS$Quarter %in% c(3,4))
length(x3)

HL_NSIBTS[head(x3),]

HL_NSIBTS <- HL_NSIBTS[-x3,]

table(HL_NSIBTS$Year, HL_NSIBTS$Quarter)


HL_ROCK<-as.data.table(HL_ROCK)
table(HL_ROCK$Year)

HL_SWC<-as.data.table(HL_SWC)
table(HL_SWC$Year)

HL_NIGFS<-as.data.table(HL_NIGFS)
  table(HL_NIGFS$Year)

HL_BTS<-as.data.table(HL_BTS)


HL_BTS <- HL_BTS[HL_BTS$Year> 1986,]
x4 <- which(HL_BTS$Year <2002 & HL_BTS$Country == "DE")
length(x4)
HL_BTS <- HL_BTS[-x4,]

HL_PT<-as.data.table(HL_PT)

## spain added
HL_SP_ARSA <-as.data.table(HL_SP_ARSA)
table(HL_SP_ARSA$Year, HL_SP_ARSA$Ship) ### remove 1996 to give continous series from 2000
HL_SP_ARSA <- HL_SP_ARSA[HL_SP_ARSA$Year >1999,]

HL_SP_PORC<-as.data.table(HL_SP_PORC)
table(HH_SP_PORC$Year)

HL_SP_NORTH<-as.data.table(HL_SP_NORTH)
table(HH_SP_NORTH$Year)

### bind
HL<-rbind(HL_EVHOE, HL_FRCGFS,HL_IGFS, HL_NSIBTS, 
          HL_ROCK, HL_SWC,HL_NIGFS,HL_PT,HL_BTS,
          HL_SP_ARSA,HL_SP_PORC, HL_SP_NORTH)#, fill=TRUE)

# check - should return true
nrow(HL) == nrow(HL_EVHOE)+nrow(HL_FRCGFS)+nrow(HL_IGFS)+
  nrow(HL_NSIBTS)+nrow(HL_ROCK)+nrow(HL_SWC)+  
  nrow(HL_NIGFS)+nrow(HL_PT)+nrow(HL_BTS)+nrow(HL_SP_ARSA) + 
  nrow(HL_SP_PORC) + nrow(HL_SP_NORTH)

# Remove the intermediate files to free up space
rm(HH_EVHOE,HH_FRCGFS,HH_IGFS,HH_NSIBTS,HH_ROCK,HH_SWC,
   HH_NIGFS,HH_BTS,HH_PT,HH_SP_ARSA, HH_SP_NORTH, HH_SP_PORC,
   HL_EVHOE,HL_FRCGFS,HL_IGFS,HL_NSIBTS,HL_ROCK,HL_SWC,
   HL_NIGFS,HL_BTS,HL_PT, HL_SP_NORTH, HL_SP_PORC, HL_SP_ARSA)


# Add unique Hauls ID fields to each of the datasets to allow combination and changing
# This takes the structure Survey/YEAR/Quarter/Ship/HaulNo/Gear
##################
HH$UniqueID<-paste(HH$Survey,HH$Year,HH$Quarter,HH$Ship, 
                   HH$HaulNo, HH$Gear, sep="/")
HL$UniqueID<-paste(HL$Survey,HL$Year,HL$Quarter,HL$Ship, 
                   HL$HaulNo, HL$Gear, sep="/")


### add ship_old field to make reference to previous codes easier. -June 2021

Ship_match <- read.csv("Raw_data/ShipC_TS_Ship.csv")

head(Ship_match)

HH$Ship_old <- Ship_match$TS_Ship[match(HH$Ship,Ship_match$ShipC)]
HL$Ship_old <- Ship_match$TS_Ship[match(HL$Ship,Ship_match$ShipC)]
    
head(HH)                              
head(HL)    

#####################
# Change -9's to NA #
####################
# All -9 should be NA as -9 represents a missing value in DATRAS
# Funtion to replace multiple -9 across lots of data tables
replace_function=function(DT){
  cnames <- colnames(DT)
  for(cname in cnames) {
    set(DT, j = cname, value = gsub("[[:space:]]", "", DT[[cname]]))
  }
  for(cname in cnames){
    set(DT, i = which(DT[[cname]] == -9), j = cname, value = NA)
  }
}
system.time(replace_function(HH))
system.time(replace_function(HL))

####################################
## Sort out data frame structure HH#
####################################
cnames <- colnames(HH)
for(cname in cnames) {
  set(HH, j = cname, value = gsub("[[:space:]]", "", HH[[cname]]))
}
str(HH)

names(HH)

# Change appropriate cols to numeric and have data tables set up properlly
numCols <- c("SweepLngt", "HaulNo", "HaulDur", "ShootLat",
             "ShootLong", "HaulLat", "HaulLong", "Depth", 
             "Netopening",  "Distance", "Warplngt", "Warpdia", "WarpDen",
             "DoorSurface", "DoorWgt","DoorSpread", "WingSpread",
             "Buoyancy", "KiteDim", "WgtGroundRope", "TowDir", 
             "GroundSpeed" , "SpeedWater", "SurCurDir", "SurCurSpeed",
             "BotCurDir", "BotCurSpeed", "WindDir", "WindSpeed",
             "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal",
             "ThermoCline", "ThClineDepth", "DateofCalculation")
HH[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]

####################################
## Sort out data frame structure HL#
####################################
# Change appropriate cols to numeric and have data tables set up properlly
cnames <- colnames(HL)
for(cname in cnames) {
  set(HL, j = cname, value = gsub("[[:space:]]", "", HL[[cname]]))
}
str(HL)
names(HL)
names(HL)[29] <- "ValidAphiaID" ### in previous versions this is what the column seems to have been called (RK0)

numCols <- c("SweepLngt","HaulNo","Year", "SpecCode","SpecVal",
             "TotalNo","CatIdentifier","NoMeas","SubFactor", "SubWgt",
             "CatCatchWgt","LngtClass", "HLNoAtLngt","ValidAphiaID")

#check - should return 0
setdiff(numCols, names(HL))

HL[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols] ### 

## if the above doesn't run, you can force it to inelegantly like so ;)
# HL$SweepLngt <- as.numeric(HL$SweepLngt)
# HL$HaulNo <- as.numeric(HL$HaulNo)
# HL$Year <- as.numeric(HL$Year)
# HL$SpecCode <- as.numeric(HL$SpecCode)
# HL$SpecVal<- as.numeric(HL$SpecVal)
# HL$TotalNo <- as.numeric(HL$TotalNo)
# HL$CatIdentifier <- as.numeric(HL$CatIdentifier)
# HL$NoMeas <- as.numeric(HL$NoMeas)
# HL$SubFactor <- as.numeric(HL$SubFactor)
# HL$SubWgt <- as.numeric(HL$SubWgt)
# HL$CatCatchWgt <- as.numeric(HL$CatCatchWgt)
# HL$LngtClass <- as.numeric(HL$LngtClass)
# HL$HLNoAtLngt <- as.numeric(HL$HLNoAtLngt)
# HL$ValidAphiaID <- as.numeric(HL$ValidAphiaID)


# all offending -9 should now be gone from all my data tables
# check summarys and insure the data sturcture is sound


str(HH)
str(HL)

summary(HH)

### one record with crazy negative distance - set to NA 
# HH$Distance[which(HH$Distance ==-16668)] <- NA

summary(HL)

# if all is good- move on, if not rerun numeric col stuff again after applying the
# NA replace_function as this might mess with the structure.

### all -9's are gone! Hooray! but some odd 0's in the  measures.. deal with in later scripts..
