###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 4 of X 
# The purpose of this script is to check the merged datasets and insure all changes  
# mentioned in the error trapping documentation are transfered into the data product
#
# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: Nicola Walker (Cefas) Jonas Hentati Sundberg (SLU)
# Edits by : Ruth Kelly (AFBI) for June 2021 reruns  - R version 4.0.4 (2021-02-15) -- "Lost Library Book"
#################################################
## Add in the Corrections sent directly to me ###
#################################################
#############################
# Northern Ireland Additions#
#############################
# The first step in the correction procedure is to add in any missing data
# these are the datasets that were stored loaded up during script 2_Loading_data

# Correction 1: Northern Ireland Data 1992 - 2007
# this data is not currently available on DATRAS
head(NI_extra)
# change -9 to NA
# First split HH and HL



NI_HH<-subset(NI_extra, V1=="HH", )

NI_HL<-subset(NI_extra, V1=="HL", )


# Sort out the Col headings
names(NI_HH)<-c("RecordType", "Quarter", "Country" , "Ship" , "Gear" , 
                "SweepLngt", "GearEx", "DoorType", "StNo" , "HaulNo" , 
                "Year", "Month", "Day", "TimeShot", "DepthStratum" ,"HaulDur",
                "DayNight", "ShootLat", "ShootLong","HaulLat", "HaulLong",
                "StatRec", "Depth","HaulVal","HydroStNo","StdSpecRecCode",
                "BySpecRecCode","DataType", "Netopening","Rigging","Tickler",
                "Distance","Warplngt","Warpdia","WarpDen","DoorSurface","DoorWgt",
                "DoorSpread","WingSpread","Buoyancy","KiteDim","WgtGroundRope","TowDir",
                "GroundSpeed","SpeedWater","SurCurDir","SurCurSpeed","BotCurDir",
                "BotCurSpeed","WindDir","WindSpeed","SwellDir","SwellHeight","SurTemp",
                "BotTemp","SurSal","BotSal","ThermoCline", "ThClineDepth")

NI_HH$Survey<-"NIGFS"
NI_HH$DateofCalculation<-"NA"


summary(NI_HH)

# Haul Durations recorded as decimal hour - change to mins (*60)
head(NI_HH$HaulDur)
NI_HH$HaulDur<-NI_HH$HaulDur*60

# Now look at HL file
summary(NI_HL)
names(NI_HL)<-c("RecordType", "Quarter","Country",          
                "Ship", "Gear","SweepLngt", "GearEx",         
                "DoorType", "StNo","HaulNo","Year",        
                "SpecCodeType","SpecCode","SpecVal","Sex",              
                "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
                "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
                "HLNoAtLngt")
keepers<-c("RecordType", "Quarter","Country",          
           "Ship", "Gear","SweepLngt", "GearEx",         
           "DoorType", "StNo","HaulNo","Year",        
           "SpecCodeType","SpecCode","SpecVal","Sex",              
           "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
           "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
           "HLNoAtLngt")
NI_HL<-NI_HL[keepers]
NI_HL$Survey<-"NIGFS"
NI_HL$DateofCalculation<-"NA"
NI_HL$ValidAphiaID<-NI_HL$SpecCode 


summary(NI_HH$TimeShot)
head(NI_HH$TimeShot)

table(NI_HH$Ship, NI_HH$Year)

# Add unique ID code
NI_HH$UniqueID<-paste(NI_HH$Survey, NI_HH$Year,NI_HH$Quarter, 
                      NI_HH$Ship, NI_HH$HaulNo, NI_HH$Gear, sep="/")
NI_HL$UniqueID<-paste(NI_HL$Survey, NI_HL$Year, NI_HL$Quarter, 
                      NI_HL$Ship, NI_HL$HaulNo, NI_HL$Gear, sep="/")

###
NI_HH$Ship_old <- NI_HH$Ship
NI_HL$Ship_old <- NI_HL$Ship

unique(NI_HH$TimeShot)

# NI_HH$NewUniqueID3<-paste(NI_HH$Survey,NI_HH$Year, NI_HH$Quarter,NI_HH$Ship, 
#                         NI_HH$HaulNo, NI_HH$Gear, NI_HH$StNo, NI_HH$Country,
#                         sep="/")
# 
# NI_HL$NewUniqueID3<-paste(NI_HL$Survey,NI_HL$Year, NI_HL$Quarter,NI_HL$Ship, 
#                           NI_HL$HaulNo, NI_HL$Gear, NI_HL$StNo, NI_HL$Country,
#                           sep="/")

# these data will be added to the product

# setdiff(NI_HH$NewUniqueID3, NI_HL$NewUniqueID3) ## headers with no HL data ## should be just 1 - "NIGFS/2001/4/LF/36/ROT/94/NI"
# setdiff(NI_HL$NewUniqueID3,NI_HH$NewUniqueID3) ## nothing in NI that's not in Datras data



HH1<-rbind(HH, NI_HH,  fill=TRUE)



### Co is the corystes vessel code should be 
HH1$Ship[HH1$Ship == "CO"] <- "74RY" 
HH1$Ship[HH1$Ship == "7.4e+10"] <- "74E9"
HH1$Ship[HH1$Ship == "LF"] <- "74LG"

unique(HH1$Ship)


HH1$UniqueID<-paste(HH1$Survey,HH1$Year,HH1$Quarter,HH1$Ship, 
                    HH1$HaulNo, HH1$Gear, sep="/")

## next HL

setdiff(names(HL), names(NI_HL)) ## new fields in Datras not in NI
setdiff(names(NI_HL),names(HL)) ## nothing in NI that's not in Datras data

head(NI_HL)

HL1<-rbind(HL,NI_HL, fill=TRUE)

table(HL1$Ship)

### Co is the corystes vessel code should be 
HL1$Ship[HL1$Ship == "CO"] <- "74RY" 
HL1$Ship[HL1$Ship == "7.4e+10"] <- "74E9"
HL1$Ship[HL1$Ship == "LF"] <- "74LG"

HL1$UniqueID<-paste(HL1$Survey,HL1$Year,HL1$Quarter,HL1$Ship, 
                   HL1$HaulNo, HL1$Gear, sep="/")

# remove intermediate datasets
rm(NI_extra, NI_HH, NI_HL, keepers)




######################################
## DENMARK IBTS CORRECTIONs/Additions#
######################################
# The third addition came from Denmark for the earlier IBTS 
# data for 1983, 1984, 1995, 1986
NS_DEN<-rbind(NS_DEN_sp_1986,NS_DEN_sp_1985,
              NS_DEN_sp_1984,NS_DEN_sp_1983, fill=TRUE)

NS_DEN_add_HH<-subset(NS_DEN, V1=="HH",
                      )
NS_DEN_add_HL<-subset(NS_DEN, V1=="HL", 
)

summary(NS_DEN_add_HL)
# remove the intermediate datasets
rm(NS_DEN_sp_1983, NS_DEN_sp_1984, NS_DEN_sp_1985, NS_DEN_sp_1986)
# Sort out the Col headings
names(NS_DEN_add_HH)<-c("RecordType", "Quarter", "Country" , "Ship" , "Gear" , 
                        "SweepLngt", "GearEx",  "DoorType", "StNo" , "HaulNo" , "Year",
                        "Month"    ,         "Day"           ,    "TimeShot"     ,     "DepthStratum"          ,
                        "HaulDur"   ,        "DayNight"      ,    "ShootLat"      ,    "ShootLong"        ,
                        "HaulLat"    ,       "HaulLong"      ,    "StatRec"        ,   "Depth"            ,
                        "HaulVal"     ,      "HydroStNo"     ,    "StdSpecRecCode" ,   "BySpecRecCode"   ,
                        "DataType"     ,     "Netopening"    ,    "Rigging"         ,  "Tickler"          ,
                        "Distance"      ,    "Warplngt"      ,    "Warpdia"        ,   "WarpDen"          ,
                        "DoorSurface"    ,   "DoorWgt"       ,    "DoorSpread"     ,   "WingSpread"       ,
                        "Buoyancy"        ,  "KiteDim"       ,    "WgtGroundRope"  ,   "TowDir"           ,
                        "GroundSpeed"      , "SpeedWater"    ,    "SurCurDir"      ,   "SurCurSpeed"      ,
                        "BotCurDir"         ,"BotCurSpeed"     ,  "WindDir"        ,   "WindSpeed"        ,
                        "SwellDir",         "SwellHeight"    ,   "SurTemp"        ,   "BotTemp"          ,
                        "SurSal",          "BotSal"        ,    "ThermoCline"    ,   "ThClineDepth"     )

NS_DEN_add_HH$Survey<-"NS-IBTS"
NS_DEN_add_HH$DateofCalculation<-"NA"
names(NS_DEN_add_HH)

NS_DEN_add_HH$Ship_old <- "DAN2"
NS_DEN_add_HH$Ship <-"26D4"



setdiff(names(HH), names(NS_DEN_add_HH)) ## new fields in Datras
setdiff(names(NS_DEN_add_HH),names(HH)) ## nothing that's not in Datras data



names(NS_DEN_add_HL)<-c("RecordType", "Quarter","Country",          
                        "Ship", "Gear","SweepLngt", "GearEx",         
                        "DoorType", "StNo","HaulNo","Year",        
                        "SpecCodeType","SpecCode","SpecVal","Sex",              
                        "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
                        "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
                        "HLNoAtLngt","x1", "x1", "x1", "x1", "x1", "x1", 
                        "x1", "x1","x1", "x1", "x1", "x1","x1", "x1", "x1", "x1","x1", "x1",
                        "x1", "x1","x1", "x1", "x1", "x1","x1", "x1", "x1", "x1","x1", "x1", 
                        "x1", "x1", "x1", "x1", "x1")

NS_DEN_add_HL$Survey<-"NS-IBTS"
NS_DEN_add_HL$DateofCalculation<- "NA"
NS_DEN_add_HL$ValidAphiaID<- NS_DEN_add_HL$SpecCode 
#NS_DEN_add_HL$x1<-NULL # repeat code until all x1 cols are gone
NS_DEN_add_HL <- NS_DEN_add_HL[,which(names(NS_DEN_add_HL) != "x1")]

NS_DEN_add_HL$Ship_old <- NS_DEN_add_HL$Ship
NS_DEN_add_HL$Ship <- "26D4"

# Add unique ID code
NS_DEN_add_HH$UniqueID<-paste(NS_DEN_add_HH$Survey,
                              NS_DEN_add_HH$Year,NS_DEN_add_HH$Quarter, 
                              NS_DEN_add_HH$Ship, NS_DEN_add_HH$StNo, 
                              NS_DEN_add_HH$Gear, sep="/")
NS_DEN_add_HL$UniqueID<-paste(NS_DEN_add_HL$Survey,
                              NS_DEN_add_HL$Year,NS_DEN_add_HL$Quarter, 
                              NS_DEN_add_HL$Ship, NS_DEN_add_HL$StNo, 
                              NS_DEN_add_HL$Gear, sep="/")
unique(HH1$Survey)
unique(HH1$Country)

Den_ibts<-subset(HH1, Survey=="NS-IBTS"&Country=="DK"&Year<1987&Year>1982,)
# 168 observations
# NS_Den_add_HH has only got 168 obs
remove<-Den_ibts$UniqueID
# # lets check if they are different
replace_function(NS_DEN_add_HH)
test<-compare_function(Den_ibts, NS_DEN_add_HH)
test1<-compare_function(NS_DEN_add_HH, Den_ibts)
# in the file directly from Denmark there are problems with the distance and
# and lat long measurements so stick with the DATRAS version of the haul Chron File
# lets look at the HL file too
Den_ibts_fish<-subset(HL1, UniqueID%in%remove,)
# 12356 obs in Datras file
# 13822 obs in denmarks file
# lest see haw they differ
replace_function(NS_DEN_add_HL)
names(NS_DEN_add_HL)
test<-compare_function(Den_ibts_fish, NS_DEN_add_HL)
test1<-compare_function(NS_DEN_add_HL, Den_ibts_fish)
# clear differences in SpecCodeType, SpecCOde, SubWght, LenghtCode/class
# remove these to see if the spp list of additiona spp shines through

copy1<-subset(Den_ibts_fish,
                 select=c(StNo,  Year, UniqueID))
copy_den<-subset(NS_DEN_add_HL,
                 select=c(StNo, Year, UniqueID))
test<-compare_function(copy_den, copy1)
test1<-compare_function(copy1, copy_den)
# nightmare data should only have extra data in the test, not the test1,
# all stations and unique ID StNo relapces Haul No in the DEN_add 
# datasets. They are the same except for Unique ID NS-IBTS/1986/1/DAN2/30/GOV which 
# is only in the DATRAS copy not the Denmark copy 
# neither copy is perfect - retain DATRAS
# then find the records of species that are missing from DATRAS 
names(Den_ibts_fish)
copy1<-subset(Den_ibts_fish,
                 select=c(StNo,  Year, UniqueID, ValidAphiaID))
copy_den<-subset(NS_DEN_add_HL,
                 select=c(StNo, Year, UniqueID, ValidAphiaID))
test<-compare_function(copy_den, copy1)
test1<-compare_function(copy1, copy_den)
# 637 records in denmark not in DATRAS - this makes sense
# 198 records in DATRAS not in denmark!?!
# complete mis-match
summary(as.factor(test$Year))
summary(as.factor(test1$Year))
summary(as.factor(test$ValidAphiaID))

# 56 spp missing from records
summary(as.factor(test1$ValidAphiaID))

setdiff(Den_ibts_fish$ValidAphiaID, NS_DEN_add_HL$ValidAphiaID)
# 4 in Datras not in supplement

length(setdiff( NS_DEN_add_HL$ValidAphiaID, Den_ibts_fish$ValidAphiaID))
# 26 in supplement not on Datras

# total mess!
# Gonna add in the missing records in the Denmark files and flag them
Den_ibts_fish$match<-paste(Den_ibts_fish$UniqueID,Den_ibts_fish$ValidAphiaID, sep="/")
NS_DEN_add_HL$match<-paste(NS_DEN_add_HL$UniqueID,NS_DEN_add_HL$ValidAphiaID, sep="/")
list<-Den_ibts_fish$match
Den_HL_additions<-subset(NS_DEN_add_HL, match%in%list)

# these are my additions for the HL-gov file
# leave the match field as a flag for now - issues with these data that 
# remain unresolved
HL2<-rbind(HL1, Den_HL_additions, fill=TRUE)
summary(as.factor(HL2$match))

#remove intermediate datasets
rm(Den_HL_additions, Den_ibts,Den_ibts_fish, test, copy_den, copy1)
rm(test1, remove, list)
rm(NS_DEN, NS_DEN_add_HL, NS_DEN_add_HH)
rm(HH, HL, HL1)

### check country codes

table(HH1$Survey, HH1$Country, useNA = "ifany")

HH1$Country[HH1$Country == "" & HH1$Survey == "SP-PORC"] <- "ES"

HH1[is.na(HH1$Country),] ## Ship code AA36 also refers to unspecified vessel ? Remove?


### add old IDs for reference.. 2021

HH1$UniqueID<-paste(HH1$Survey,HH1$Year,HH1$Quarter,HH1$Ship, 
                    HH1$HaulNo, HH1$Gear, sep="/")

HH1$UniqueIDP<-paste(HH1$Survey,HH1$Year,HH1$Quarter,HH1$Ship_old, 
                    HH1$HaulNo, HH1$Gear, sep="/")

#####################################
## Appendix Error Trapping Q and A #
#####################################
# To check that each error/question raised is addressed, everything listed in the
# Appendix must be rechecked and verified as corrected at source(already on DATRAS)
# or alternatively fixed here with a line of code
#########################################################################
# 1.1.1	The First Quarter International Bottom Trawl Survey (GNSIntOT1) #
#########################################################################
#########################
### NORWAY NS-IBTS Q1 ###
#########################
summary(as.factor(HH1$Country))

unique(HH1$Country)


# remove country DUM
# Not standard survey
HH1<-HH1[Country!="DUM"]
# Question 1: 179m doorspread at depth of 73m. 
# Is it a true value or a mistake?
# JD: Most likely was the sensors reading incorrectly. 
# Value checked with trawl readings, was NA (bad recording). 
# Should be replaced using this formula: 
# DS 110m sweeps = 55.7 + 0.56 * depth + -0.001 * depth2
# MM: I will remove the incorrect value and estimate using mixed model
# for consistency with other estimated values
list<-c("NS-IBTS/2013/1/58G2/22/GOV", "NS-IBTS/2012/3/JHJ/299/GOV")
HH1$DoorSpread[HH1$UniqueIDP%in%list]#<-NA

### these values appear to have been changed in the raw data, code disabled above RK

# Question 2; Ship is called â€œ58G2â€ in Q1 â€“ Is this the 
# correct ship name or should this be GO Stars (GOS)?
# This is the international ship code for GO Sars.
# MM change GOS to 58G2

# list<-HH1$Ship%in%"GOS"
# HH1$Shipchanged[HH1$Ship%in%"GOS"]<-"GOS changed to 58G2"
# HH1$Ship[HH1$Ship%in%"GOS"]<-"58G2"

# Station inconsistent with other areas sampled in that year, 
# is this the correct Latitude and Longitude. 
# JD: it is possible this was incorrectly recorded and should 
# be 57.24.00 instead.
HH1$ShootLat[HH1$UniqueID=="NS-IBTS/1986/1/58EJ/41/GOV"] <-57.24


# 1994/1/GOS/26/GOV Station is inconsistent with sampled area,
# is this station correct?
# JD:This haul should be in ICES rectangle 43F7
check<-HH1[HH1$UniqueID=="NS-IBTS/1994/1/58G2/26/GOV"]

check

# looks to have been changed online.
##############################
# Check: Scotland NS-IBTS Q1 #
##############################
# check that uploads have translated onto DATRAS
sco<-subset(HH1, HH1$Country=="GB-SCT"&HH1$Year==1991&HH1$Survey=="NS-IBTS",)
plot(sco$Depth, sco$DoorSpread, pch=21, col="grey")

#### These look okay now. RK 2021

# solution from F.B to remove these values from the 
# existing DATRAS upload and replace with -9 (NA).
# list<-c('NS-IBTS/1991/1/SCO2/38/GOV',	'NS-IBTS/1991/1/SCO2/60/GOV',	
#         'NS-IBTS/1991/1/SCO2/43/GOV', 'NS-IBTS/1991/1/SCO2/36/GOV',	
#         'NS-IBTS/1991/1/SCO2/37/GOV',	'NS-IBTS/1991/1/SCO2/46/GOV',
#         'NS-IBTS/1991/1/SCO2/40/GOV',	'NS-IBTS/1991/1/SCO2/39/GOV',	
#         'NS-IBTS/1991/1/SCO2/42/GOV', 'NS-IBTS/1991/1/SCO2/45/GOV',	
#         'NS-IBTS/1991/1/SCO2/41/GOV',	'NS-IBTS/1991/1/SCO2/44/GOV')
# HH1$DoorSpread[HH1$UniqueID%in%list]<-NA

# check that uploads have translated onto DATRAS
sco<-subset(HH1, HH1$Country=="GB-SCT"&HH1$Year==2000&HH1$Survey=="NS-IBTS",)
plot(sco$Depth, sco$DoorSpread, pch=21, col="grey")
# outliers removed
###########################
# Check: EnglandNS-IBTS Q1#
###########################
# The following stations have outlying net opening values, 
# can you verify that these outliers are true values (figure 1.1.1.13/14).
# response was to use all but one - changed below
HH1$Netopening[HH1$UniqueID=="NS-IBTS/1998/3/CIR/66/GOV"] #<-NA
HH1$Netopening[HH1$UniqueID=="NS-IBTS/1998/3/74CZ/66/GOV"]  <- NA


###################### 
# Denmark NS-IBTS Q1 #
######################
# In figure 1.1.1.15 can you explain why the sweep length 
# varies so much in the early 1990’s? 
# check has this been removed
plot(HH1$Depth[HH1$Country=="DK"], HH1$SweepLngt[HH1$Country=="DK"],
     pch=19)

# warp values are still recorded in place of sweeps
# great craic, lets fix this
den<-HH1[HH1$Country=="DK"& HH1$SweepLngt>111,]
summary(as.factor(den$Year))
# gotta catch them all - range from 1991 to 1995 425obs
# want to move the warp lengths to the right spot too

list<-den$UniqueID
HH1$Warplngt[HH1$UniqueID%in%list]<-HH1$SweepLngt[HH1$UniqueID%in%list]
HH1$SweepLngt[HH1$UniqueID%in%list]<-NA

# Question: In 1998 the doorspread is have quite large values 
# in depth range of 74m to 137m. Are these values the same as in 
# your dataset, and do you know why this has occurred?
# KW: Incorrect data make -9 (NA)


# list<- c( 'NS-IBTS/1998/1/DAN2/14/GOV','NS-IBTS/1998/1/DAN2/15/GOV',
#           'NS-IBTS/1998/1/DAN2/16/GOV','NS-IBTS/1998/1/DAN2/46/GOV',
#           'NS-IBTS/1998/1/DAN2/9/GOV','NS-IBTS/1998/1/DAN2/13/GOV',
#           'NS-IBTS/1998/3/DAN2/24/GOV','NS-IBTS/1998/3/DAN2/49/GOV',
#           'NS-IBTS/1998/3/DAN2/45/GOV','NS-IBTS/1998/3/DAN2/48/GOV')


list<- c( 'NS-IBTS/1998/1/26D4/14/GOV','NS-IBTS/1998/1/26D4/15/GOV',
          'NS-IBTS/1998/1/26D4/16/GOV','NS-IBTS/1998/1/26D4/46/GOV',
          'NS-IBTS/1998/1/26D4/9/GOV','NS-IBTS/1998/1/26D4/13/GOV',
          'NS-IBTS/1998/3/26D4/24/GOV','NS-IBTS/1998/3/26D4/49/GOV',
          'NS-IBTS/1998/3/26D4/45/GOV','NS-IBTS/1998/3/26D4/48/GOV')

HH1$DoorSpread[HH1$UniqueID%in%list]<-NA


# outliers removed

############################
# Netherlands IBTS Q1      #
############################
# For station with unique ID 2004/1/TRI2/33/GOV the door spread 
# is 118m at a depth of 64m. Is the door spread value correct?
# IdB: We agree it is an outlier, but we cannot find the hard 
# copies for hauls 1-38 in 2004 so we’re not able to check
# Decision - remove outlier
HH1$DoorSpread[HH1$UniqueID=="NS-IBTS/2004/1/64T2/33/GOV"] <-NA

# Question 1: 1984/1/TRI/4/GOV
# NetOpening is 8m at depth of 78m. This is an outlier, 
# but is it a possible value?	
# IdB: Change into 6, the hard copy is not too clear 
HH1$Netopening[HH1$UniqueID=="NS-IBTS/1984/1/64T0/4/GOV"] <-6

####################### 
### GERMANY NS-IBTS ###
####################### 
# Question: sweep of 80m for H18 gear 
#
funnysweep<-subset(HH1, c(HH1$Country=="DE"& HH1$Survey=="NS-IBTS"& Year=="1984" & Gear == "H18"),)
# no need to worry as H18 gear will be removed in the standard gear type.
funnysweep$UniqueID
HH1$SweepLngt[HH1$UniqueID%in%funnysweep$UniqueID]<-NA

# Question: Doorspread is not consistent with depth, 
# can you verify that these outliers are the correct values?
# MK: Failures in the scanmar 
list<-c("NS-IBTS/2011/1/06NI/14/GOV", "NS-IBTS/2014/1/06NI/11/GOV", 
        "NS-IBTS/2014/1/06NI/7/GOV", "NS-IBTS/2014/1/06NI/8/GOV")
HH1$DoorSpread[HH1$UniqueID%in%list]<-NA

# Question: Why was the ship SOL used in Q1, 1992, 
# and why was the area surveyed so different in this year?        
# MK: SOL shouldn't be here. GOV is non standard and the data
# shouldn't be included in DATRAS. 

table(HH1$Ship[HH1$Country == "DE" & HH1$Year == 1992])

delete_ship<-subset(HH1, HH1$Year=="1992"&
                      HH1$Country=="DE"
                    & HH1$Ship=="06S1",)
list<-(delete_ship$UniqueID)

HH1<-subset(HH1, !HH1$UniqueID%in%list, )


####################
#Sweden NS-IBTS Q1 # 
####################
plot(HH1$Depth[HH1$Country=="SE"],HH1$WingSpread[HH1$Country=="SE"], pch=21, col="grey")
# outlier not removed
# 2021 - plot looks okay, outlier may have been removed on Datras?
HH1$WingSpread[HH1$UniqueIDP=='NS-IBTS/2014/1/DANS/30/GOV']#<-NA
HH1$WingSpread[HH1$UniqueIDP=='NS-IBTS/2014/1/DANS/39/GOV']#<-NA

#####################
# France NS-IBTS Q1 #
#####################
#Wingspread is not consistent with depth, 
#can you verify that these outliers are true values. 
# YV: Values are too hight, delete and estimate based on model.
# Wingspread normally 15m 

# list<-c("NS-IBTS/1993/3/THA/53/GOV", "NS-IBTS/1995/1/THA/29/GOV")

hist(HH1$WingSpread[HH1$Ship_old == "THA"] )
which(HH1$WingSpread[HH1$Ship_old == "THA"] > 30)
## seems like only one of these is still present - 2021

# 1995 record changed to 16
# remove 1993 record only
HH1$WingSpread[HH1$UniqueIDP=="NS-IBTS/1995/1/THA/29/GOV"]<-16

# Netopening is not consistent with depth, 
# can you verify that these outliers are true values. 
# YV/FC: Incorrect use -9 
list<-c("NS-IBTS/1993/3/THA/9/GOV", "NS-IBTS/1994/1/THA/31/GOV")
HH1$Netopening[HH1$UniqueIDP%in%list] <- NA

# Doorspread is not consistent with depth, 
# can you verify that these outliers are true values. 
# YV/FC: Need to be recalculated
list<-c( 'NS-IBTS/1996/3/THA2/21/GOV','NS-IBTS/1996/3/THA2/20/GOV',
         'NS-IBTS/1997/1/THA2/1/GOV','NS-IBTS/1997/1/THA2/6/GOV',
         'NS-IBTS/1997/1/THA2/8/GOV','NS-IBTS/1997/1/THA2/41/GOV',
         'NS-IBTS/2000/1/THA2/56/GOV','NS-IBTS/2002/1/THA2/29/GOV',
         'NS-IBTS/2010/1/THA2/21/GOV','NS-IBTS/2010/1/THA2/26/GOV',
         'NS-IBTS/2010/1/THA2/29/GOV','NS-IBTS/2015/1/THA2/38/GOV')
HH1$DoorSpread[HH1$UniqueIDP%in%list] <- NA

# Depth of 61m netopening 9.8m. This is an outlier,
# can you check that the value is correct.
# YV/FC: Incorrect value
HH1$Netopening[HH1$UniqueIDP=='NS-IBTS/2011/1/THA2/74/GOV']<-NA
#########################################################################
# 1.1.2	The Third Quarter International Bottom Trawl Survey (GNSIntOT3) #
#########################################################################
#########################
### NORWAY NS-IBTS Q3 ###
#########################
# Question 3: haul duration
# JD: 2008/3/JHJ/259/GOV: this is not an IBTS haul. 
# Was made for another purpose (should be coded HaulVal=I).         
# 2008/3/JHJ/269/GOV: it looks like tow time was calculated 
# incorrectly. Distance =2.4 km, speed at 3.4 n mi = 6.297 km/hr: 
# 2.4/6.297*60 = 22.9 minutes
HH1$HaulVal[HH1$UniqueIDP=="NS-IBTS/2008/3/JHJ/259/GOV"]<-"I"
HH1$HaulDur[HH1$UniqueIDP=="NS-IBTS/2008/3/JHJ/269/GOV"]<-23
# One sample has a 60 min duration, is there a reason for this?
# JD: looks to be an error. Distance/speed*60 = 32.4 minutes
HH1$HaulDur[HH1$UniqueIDP=="NS-IBTS/1999/3/MIC/619/GOV"]<-32

# 2011/3/JHJ/242/GOV	Netopening is 8.8, at a depth of 64m, 
# this net opening is an outlier, is the value correct?	
# JD: Unlikely to be correct. No correction (NA).
# 1999/3/MIC/591/GOV	Netopeing is 7.7m depth 47m, this net opening
# is a meter more than the next largest value. 
# Is this an acceptable value for this ship?	
# Unlikely to be correct. No correction (NA).
list<-c("NS-IBTS/2011/3/JHJ/242/GOV", "NS-IBTS/1999/3/MIC/591/GOV")
HH1$Netopening[HH1$UniqueIDP%in%list] <- NA

#########################
## England NS-IBTS Q3 ##
#########################
# At station with unique ID 2000/3/CIR/17/GOV the shoot and haul lat
# and long are the same values, can you check this please?
# G.B  Original data available. Haul long Input incorrectly. Change posn
# Shot: 54.639; 5.501
# Haul: 54.641; 5.564
check<-HH1[HH1$UniqueIDP=="NS-IBTS/2000/3/CIR/17/GOV",]

# need to change all postional data
 HH1$ShootLat[HH1$UniqueIDP=="NS-IBTS/2000/3/CIR/17/GOV"]<-54.639
 HH1$ShootLong[HH1$UniqueIDP=="NS-IBTS/2000/3/CIR/17/GOV"]<-5.501
 HH1$HaulLat[HH1$UniqueIDP=="NS-IBTS/2000/3/CIR/17/GOV"]<-54.641
 HH1$HaulLong[HH1$UniqueIDP=="NS-IBTS/2000/3/CIR/17/GOV"]<-5.564

 #########################################################################
# 1.1.3	The Fourth Quarter French Channel Groundfish Survey  (GNSFraOT4)#
#########################################################################
# quick check from Appendix 1 Figure 1.1.3.3 which highlights a haul duration of
# 90 minutes in the haul with unique ID 1995/4/GWD/49/GOV. Is this correct?
# Y.V & F.C:  This should be 30 mins not 90 mins.
# haul not corrected

 HH1$HaulDur[HH1$UniqueIDP=="FR-CGFS/1995/4/GWD/49/GOV"]<-30

# Figure 1.1.3.4, shows stations in 2012 which have been recorded with
# shoot positions on land. What is the correct shoot positions?
# Y.V & F.C:  This is clearly an error; I will check the real values and 
# send them to you.
plot(HH1$ShootLong[HH1$Survey=="FR-CGFS"], 
     HH1$ShootLat[HH1$Survey=="FR-CGFS"],
     pch=19)
abline(h=49.5, col="grey")
abline(h=50, col="grey")
abline(h=50.5, col="grey")
abline(h=51, col="grey")
abline(v=-1, col="grey")
abline(v=0, col="grey")
abline(v=1, col="grey")
abline(v=2, col="grey")

# check Stat Rec is right?
landlocked<-HH1[HH1$StatRec=="27F0",]
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/49/GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/49/GOV"],
       pch=19, col="red")
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/50/GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/50/GOV"],
       pch=19, col="red")
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/51/GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/51/GOV"],
       pch=19, col="red")
landlocked<-HH1[HH1$StatRec=="28F0",]
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/52/GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/52/GOV"],
       pch=19, col="red")
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/53/GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS/2012/4/GWD/53/GOV"],
       pch=19, col="red")
list<-c("FR-CGFS/2012/4/GWD/49/GOV", "FR-CGFS/2012/4/GWD/50/GOV",
        "FR-CGFS/2012/4/GWD/51/GOV", "FR-CGFS/2012/4/GWD/52/GOV",
        "FR-CGFS/2012/4/GWD/53/GOV")
HH1$StatRec[HH1$UniqueIDP%in%list]<-c("27E9", "27E9", "28E9", "28E9" ,"28E9")
# find stations on land
HH1$HaulLong[HH1$UniqueIDP%in%list]<-as.numeric(HH1$HaulLong[HH1$UniqueIDP%in%list])*-1
HH1$ShootLong[HH1$UniqueIDP%in%list]<-as.numeric(HH1$ShootLong[HH1$UniqueIDP%in%list])*-1

#######################
# 1.2 The Celtic Seas #
#######################
##############################################################
# 1.2.1 The First Quarter Scottish West Coast IBTS (CSScoOT1)#
##############################################################
# In figure 1.2.1.2 there are a few door spread values that look like 
# outliers, can you check them please.
# Unique ID: 2000/1/SCO3/54/GOV: Depth 90m, Doorspread 113m
# F.B: (this was 84 in FMD so will reupload to DATRAS) 
# CHECK HAS CHANGE OCCURED ON DATRAS
check<-HH1[HH1$UniqueIDP=="SWC-IBTS/2000/1/SCO3/54/GOV"]
# doorspread changed at source
# In figure 1.2.1.4 Warp length has got a generally linear 
# relationship with depth. Can you check if the outlying values likely to 
# be correct?
# F.B: See table 1.2.1.1 below, erroneous values in red but many of the others
# I have left as they are within what we may consider reasonable.
# Check changes
plot(HH1$Depth[HH1$Survey=="SCOWCGFS"], HH1$Warplngt[HH1$Survey=="SCOWCGFS"],)
# points(430, 450, col="red", pch=16)
# points(550,180, col="red", pch=16)
check<-HH1[HH1$UniqueIDP=="SCOWCGFS/2012/4/SCO3/49/GOV",] ### appears to have been changed in datras RK
# the correction was that depth was 175m adnd warp was 550 - values mixed up
HH1$Depth[HH1$UniqueIDP=="SCOWCGFS/2012/4/SCO3/49/GOV"]#<-175
HH1$Warplngt[HH1$UniqueIDP=="SCOWCGFS/2012/4/SCO3/49/GOV"]#<-550
# station with unique ID SWC-IBTS/2009/4/SCO3/15/GOV
# check Depth is plausable
check<-HH1[HH1$UniqueIDP=="SWC-IBTS/2009/4/SCO3/15/GOV",]
# given the location of this sample the depth is likely to be incorrect 
# should be about 130 m depth which will make warp good
# make depth NA and retain warp
HH1$Depth[HH1$UniqueIDP=="SWC-IBTS/2009/4/SCO3/15/GOV"]#<-NA ### data changed on datras 2021

################################################################
# 1.2.3.	The Fourth Quarter Irish Groundfish Survey (CSIreOT4)#
################################################################
# remove duplicate hauls 
# no longer required - new data upload
# HH1<-subset(HH1, !Quater==3&Survey=="IE-IGFS",)
######################################################################
##1.2.4 The First Quarter Northern Irish Groundfish Survey (CSNIrOT1)#
######################################################################
# At the station with the unique ID 2010/1/COR/44/ROT the haul duration
# is recorded as 891 mins. Is this correct? 
# M.L: haul duration corrected to 61min.
# check<-HH1[HH1$UniqueIDP=="NIGFS/2010/1/COR/44/ROT",]
# check

hist(HH1$HaulDur[HH1$Survey == "NIGFS"]) ### this seems to have been corrected at source


# In figure 1.2.4.1 there are outliers in the following door spread values:
# •	Unique ID: 2008/1/COR/15/ROT, Depth 48 m Door spread 26.2 m
# •	Unique ID: 2009/1/COR/37/ROT, Depth 62 m Door spread 31.3 m
# Can you check these please?
# M.L: Unique ID: 2008/1/COR/15/ROT, Door spread corrected to 34.2 m 
# and Unique ID: 2009/1/COR/37/ROT, Door spread corrected to 38.3 m
HH1[HH1$UniqueIDP=="NIGFS/2009/1/CO/37/ROT",] ### #corrected at source -rk 2021
HH1[HH1$UniqueIDP=="NIGFS/2008/1/CO/15/ROT",]### #corrected at source -rk 2021


#######################################################################
# 1.2.5 The Fourth Quarter Northern Irish Groundfish Survey (CSNIrOT4)#
#######################################################################
# Country: Northern Ireland, Data Provider: Mathieu Lundy, File Type: HH
# At the station with unique ID 2014/4/COR/50/ROT no depth value has been 
# recorded, is this available? 
# M.L: Yes, the depth should be 39m.
HH1$Depth[HH1$UniqueIDP=="NIGFS/2014/4/COR/50/ROT"]

#######################################################################
# 1.3.5 The Fourth Quarter Portuguese Groundfish Survey (BBICPorOT4) ##
#######################################################################
# 2005/4/NOR/32/NCT Depth: 131m Warp length: 1500m. 
# Can you check if this outlier is the correct value. 
# C.C: Error this should be 500m 
HH1$Warplngt[HH1$UniqueIDP=="PT-IBTS/2005/4/NOR/32/NCT"]<-500
##################################################################
##  1.4.1 The Third Quarter Scottish Rockall Survey (WAScoOT3)  ##
##################################################################
# In figure 1.4.1.1 the station with the unique ID 2013/3/SCO3/17/GOV has a depth 
# of 195m and a door spread of 72m. Can you check if this outlier is correct please.
# F.B: This was for haul 307 and after having looked at the RADOS file for this 
# haul I have decided to remove the doors spread value as the unit was faulty 
# midway through and so cannot trust the reading which looked suspect from the 
# start. I will re-upload this survey to DATRAS.
check<-HH1[HH1$UniqueIDP=="ROCKALL/2013/3/SCO3/307/GOV",]

# In figure 1.4.1.3 at station with unique ID 1999/3/SCO3/43/GOV, the net opening
# is 2.7m and the depth is 234m, Can you check this outlier please. 
# F.B: Haul 446. With no RADOS file to interrogate and no wing spread reading
# it is very hard to know whether this was a case of overspreading of the net 
# (in which case it is genuine). I have removed and re-uploaded to DATRAS.
check<-HH1[HH1$UniqueIDP=="SCOROC/1999/3/SCO3/43/GOV",] ## gone from Datras 2021

# In figure 1.4.1.4 at station with unique ID 2011/3/SCO3/46/GOV, the warp length 
# is 480m at a depth of 480 m, is this correct?
# F.B: Haul 442, Depth had been incorrectly recorded as 480 and should been 150m.
# Has been corrected on FMD and have re-uploaded to DATRAS.
check<-HH1[HH1$UniqueIDP=="ROCKALL/2011/3/SCO3/46/GOV",]
##############################
## 1.2 The Beam Trawl Surveys#
##############################
#############################
# The Greater North Sea BTS #
#############################
#######################
# The Celtic Seas BTS #
#######################
cols<-rainbow(19)
summary(as.factor(HH1$Year[HH1$Ship=="74RY"]))
summary(as.factor(HH1$Country))
plot(HH1$ShootLong[HH1$Ship=="74RY"], HH1$ShootLat[HH1$Ship=="74RY"],
     col=cols[as.factor(HH1$Year[HH1$Ship=="74RY"])], pch=19)
# some beams not being used at this stage 
# Belgium survey not long enough in previous versions 6 years of data now added 
belgium<-subset(HH1, Country=="BE",)
# table(belgium$Year)
# write.csv(belgium, "samples_removed_time_series_not_sufficent.csv")
#HH1<-subset(HH1, !Country=="BEL",)

### Keep Belguim as more Data now on Datras - 2021

# the inshore survey which normally occured on the CAR was on the COR 
# on a couple of occasions, this needs to be removed - not a full survey series.
abline(v=c(-8:3), col="lightgrey")
abline(h=c(48:55), col="lightgrey")
abline(h=c(48.5:54.5), col="lightgrey")
# 6 ices rectangles have data that shouldn't be part of survey time series.
list<-c("28E5","28E6","29E6","29E7","30E6","30E7")
points(HH1$ShootLong[HH1$StatRec%in%list], 
       HH1$ShootLat[HH1$StatRec%in%list],
       col=cols[as.factor(HH1$Year[HH1$StatRec%in%list])], pch=19)
names(HH1)
summary(as.factor(HH1$Survey))

remove_stations<-subset(HH1, StatRec%in%list & Ship=="74RY" & Survey=="BTS",) ### this needs to be double checked, as time series for 
### "BTS-VII" is no longer separate from "BTS" - RK 2021

list<-(remove_stations$UniqueID)
HH2<-subset(HH1, !UniqueID%in%list,)
nrow(HH1)-nrow(HH2)
#removes 2 hauls from time series
plot(HH2$ShootLong, HH2$ShootLat,
     col=cols[as.factor(HH2$Ship)], pch=19)
# One point on land
find<-HH2[HH2$UniqueID=="BTS/2002/3/74RY/133/BT4A",] ### no longer in raw dataset - 2021
# Shoot lat shoul be 50 not 54 so that put the haul into the ices boxes
# we have just removed so delete haul
#HH2<-subset(HH2, !HH2$UniqueID=="BTS-VIIa/2002/3/COR/133/BT4A",)

######################
# Additional Tweaks ##
######################
# Ship CAR which is used on in the BTS isn't suitable as the biodiversity of fish 
# not adequately sampled - delete from product
summary(as.factor(HH2$Ship))
Car_ship<-subset(HH2, Ship=="74OJ",) ### no longer in the database - 2021
#HH2<-subset(HH2, !Ship=="CAR", )
#write.csv(Car_ship, "Samples_removed_unsuitable.csv")
# Ship SOL when used on in the IBTS isn't suitable as GOV was not standard 
SOL_ship<-subset(HH2, Ship=="06S1"&Year==1992,) ### No longer in database - 2021
# list<-SOL_ship$UniqueID
# hauls<-subset(HH2, !UniqueID%in%list, )
# write.csv(SOL_ship, "Samples_removed_unsuitable_boat.csv")

##############################################
# ALL HAUL CORRECTION CHECKED AND COMPELTED  # 
# NOW HAVE BEST AVAILABLE DATA ACCORDING TO  #
# NATIONAL DATA PROVIDERS ACROSS ALL SURVEYS #
##############################################

######################################################
# Check out all biological data to insure corrections#
# are in datras/fix for product only base on answers #
######################################################

HL2$UniqueIDP<-paste(HL2$Survey,HL2$Year,HL2$Quarter,HL2$Ship_old, 
                     HL2$HaulNo, HL2$Gear, sep="/")


####################################
## Appendix Error Trapping Q and A #
####################################
# To check that each error/question raised is addressed, everything 
# listed in the Appendix must be rechecked and verified as corrected at 
# source(already on DATRAS)
# or alternatively fixed here with a line of code
#########################################################################
# 1.1.1	The First Quarter International Bottom Trawl Survey (GNSIntOT1) #
#########################################################################
# Bathyraja brachyurops (Fowler, 1910) # AphiaID: 271509 
# mismapped  should be Raja brachyura (Lafont, 1871) # AphiaID: 367297  
HL2$ValidAphiaID[HL2$ValidAphiaID=="271509"]<-"367297"
# Scomber japonicus Houttuyn, 1782 # AphiaID: 127022 
# mis id of Scomber colias Gmelin, 1789 # AphiaID: 151174 
HL2$ValidAphiaID[HL2$ValidAphiaID=="127022"]<-"151174"
names(HL2)
summary(as.factor(HL2$ValidAphiaID))
#########################
### NORWAY NS-IBTS Q1 ###
#########################
check<-subset(HL2, ValidAphiaID==271564,)
##############################
# Check: Scotland NS-IBTS Q1 #
##############################
# Figure 1.1.1.X shows that at the station with the unique id 
# 2003/1/SCO3/46/GOV, Acentronura shows up, (Aphia Code 159441)
# this pygmy pipehorse is native to the Indian and Pacific oceans: 
# Don’t think this is found in the North Sea. Can you check this record is 
# correct please? And if so is there a photo to back up the entry?
check<-subset(HL2, ValidAphiaID==159441,)
#gone

# Figure 1.1.1.X shows 2 records of Bairds smoothhead’s (Aphia Code 126682, 
# species name Alepocephalus bairdii) this is a deep water species, 
# the Unique ID 2009/1/SCO3/19/GOV (red) and 2009/1/SCO3/31/GOV(blue). 
# This is not a North Sea species, can you check that this is the correct 
# species identification please. 
check<-subset(HL2, ValidAphiaID==126682,)

#### this is back!!! ? REMOVE 2021!
table(check$Year)

#gone
###########################
# Check: EnglandNS-IBTS Q1#
###########################
# Figure 1.1.1.40 shows a record of the gulper shark (Valid Aphid: 105899,
#Species: Centrophorus granulosus) station with unique ID 2006/3/END/106/GOV 
# The gulper shark is not traditionally found in North Sea, can you confirm
# this record please.
summary(as.factor(HL2$ValidAphiaID))
        
check<-subset(HL2, ValidAphiaID==105899,)
check
# still there as an observed only record!
###################### 
# Denmark NS-IBTS Q1 #
######################
# Three records with unique ID’s 2007/3/DAN2/19/GOV, 2008/3/DAN2/17/GOV
# and 2008/3/DAN2/20/GOV contain Sarpa salpa (figure 1.1.1.43).
# Sarpa salpa is outside native range, is there any photographs that can 
# verify this species identification? 

# K.W: Can’t confirm ID is correct. It is a possible error, no 
# photographs to confirm


# The broadnosed skate has not been previously recorded in this habitat 
# (figure 1.1.1.44), and no other country has recorded this species. 
# See records 2005/3/DAN2/33/GOV, 2006/3/DAN2/14/GOV, 2006/3/DAN2/14/GOV, 
# 2006/3/DAN2/18/GOV, 2011/3/DAN2/13/GOV, 2012/1/DAN2/16/GOV, 
# 2012/1/DAN2/10/GOV and 2012/3/DAN2/18/GOV. 
# This there any photographs to back up these species ID’s? 
# K.W: No confirmation but believe it is correct at least from 2009 onwards

# The following hauls contain species with SpecVal 0 – 
# if one species has invalid information how is the rest of the 
# information considered valid?
check<-subset(HL2, UniqueIDP=="2011/1/DAN2/16/GOV"&ValidAphiaID==127143,)
check<-subset(HL2, UniqueIDP=="2015/1/DAN2/18/GOV"&ValidAphiaID==126436,)
check<-subset(HL2, UniqueIDP=="2015/1/DAN2/22/GOV"&ValidAphiaID==126436,)
#gone
############################
# Netherlands IBTS Q1      #
############################
# If a fish is measures to a length code of “5” has it also been 
# measured to a length code of “1”?  5cm length classes are normally 
# taken for stomach content analyses "25" represents a fish 25-29cm long. 
# But has this fish also been measured using length code “1”, to the cm below?
# If so should all length code “5” be removed as they are duplicates of 
# effort?
# List of Valid Aphia Codes measured in the length code “5” by Netherlands
#•	105814 •	105820 •	105887 •	105883 •	126436 •	126555 •	105732
#•	105821 •	105822 •	105865 •	105865 •	105869 •	105874 •	105876
#•	105923 •	125546 •	126281 •	126281 •	126375 •	126437 •	126440
#•	126441 •	126447 •	126461 •	126484 •	126758 •	126977 •	127138
#•	127143 •	127149 •	127186 •	223866 •	367297 
# IdB: I assume length code=5 only occurs on older data and only applies to
# larger length classes. In those days in the Netherlands the larger fish
# (>= 60 cm?) was measured in 5 cm classes. 
# So it can well be (and should) that a species in a certain haul is 
# measured to the cm below (all fish < 60 cm) as well as to the 5 cm 
# below (>= 60 cm). 
# If they are not duplicated measurements is the mean length per class 
# an acceptable measurement? E.g. a fish in class 25-29 will be called a 
# 27.5cm fish. 
# IdB: I would say yes. That’s what I normally tend to do when turning 
# length measurements into weight by using a length-weight relationship;
# W=a*(length+0.5*measurementunit)b ; a and b being constant values per 
# species. 
# okay so all lenght codes can be standardised - i will do this all together

# From 2000-2015 See Table 2, Appendix 1: SpecVal 0, when a haul contains 
# invalid information on a given species. How does the rest of the haul 
# contain valid information if one species is listed as having invalid 
# information?
# IdB: These HL records contain ‘dummy’ fish –no numbers, no subfact, 
# specval=0. The records have been created for CA records to which no 
# length can be connected. We measure the fish twice: once for lf 
# distribution, once for the biological parameters. Sometimes a fish 
# is 30.1 cm and measured as 30 in the lf and for biological measurements 
# it is 29.9 cm. The 30 cm length class in HL does then not link up to the
# 29 cm class in CA. For your analysis: delete all records where specval=0
HL2<-subset(HL2, !SpecVal=="0", )
####################### 
### GERMANY NS-IBTS ###
####################### 
# Lmax
####################
#Sweden NS-IBTS Q1 # 
####################
# Lmax
#####################
# France NS-IBTS Q1 #
#####################
# Lmax

#########################################################################
# 1.1.3	The Fourth Quarter French Channel Groundfish Survey  (GNSFraOT4)#
#########################################################################

#######################
# 1.2 The Celtic Seas #
#######################
##############################################################
# 1.2.1 The First Quarter Scottish West Coast IBTS (CSScoOT1)#
##############################################################
# There is an issue with several fish exceeding the maximum length 
# recorded, can you check and verify these records. Again due to the size, 
# see “Lmax_issues_sco.xls” file.
# F.B: Cross checks completed and corrections made where required.

######################################################################
##1.2.4 The First Quarter Northern Irish Groundfish Survey (CSNIrOT1)#
######################################################################
# Lmax

#######################################################################
# 1.2.5 The Fourth Quarter Northern Irish Groundfish Survey (CSNIrOT4)#
#######################################################################
# Several rounding errors are found in the HL file where total number 
# of fish is not equal to sum of HL at length*Subfactor. Due to the size 
# of the file please see “EVHOE_Rounding_Errors.xls file. 
# M.S: Cross checks completed and corrections made where required.

#######################################################################
# 1.3.5 The Fourth Quarter Portuguese Groundfish Survey (BBICPorOT4) ##
#######################################################################

##################################################################
##  1.4.1 The Third Quarter Scottish Rockall Survey (WAScoOT3)  ##
##################################################################

##############################################
# ALL BIOLOGICAL CORRECTIONS ADDRESSED       # 
# NOW HAVE BEST AVAILABLE DATA ACCORDING TO  #
# NATIONAL DATA PROVIDERS ACROSS ALL SURVEYS #
##############################################
# Some extra things I've come across that needs fixing



