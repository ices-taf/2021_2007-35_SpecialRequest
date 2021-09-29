###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 5 of X
# The next step is to 'define' the surveys - after the genuine errors have been 
# checked/changes e.g. a haul recorded as 90mins but was actually 30mins would be
# deleted if this step occurred befor the checks are done.
# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: Nicola Walker (Cefas) Jonas Hentati Sundberg (SLU)
# Edits by: Ruth Kelly (AFBI) June 2021 for reruns  - R version 4.0.4 (2021-02-15) -- "Lost Library Book"

#################
# Select Columns#
#################
# manually remove the individual countries data sets and work on data combined by gear, 
# see script 3_merge_datasets for details on how this is done 
# check data table names

# 2 data tables 1 HH and 1 HL, 
# we will focus on HH for the moment
## check the structure of these data tables
# note numeric cols are correct

# Remove extra columns
# if(any(colnames(HH) == "V1")) HH[, ":=" (V1 = NULL)]

# Remove extra spaces from character strings
# cnames <- colnames(HH)
# for(cname in cnames) {
# set(HH, j = cname, value = gsub("[[:space:]]", "", HH[[cname]]))
# }
######################################
## 1.2 Overview of the M & A Process##
######################################
# In section 1.2  (Moriarty & Greenstreet, 2016)
# We show the processes undertaken to standardise/exclude hauls from the data set
# Is the haul valid? yes retain, no remove

invalidhauls<-subset(HH2, HaulVal=="I", ) # 1465 obs
write.csv(invalidhauls, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/invalid_removed.csv")

# Select only valid hauls 
# Select important columns
# I'm making a copy of the HH files here so that I retain the original file
# This means if I make a mistake I can easily go back to this and redo without
# needing to go back to scripts 1-3

HH_save <- HH2

HH_save$NewUniqueID2<-paste(HH_save$Survey,HH_save$Year,HH_save$Quarter,HH_save$Ship, 
                            HH_save$HaulNo, HH_save$Gear, HH_save$StNo, HH_save$Country,
                            sep="/")

names(HH2)
summary(as.factor(HH2$Month))
HH2$Month[is.na(HH2$Month)]<-HH2$month[is.na(HH2$Month)]
summary(as.numeric((HH2$TimeShot)))
summary(as.factor(HH2$HaulVal))
# For all Surveys 

hauls <- subset(HH2, HH2$HaulVal=='V',)
# check this has deleted all the Invalid hauls

summary(as.factor(hauls$HaulVal))
summary(as.numeric(hauls$TimeShot))
# we can uneversially drop cols that are not of use to us going forward
# RecordType is HH for all - delete
# other cols deleted: Warpdia, WarpDen, DoorSurface, DoorWgt, Buoyancy,
# KiteDim, WgtGroundRope, TowDir, SurCurDir, SurCurSpeed, BotCurDir, BotCurSpeed,
# WindDir, WindSpeed, SwellDir, SwellHeight, SurTemp, BotTemp, SurSal, BotSal,
# ThermoCline, ThClineDepth, DateofCalculation

keepers<-c("Survey","Quarter","Country", "Ship","Gear", "SweepLngt","GearEx",          
           "DoorType","StNo","HaulNo","Year","Month", "Day","TimeShot","DepthStratum",
           "HaulDur","DayNight","ShootLat","ShootLong", "HaulLat","HaulLong",
           "StatRec","Depth","HaulVal","StdSpecRecCode","BySpecRecCode",  
           "DataType","Netopening","Rigging","Tickler", "Distance","Warplngt",             
           "DoorSpread","WingSpread","GroundSpeed","SpeedWater","UniqueID", "UniqueIDP")
setdiff( keepers, names(hauls))
setdiff(names(hauls), keepers)

others <- colnames(hauls)[!colnames(hauls) %in% keepers]
hauls[, c(others):= NULL]

str(hauls)

### add more unique ID that includes the station number and country (2021)

hauls$NewUniqueID2<-paste(hauls$Survey,hauls$Year,hauls$Quarter,hauls$Ship, 
                          hauls$HaulNo, hauls$Gear, hauls$StNo, hauls$Country,
                          sep="/")

# for all surveys we will not look at any data prior to 1983
# delete all hauls prior to 1983
pre1983<-subset(hauls, Year<1983) # this removes 4623 hauls from the IBTS Q1
### I did this in an earlier step - RK 2021

#write.csv(pre1983, "./Diagnostics/Diagnostic_data/deleted_older_hauls.csv")
hauls<-subset(hauls, Year>1982, )
# Gears differ between regions and Surveys we retain several gear types 
# Delete:DHT   H18   VIN    HT   BOT   SOV   FOT   HOB   KAB   H12   GRT   ABD  CAR   
# No :    90    63     0     0     0     0     0     9     0     0    87   520  157            
summary(as.factor(hauls$Gear))

#table(hauls$Gear, hauls$Survey)

list<-c('GOV','NCT','ROT','BT8','BT4A','BT7', 'PORB','BAK') ### readded PORB and BAK used by Spain which were in previous product in 2016
non_standard_gear<-subset(hauls, !Gear%in%list,) ### 65056 hauls
table(non_standard_gear$Survey)
hauls <- subset(hauls, Gear%in%list, )
summary(hauls$Gear)
write.csv(non_standard_gear, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/deleted_non_standard_gear_hauls.csv")
# What is the standard tow duration
# Delete hauls that are outside of the standard time 

### EXCEPT FOR NIGFS hauls as all tows in database are concidered valid. 

hauls$HaulDur<-(as.numeric(hauls$HaulDur))
hist(hauls$HaulDur)
summary(hauls$HaulDur)
smallhauls<- hauls[hauls$HaulDur<12 & hauls$Survey !="NIGFS",]
table(smallhauls$Survey)

too_short <- which(hauls$HaulDur<12 & hauls$Survey !="NIGFS")

hauls <- hauls[-too_short,]

write.csv(smallhauls, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/deleted_too_short_hauls.csv")
largehauls<- hauls[HaulDur>67 & hauls$Survey !="NIGFS",]
table(largehauls$Survey)

too_long <- which(hauls$HaulDur>67 & hauls$Survey !="NIGFS")
hauls<- hauls[-too_long,]
summary(hauls$HaulDur[hauls$Survey != "NIGFS"])

write.csv(largehauls, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/deleted_too_long_gear_hauls.csv")
summary(as.numeric(hauls$HaulDur))
# Is the Survey Coordinated eg Q1 NS-IBTS
levels(as.factor(hauls$Survey))
# Delete NS-IBTS Quarter 2 and 4 surveys - related to the years of the stomach data collection
# in these years there was no overlap (ie all q 1 and q 3 suveys were within
# correct months)

## Note: this quarter2or4 code doesn't do exactly the same thing as the 
# subset below. According to the text all Q2 and Q4 for NS-IBTS were removed, 
# so the second piece of code does this, despite the seemingly unnessary
# year argument- RK
#quater2or4<-subset(hauls, Survey=="NS-IBTS"&Quarter==2&Year>1997|Quarter==4)


hauls<-subset(hauls,Survey=="NS-IBTS"& Quarter==3 & Year > 1997|Quarter==1
              |Survey=="EVHOE"|Survey=="FR-CGFS"
              |Survey=="IE-IGFS"|Survey=="SWC-IBTS"
              |Survey=="ROCKALL"|Survey== "SCOROC"
              |Survey=="SP-ARSA"|Survey== "SP-PORC"
              |Survey ==  "SP-NORTH"
              |Survey=="BTS"|Survey=="SCOWCGFS"|Survey=="NIGFS"
              |Survey=="PT-IBTS")
unique(hauls$Survey)


# check quaters by survey
cols<-rainbow(13)
plot(hauls$Quarter, col=cols[as.factor(hauls$Survey)], pch=20)
quaterscheck<-ddply(hauls, c("Survey", "Quarter"),
                    summarise,
                    count=length(StNo))

### redundant due to code above which removes Q2 from NS-IBTS trawls - rk
# hauls$QuarterCheck[hauls$Survey=="NS-IBTS" &
#                      hauls$Quarter=="2"& hauls$Year>="1998"|hauls$Survey=="NS-IBTS" & hauls$Quarter=="4"
#                    & hauls$Year>="1998"] <-"changed to 3"
# hauls$Quarter[hauls$Survey=="NS-IBTS" &
#                 hauls$Quarter=="2"& hauls$Year>="1998"|hauls$Survey=="NS-IBTS" & hauls$Quarter=="4"
#               & hauls$Year>="1998"] <-3
hauls$QuarterCheck[hauls$Survey=="BTS" &
                     hauls$Quarter=="4"] <-"changed to 3"
hauls$Quarter[hauls$Survey=="BTS" &
                hauls$Quarter=="4"] <-3
# hauls$QuarterCheck[hauls$Survey=="BTS-VIIa" &
#                      hauls$Quarter=="4"] <-"changed to 3"
# hauls$Quarter[hauls$Survey=="BTS-VIIa" &
#                 hauls$Quarter=="4"] <-3
hauls$QuarterCheck[hauls$Survey=="IE-IGFS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="IE-IGFS" &
                hauls$Quarter=="3"] <- 4
hauls$QuarterCheck[hauls$Survey=="PT-IBTS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="PT-IBTS" &
                hauls$Quarter=="3"] <- 4
hauls$QuarterCheck[hauls$Survey=="SWC-IBTS" &
                     hauls$Quarter=="2"] <-"changed to 1"
hauls$Quarter[hauls$Survey=="SWC-IBTS" &
                hauls$Quarter=="2"] <-1

hauls$QuarterCheck[hauls$Survey=="NIGFS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="NIGFS" &
                hauls$Quarter=="3"] <-4


### In 1997 some NIGFS in early April but survey started in March
### In 2014 NIGFS happened in April but still concidered part
### of Q1 survey. 
hauls$QuarterCheck[hauls$Survey=="NIGFS" &
                     hauls$Quarter=="2"] <-"changed to 1"
hauls$Quarter[hauls$Survey=="NIGFS" &
                hauls$Quarter=="2"] <-1


hauls$QuarterCheck[hauls$Survey=="PT-IBTS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="PT-IBTS" &
                hauls$Quarter=="3"] <- 4

hauls$QuarterCheck[hauls$Survey=="SP-PORC" &
                     hauls$Quarter=="4"] <-"changed to 3"
hauls$Quarter[hauls$Survey=="SP-PORC" &
                hauls$Quarter=="4"] <-3



SP_north <- hauls[hauls$Survey == "SP-NORTH",]
table(SP_north$Quarter, SP_north$Month)
table(SP_north$Quarter, SP_north$Year)

##### Looks like all part of the same surveys from the dates in the table. 
## assign Q3 as Q4 - check this with Fran V - 2021


hauls$QuarterCheck[hauls$Survey=="SP-NORTH" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="SP-NORTH" &
                hauls$Quarter=="3"] <-4


##


# check quaters by survey
cols<-rainbow(13)
plot(hauls$Quarter, col=cols[as.factor(hauls$Survey)], pch=20)

table(hauls$Quarter, hauls$Survey)

table(hauls$QuarterCheck, hauls$Quarter)


#####################################
#

write.csv(hauls, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Working_HH_file_27_072021.csv")
## Check Unique IDS are unique
check<-unique(hauls$NewUniqueID2)
findduplicates<-hauls[duplicated(hauls$NewUniqueID2),]
listF <- findduplicates$NewUniqueID2

unique(findduplicates$Survey)
## 80 duplicates - all in SP-PORC

bad_data<-subset(hauls, NewUniqueID2%in%listF)
nrow(unique(bad_data)) ### these are completely duplicated in all fields. 
## remove duplicated rows from dataset. 

hauls_sub <- hauls[!hauls$NewUniqueID2 %in% listF]
add_SP <- unique(bad_data)

hauls <- rbind(hauls_sub, add_SP)


### remove hauls no longer in the header data from the HL data

HL2$NewUniqueID2<-paste(HL2$Survey,HL2$Year, HL2$Quarter,HL2$Ship, 
                          HL2$HaulNo, HL2$Gear, HL2$StNo, HL2$Country,
                          sep="/")


## Check Unique IDS are unique

nrow(hauls[duplicated(hauls$New_UniqueID2),]) ## should return 0


checkhl<-unique(HL2$NewUniqueID2)
check <- unique(hauls$NewUniqueID2)

length(setdiff(checkhl, check)) ##  hauls no longer in HH - expected
length(setdiff(check, checkhl)) ## odd ids in the HH not in the HL 

HHcheck <- hauls[hauls$NewUniqueID2 %in% setdiff(check, checkhl),]
HHcheck2 <- HL2[HL2$NewUniqueID2 %in% setdiff( checkhl, check),]

table(HHcheck$Survey, HHcheck$Year)
table(HHcheck2$Survey, HHcheck2$Year)
HHcheck_NI <- HHcheck[HHcheck$Survey == "NIGFS",]
HHcheck_NI2 <- HHcheck2[HHcheck2$Survey == "NIGFS",]
unique(HHcheck_NI$ NewUniqueID2) ## okay - "NIGFS/2001/4/74LG/36/ROT/94/NI" - this really does have no haul data.. 
unique(HHcheck_NI2$ NewUniqueID2) ### these are hauls of more than 67 minutes duration. 


#### these are mainly in the Spanish data, SP-NORTH and SP-PORC 
# perhaps because Spain aren't currently submitting all their species records. 
# Remove Spanish surveys - SP-PORC and SP-NORTH from this data product, as this
# can't be easily resolved right now - RK - June 2021

hauls <- hauls[! hauls$Survey %in% c("SP-NORTH", "SP-PORC"),]
HL2 <- HL2[! HL2$Survey  %in% c("SP-NORTH", "SP-PORC"), ]

### Recheck


checkhl<-unique(HL2$NewUniqueID2)
check <- unique(hauls$NewUniqueID2)

length(setdiff(checkhl, check)) ## expected records attached to hauls no longer in HH
length(setdiff(check, checkhl)) ## odd ids in the HH not in the HL 

# now select the hauls and HL files that match up given the new unique IDS
HL3<- HL2[HL2$NewUniqueID2 %in% hauls$NewUniqueID2, ]
checkhl1<-unique(HL3$NewUniqueID2)

### 
setdiff(checkhl1, check)
length(setdiff(check, checkhl1))

### Following Moriarty et al. I will also drop any Hauls info header info and no
# HL data - there are not very many and they may be invalid for unknown reasons..
# RK - June 2021

### hopefully, data might be available for these in future - RK

hauls<-subset(hauls, NewUniqueID2%in% HL3$NewUniqueID2)

check<-unique(hauls$NewUniqueID2)

checkhl1<-unique(HL3$NewUniqueID2)
setdiff(checkhl1, check)
setdiff(check, checkhl1)
# 


# difference in ValidAphiaID
ValidAphiaIDHL<-unique(HL3$ValidAphiaID)
names(HL3)
### Removed line below because 127459 is a real species code for :Symphodus melops (Linnaeus, 1758) - RK-2021
#HL3$ValidAphiaID[is.na(HL3$ValidAphiaID)]<-127459
ValidAphiaIDHL2<-unique(HL2$ValidAphiaID)
setdiff(ValidAphiaIDHL, ValidAphiaIDHL2)
write.csv(ValidAphiaIDHL2, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Standard_Survey_Species_list.csv")
write.csv(ValidAphiaIDHL, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Full_Species_list.csv")

summary(as.numeric(HL3$ValidAphiaID))


HL3[HL3$ValidAphiaID == 0] ### hmmmmm.... check this at a later point..

## set to NA
HL3$ValidAphiaID[which(HL3$ValidAphiaID == 0)] <- NA

### Sort out any lingering -9's in key HH columns - RK 2021

summary(hauls)

hauls$SweepLngt[which(hauls$SweepLngt == -9)] <- NA
hauls$Depth[which(hauls$Depth == -9)] <- NA
hauls$Netopening[which(hauls$Netopening == -9)] <- NA  
hauls$Warplngt[which(hauls$Warplngt == -9)]<- NA
hauls$DoorSpread[which(hauls$DoorSpread == -9)]<- NA        
hauls$WingSpread[which(hauls$WingSpread == -9)]<- NA 
hauls$GroundSpeed[which(hauls$GroundSpeed  == -9)]<- NA 
hauls$SpeedWater[which(hauls$GroundSpeed  == -9)]<- NA  

write.csv(hauls, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Working_HH_file.csv")

head(HL3)

write.csv(HL3, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Working_HL_file.csv")

## tidy - RK -2021
old_files <- c("hauls_sub", "HH_BTS_DE", "HH_save", 
               "HH1", "HH", "invalidhauls", "largehauls",
               "HL2", "landlocked", "non_standard_gear", "pre1983",
                "quaterscheck" ,"remove_stations","SOL_ship" ,
               "SP_north", "check", "checkhl","checkhl1", "x1", "x2", "x3", "x4",
               "HH2", "add_SP", "bad_data", "belgium", "Car_ship",
               "delete_ship", "den", "find", "findduplicates", "funnysweep",
               "HHcheck", "sco", "smallhauls")
rm(list = old_files)

