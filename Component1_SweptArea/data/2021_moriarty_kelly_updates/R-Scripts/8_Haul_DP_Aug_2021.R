###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 8 of 9 
# The purpose of this script is to seperate the surveys and 
# define the final data products structure for the haul files
# Then select the Standard Survey Area using the agreed criteria 
# AUTHOR: Meadhbh Moriarty, 2016
# Edits by: Ruth Kelly (AFBI), 2021 for reruns, Standard Sample Area not applied. 

### Notes on June 2021 version. 
# Script 7 cleaning of HL data has not been run, 
#  Standard Sample Area not applied. 
# Script parts relating to the Biological Product and Standard Sample Area are 
# deleted for simplicity. 


#############
# Prep Data #
#############
# Here we assign the column names to the values that will appear in the top end 
# of the data product to insure transparency and it is user friendly

## to run without running previous scripts load
# setwd("C:/R/OSPAR_IBTS_dc")
#hauls <- read.csv("Data_QA_Process_V5_2021/final_full_cleaned_hauls-end-haul-QA.csv")

h<-hauls
# check that the final list of hauls match in both groups
list<-unique(h$New_UniqueID)
#list<-unique(dat8$New_UniqueID)
# fix time
summary(as.numeric(h$TimeShot))
h$TimeShot_numeric<-(as.numeric(h$TimeShot))
h$TimeShot_numeric[h$TimeShot_numeric>2359]<-h$TimeShot_numeric[h$TimeShot_numeric>2459]/100
find<-subset(h, TimeShot_numeric>2359,)
summary(as.numeric(h$TimeShot_numeric))
h$TimeShot<-h$TimeShot_numeric


#  Add Survey Acronyms from Data products
# See table 2.1 in Moriarty et al (in prep) for details 
names(h)

#h$Survey <- droplevels(h$Survey)
unique(h$Survey)

h$Survey_Acronym[h$Survey=="NS-IBTS"&h$Quarter=="1"]<-"GNSIntOT1"
h$Survey_Acronym[h$Survey=="NS-IBTS"&h$Quarter=="3"]<-"GNSIntOT3"
h$Survey_Acronym[h$Survey=="FR-CGFS"&h$Quarter=="4"]<-"GNSFraOT4"
h$Survey_Acronym[h$Survey=="SWC-IBTS"&h$Quarter=="1"]<-"CSScoOT1"
h$Survey_Acronym[h$Survey=="SWC-IBTS"&h$Quarter=="4"]<-"CSScoOT4"
h$Survey_Acronym[h$Survey=="SCOWCGFS"&h$Quarter=="1"]<-"CSScoOT1"
h$Survey_Acronym[h$Survey=="SCOWCGFS"&h$Quarter=="4"]<-"CSScoOT4"

h$Survey_Acronym[h$Survey=="IE-IGFS"&h$Quarter=="4"]<-"CSIreOT4"

h$Survey_Acronym[h$Survey=="NIGFS"&h$Quarter=="1"]<-"CSNIrOT1"
h$Survey_Acronym[h$Survey=="NIGFS"&h$Quarter=="4"]<-"CSNIrOT4"
h$Survey_Acronym[h$Survey=="EVHOE"&h$Quarter=="4"]<-"CSBBFraOT4"
# h$Survey_Acronym[h$Survey=="SP-ARSA"&h$Quarter=="1"]<-"BBIC(s)SpaOT1"
# h$Survey_Acronym[h$Survey=="SP-ARSA"&h$Quarter=="4"]<-"BBIC(s)SpaOT4"

h$Survey_Acronym[h$Survey=="PT-IBTS"&h$Quarter=="4"]<-"BBICPorOT4"
h$Survey_Acronym[h$Survey=="ROCKALL"&h$Quarter=="3"]<-"WAScoOT3"
h$Survey_Acronym[h$Survey=="SCOROC"&h$Quarter=="3"]<-"WAScoOT3"
#h$Survey_Acronym[h$Survey=="SP_PORC"&h$Quarter=="3"]<-"WASpaOT3"
h$Survey_Acronym[h$Survey=="BTS"&h$Quarter=="3"&
                   h$Country=="NL"]<-"GNSNetBT3"
h$Survey_Acronym[h$Survey=="BTS"&h$Quarter=="3"&
                   h$Country=="DE"]<-"GNSGerBT3"
h$Survey_Acronym[h$Survey=="BTS"&h$Quarter=="3"&
                   h$Country=="GB"]<-"GNSEngBT3"
# h$Survey_Acronym[h$Survey=="BTS-VIIa"&h$Quarter=="3"&
#                    h$Country=="ENG"]<-"CSEngBT3"
summary(as.factor(h$Survey_Acronym))

table(h$Survey[is.na(h$Survey_Acronym) ], h$Country[is.na(h$Survey_Acronym) ])
### Belgian BTS survey.. this wasn't in original product give it a name

h$Survey_Acronym[h$Survey=="BTS"& h$Country=="BE"]<-"BEBTS_not_in_previous"


# add a gear type filer column
h$GearType[h$Survey=="BTS"]<-"BT"
h$GearType[is.na(h$GearType)]<-"OT"
# new haul unique identifer Survey Acronym/ship/year/haul No
# read ship table
ships <- read.csv("Raw_Data/ShipC_TS_Ship.csv")

h$Ship_old <- ships$TS_Ship[match(h$Ship,ships$ShipC)]

table(h$Ship, h$Ship_old)

h$HaulID<-paste(h$Survey_Acronym,h$Ship_old,h$Year,h$HaulNo, h$Country, h$StNo, sep="/")
length(which(duplicated(h$HaulID)))
h$YearShot<-h$Year                
h$MonthShot<-h$Month                  
h$DayShot<-h$Day
#h$HourShot
#h$MinShot
names(hauls)
summary(as.factor(hauls$Month))
h$HaulDur_min<-h$HaulDur
h$ICESStSq<-h$check_StatRec
h$SurvStratum<-h$DepthStratum
summary(as.factor(h$SurvStratum))
# Sort out strata 
names(h)
h$SurvStratum[h$Survey_Acronym=="GNSIntOT1"]<-h$ICESStSq[h$Survey_Acronym=="GNSIntOT1"]
h$SurvStratum[h$Survey_Acronym=="GNSIntOT3"]<-h$ICESStSq[h$Survey_Acronym=="GNSIntOT3"]
h$SurvStratum[h$Survey_Acronym=="GNSGerBT3"]<-h$ICESStSq[h$Survey_Acronym=="GNSGerBT3"]
h$SurvStratum[h$Survey_Acronym=="GNSNetBT3"]<-h$ICESStSq[h$Survey_Acronym=="GNSNetBT3"]
h$SurvStratum[h$Survey_Acronym=="GNSEngBT3"]<-h$ICESStSq[h$Survey_Acronym=="GNSEngBT3"]
find<-subset(h, is.na(SurvStratum),)
summary(as.factor(find$Survey_Acronym))
h$SurvStratum[is.na(h$SurvStratum)]<-"not_recorded"
# we don't believe the recorded wingspreads of 10 on the 
# CGFS - using modelled data instead. 
h$Use_WingSpread[h$Survey_Acronym=="GNSFraOT4"&h$Year=="2014"]<-h$mod1_wingspread_gov[h$Survey_Acronym=="GNSFraOT4"&h$Year=="2014"]
h$QualityWing[h$Survey_Acronym=="GNSFraOT4"&h$Year=="2014"]<-"mod1_wing_gov"
h$Depth_m<-h$DepthNew
h$Distance_km<-h$newDist/1000
h$WingSpread_m<-h$Use_WingSpread
h$DoorSpread_m<-h$Use_DoorSpread
h$NetOpen_m<-h$Use_Netopening
summary(h$SweptArea_wing_km_sqrd)
h$SweptArea_wing_m_sqrd<-h$Use_WingSpread*h$newDist
h$SweptArea_wing_km_sqrd<-h$SweptArea_wing_m_sqrd/1000/1000
h$WingSwpArea_sqkm<-h$SweptArea_wing_km_sqrd
h$WingSwpVol_CorF<-1/(h$Use_Netopening/1000)
h$DoorSwptArea_CorF<-(h$Use_WingSpread/1000)/(h$Use_DoorSpread/1000)
h$DoorSwptVol_CorF<-(h$Use_WingSpread/1000)/((h$Use_DoorSpread/1000)*(h$Use_Netopening/1000))
summary(h$WingSwpVol_CorF)
summary(h$DoorSwptArea_CorF)
summary(h$DoorSwptVol_CorF)
h$ShootLat_degdec<-h$ShootLat
h$ShootLong_degdec<-h$ShootLong

#check duplicate times in h file
h$date_time_check<-paste(h$Ship, h$Year, h$Quarter, h$month, h$Day, h$TimeShot, sep="/")
list<-(unique(h$date_time_check))
# 44 combos are the same now
list<-h[duplicated(h$date_time_check),]
# this is a case of time not been updated on from the previous haul.
# an estimated time will be added into the dataset

check1 <- length(unique(h$HaulID))

h$TimeShot[h$UniqueIDP=="IE-IGFS/2003/4/CEXP/73/GOV"&h$TimeShot=="1651"]<-1851
h$TimeShot[h$UniqueIDP=="IE-IGFS/2004/4/CEXP/11/GOV"&h$TimeShot=="813"]<-2013
h$TimeShot[h$UniqueIDP=="IE-IGFS/2004/4/CEXP/6/GOV"&h$TimeShot=="802"]<-1000
h$TimeShot[h$UniqueIDP=="IE-IGFS/2015/4/CEXP/102/GOV"&h$TimeShot=="745"]<-1400
h$TimeShot[h$UniqueIDP=="NS-IBTS/1995/1/WAH3/4/GOV"&h$TimeShot=="801"]<-2001
h$TimeShot[h$UniqueIDP=="NS-IBTS/1998/1/THA2/9/GOV"&h$TimeShot=="754"]<-1954
h$TimeShot[h$UniqueIDP=="NS-IBTS/2013/1/DAN2/37/GOV"&h$TimeShot=="632"]<-932
h$TimeShot[h$UniqueIDP=="NS-IBTS/2013/3/SCO3/243/GOV"&h$TimeShot=="1130"]<-1430
h$TimeShot[h$UniqueIDP=="NS-IBTS/2016/1/SCO3/38/GOV"&h$TimeShot=="1244"]<-1444
h$TimeShot[h$UniqueIDP=="ROCKALL/2015/3/SCO3/328/GOV"&h$TimeShot=="1054"]<-1300
h$TimeShot[h$UniqueIDP=="ROCKALL/2015/3/SCO3/346/GOV"&h$TimeShot=="926"]<-1130
h$TimeShot[h$UniqueIDP=="SWC-IBTS/2015/1/SCO3/62/GOV"&h$TimeShot=="1010"]<-1320
h$TimeShot[h$UniqueIDP=="SPNGFS/2006/4/CDS/78/BAK"&h$TimeShot=="614"]<-1814
h$TimeShot[h$UniqueIDP=="BTS/2003/3/ISI/11/BT8" &h$TimeShot=="925"]<-1125

h$TimeShot[h$UniqueIDP=="NS-IBTS/2016/3/SCO3/281/GOV" &h$TimeShot=="1424"]<-1705
find<-subset(h, h$Ship=="EZA", ) ## not present
summary(as.factor(find$TimeShot))
#h$TimeShot[h$Ship=="EZA"]<-100*as.numeric(h$TimeShot[h$Ship=="EZA"])
# wash and repeat
h$date_time_check<-paste(h$Ship, h$Year, h$Quarter, h$Month, h$Day, h$TimeShot, sep="/")
list<-(unique(h$date_time_check))
# 0 combos are the same
list<-h[duplicated(h$date_time_check),]

### RK - 12 left not going to update these now. To be checked further later

table(list$Survey, list$Year)



# recheck
list<-(unique(h$HaulID))
list<-h[duplicated(h$HaulID),]
summary(h$WingSwpArea_sqkm)
summary(h$Depth_m)
summary(h$DoorSwptVol_CorF)

h$SurveyDATRAS <- h$Survey
summary(h)
haul_dat<-subset(h, 
                 select=c(HaulID,Survey_Acronym,Ship,GearType,Gear, 
                          YearShot,MonthShot,DayShot,TimeShot, 
                          HaulDur_min,ShootLat_degdec,ShootLong_degdec,ICESStSq,
                          SurvStratum,Depth_m,Distance_km,WingSpread_m, 
                          DoorSpread_m, NetOpen_m,WingSwpArea_sqkm,
                          WingSwpVol_CorF, DoorSwptArea_CorF,DoorSwptVol_CorF,
                          SurveyDATRAS))

for (cat in unique(haul_dat$Survey_Acronym)){
  mypath <- file.path(paste("Data_QA_Process_V5_2021/Diagnostics/Haul Diagnostics", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  par(mfrow=c(2,3))
  d <- subset(haul_dat, Survey_Acronym == cat)
  plot(d$ShootLong_degdec, d$ShootLat_degdec, 
       main=unique(d$cat), pch=19, xlab="Longitude", 
       ylab="Latitude",cex=1.9)
  plot(europe, col="lightgrey", add=T)
  title(unique(d$cat))
  plot(d$HaulDur_min, d$Distance_km, pch=19, xlab="Time (min)", 
       ylab="Distance (km)", cex=1.9)
  x<-c(13:66)
  points(x, x*4*1.852/60, type="l", col="red", lwd=3)
  points(x, x*2*1.852/60, type="l", col="red", lty=2, lwd=2)
  points(x, x*6*1.852/60, type="l", col="red", lty=2, lwd=2)
  plot(d$Distance_km, d$WingSwpArea_sqkm, pch=19, xlab="Distance (km)", 
       ylab="Wing Spread (km2)", cex=1.9)
  plot(d$Depth_m, d$WingSpread_m, pch=19, xlab="Depth (m)", 
       ylab="Wing Spread (m)", cex=1.9)
  plot(d$Depth_m, d$DoorSpread_m, pch=19, xlab="Depth (m)", 
       ylab="Door Spread (m)", cex=1.9)
  plot(d$Depth_m, d$NetOpen_m, pch=19, xlab="Depth (m)", 
       ylab="Net Opening (m)", cex=1.9)
    dev.off()
}
summary(haul_dat)

# disable scientific notation (e.g. e-4 etc.. )
options(scipen=999)

write.csv(haul_dat, "Data_QA_Process_V5_2021/Sampling_info_all_surveysV5_04-08-2021.csv", row.names = FALSE)
write.csv(haul_dat, "Data_QA_Process_V5_2021/Sampling_info_all_surveysV5_04-08-2021.txt",  row.names = FALSE)
write.csv(table(haul_dat$YearShot,haul_dat$SurveyDATRAS), "Data_QA_Process_V5_2021/Hauls_per_year_final_prod.csv")

