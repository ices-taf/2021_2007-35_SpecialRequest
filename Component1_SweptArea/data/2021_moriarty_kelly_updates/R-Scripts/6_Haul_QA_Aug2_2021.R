###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 6 of X
# This Script follows the guidlines outlined in Section 4.1 HH Data - Haul Summary Information
# from: Moriarty and Greenstreet 2016 
# The next step is to model missing data in the haul data of the surveys, and plot 
# lots of graphs to check all the haul stuff is believable now!

# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: Nicola Walker (Cefas) Jonas Hentati Sundberg (SLU)
# Edits - Ruth Kelly (AFBI)- R version 4.1.0 

require(rgdal)
library(marmap)
require(corrgram)
##########################
# Load Data Frame Again  #
##########################
HH<-read.csv("Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Working_HH_file.csv", row.names = "X")
HL<-read.csv("Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Working_HL_file.csv", row.names = "X")

#rm(HL3, hauls)# - if present from previous script - RK

hauls<-(HH) 
summary(hauls)

### remove SP-ARSA

##########################
# 4.1.1 Sample Location #
#########################

# check shoot and haul match ICES Stat Rec
summary(as.factor(hauls$DepthStratum))
# set -9's to NA

hauls$DepthStratum[which(hauls$DepthStratum == -9)] <- NA
summary(as.factor(hauls$DepthStratum)) ## many NAs

table(hauls$StatRec, useNA = "ifany") ## some NAs
# ices<-read.csv("./Regional Boundaries Maps and Data/ICES_rectangles_statistics/Ices_rect_table.csv")
# names(ices)
# shoot longs that are NA should be -9 , silly DATRAS -9 thing messing stuff up here!
summary(hauls$ShootLong)
### not changing these as making them -9 seems like telling R it's a real location... RK 2021
# long_error<-hauls[is.na(hauls$ShootLong),]
# list<-long_error$UniqueID
# hauls$ShootLong[hauls$UniqueID%in%list]<--9

table(hauls$Survey[is.na(hauls$ShootLong)])

# PT-IBTS SWC-IBTS 
# 5       20 


# check haul positions match ICES Stats
names(hauls)
summary(hauls$ShootLat)
lat<-hauls$ShootLat
hauls$check_StatRec<- ices.rect2(hauls$ShootLong, hauls$ShootLat)
summary(as.factor(hauls$check_StatRec))
summary(as.factor(hauls$Survey))
check<-subset(hauls, !(hauls$check_StatRec%in%hauls$StatRec))
check<-hauls[which(hauls$check_StatRec!=hauls$StatRec),]
check<-subset(hauls, is.na(hauls$DepthStratum))
summary(as.factor(check$Survey))
summary(as.factor(hauls$check_StatRec))

hauls$StatRec[is.na(hauls$ShootLong)] ### those with ShootLats did have statistical rectangles use these - RK

hauls$check_StatRec[is.na(hauls$ShootLong)] <- hauls$StatRec[is.na(hauls$ShootLong)]

# use Check StatRec col = mistakes in actual Stat Rec.
# some mis-matches - use check_StatRec for correct Stat Rec
# need to assign stratum based on survey!
# need maps
# New BaseMap


#load in basemap files for all maps
#set directory to working folder and insure subfolders are availible
europe<-readOGR("./Regional Boundaries Maps and Data/shapes//europe.dbf","europe")
contour_1000m<-readOGR("./Regional Boundaries Maps and Data/shapes//1000m.dbf","1000m")
NWW_boundary<-readOGR("./Regional Boundaries Maps and Data/shapes//NWW_boundary.dbf","NWW_boundary")
contour_200m<-readOGR(".//Regional Boundaries Maps and Data/shapes//200m.dbf","200m")
ospar<-readOGR("./Regional Boundaries Maps and Data/ospar_boundaries//OSPAR_inner_Boundary.dbf", "OSPAR_inner_Boundary")
sco1<-readOGR("./Regional Boundaries Maps and Data/SWCQ1.WGS84//SWC_Q1.dbf", "SWC_Q1")
sco3<-readOGR("./Regional Boundaries Maps and Data/SWCQ4.WGS84//SWC_Q4.dbf", "SWC_Q4")
ices<-readOGR("./Regional Boundaries Maps and Data/ICES_rectangles_statistics/ICES_rectangles_statistics.dbf", "ICES_rectangles_statistics")
ire<-readOGR("./Regional Boundaries Maps and Data/IGFS.WGS84//IGFS.WGS84.dbf", "IGFS.WGS84")
ni<-readOGR("./Regional Boundaries Maps and Data/NI-IBTS.WGS84//NI_IBTS.WGS84.dbf", "NI_IBTS.WGS84")
evhoe<-readOGR("./Regional Boundaries Maps and Data/Fr-EVHOE.WGS84//EVHOE.WGS84.dbf", "EVHOE.WGS84")
rock<-readOGR("./Regional Boundaries Maps and Data/SWC-RockQ3.WGS84//SWC_Q3.dbf", "SWC_Q3")

summary(hauls$ShootLong)
#par(mai=c(0,0,0,0),bg='white' )
#plot(contour_200m, add=TRUE, col="lightblue3")
#plot(contour_1000m, add=TRUE, col="lightblue4")
plot(NWW_boundary, border="white", xlim = c(-16,13), ylim = c(36, 62))
plot(europe,add=TRUE, xlim = c(-16, 13), ylim = c(36, 62), asp = 1, col=c('gray81'), 
     border='gray3')
# plot(contour_200m, add=TRUE, border="lightblue3")
# plot(contour_1000m, add=TRUE, border="lightblue4")
plot(ire, add=TRUE, lty=2, lwd=3, border="green")
plot(sco3, add=TRUE, lty=1, lwd=1, border="blue")
plot(evhoe, add=T, lty=1, lwd=1, border="red")
plot(ni, add=T, border="lightblue")
plot(rock, add=T, border="yellow")
plot(NWW_boundary, border="white", xlim = c(-16,13), ylim = c(36, 62))
plot(ices, add=TRUE)
plot(europe,add=TRUE, xlim = c(-16, 13), ylim = c(36, 62), asp = 1, col=c('gray81'), 
     border='gray3')
#text(ices$Centr_X, ices$Centr_Y, ices$ICESNAME)
plot(ospar, col="red", add=T, lwd=4)
check<-subset(hauls, !(hauls$check_StatRec%in%hauls$StatRec))
points(check$ShootLong, check$ShootLat, col="red")
points(hauls$ShootLong, hauls$ShootLat, pch=19, col="black")
unique(check$Survey)
### issues are with SP-ARSA



################
# 4.1.2  Depth #
################
hauls$EstDepth<-hauls$Depth
summary(hauls$Depth)

summary(hauls$ShootLat[is.na(hauls$Depth)])
summary(hauls$ShootLong[is.na(hauls$Depth)])

# All observations with no depth have no haul positional data 
# estimate depth on shoot position is the best we can do

summary(hauls$ShootLat)
summary(as.numeric(hauls$ShootLong))


# use NOAA website to get bathy map
# papoue <- getNOAA.bathy(lon1 = -16, lon2 = 13,
#                         lat1 = 36, lat2 = 62, resolution = .5)

### bathy map stored locally because AFBI IT security blocks the NOAA api from working

 # papoue <- getNOAA.bathy(lon1 = -16, lon2 = 13,
 #                         lat1 = 36, lat2 = 62, resolution = 1)

papoue <- read.bathy("./Regional Boundaries Maps and Data/marmap_coord_-16;36;13;62_res_1.csv", header = TRUE)
 

 
class(papoue)

# make a pretty map of all the stations
png(file="Data_QA_Process_V5_2021/surveydepthmap.png")
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.99), grey(0.95), grey(0.85))
plot(papoue, image = TRUE, land = FALSE, lwd = 0.03,
     bpal = list(c(0, max(papoue), greys),
                 c(min(papoue), 0, blues)))
plot(papoue, n = 1, lwd = 0.04, add = TRUE)
cols<-heat_hcl(13, c = c(80, 30), l=c(30,90), power=c(1/5, 1.5))
hauls$Survey<-(as.factor(hauls$Survey))
points(hauls$ShootLong, hauls$ShootLat, pch=19, 
       cex=0.3, col=cols[hauls$Survey])
dev.off()

# 

hauls$HaulLong[is.na(hauls$ShootLong)]
hauls$Depth[is.na(hauls$ShootLong)] ## all hauls without ShootLongs have depths
hauls$Depth[is.na(hauls$ShootLat)]
### calculate Depths only for hauls where Shoot Longs are known RK
NOAA_Depth<-get.depth(papoue, x=hauls$ShootLong[!is.na(hauls$ShootLong)], y=hauls$ShootLat[!is.na(hauls$ShootLong)], locator=FALSE)

# hauls$HaulLong[hauls$UniqueID=="BTS-VIIa/2006/3/COR/80/BT4A"]<--5.368430
# hauls$HaulLong[hauls$UniqueIDP=="BTS/2006/3/COR/80/BT4A"]#<--5.368430 ## haul is not in 2021 data

hauls$EstDepth[!is.na(hauls$ShootLong)] <- NOAA_Depth$depth*-1
hauls$EstDepth[is.na(hauls$ShootLong)] <- NA ### can't estimate in instances with no longitude 

hauls$DepthNew<-hauls$Depth
hauls$DepthNew[is.na(hauls$Depth)]<-hauls$EstDepth[is.na(hauls$Depth)] 
summary(hauls$DepthNew)
# hauls$DepthNew[hauls$Depth==4]<-18  #no longer needed
# hauls$DepthNew[hauls$Depth==-9]<-37 #no longer needed
summary(hauls$DepthNew)


# make a graph of all the difference between estimated and recorded depths
png(file = "Data_QA_Process_V5_2021/Diagnostics/depth_differences.png", bg = "transparent")
plot(hauls$Depth, hauls$EstDepth, pch=19, xlab="Recorded Depth (m)",
     ylab="Estimated point depth (m)", ylim=c(0,800))
abline(a=0, b=1, col="red")
dev.off()
#write.csv(papoue, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Bathy_map_29_07_2021.csv")

# add some diagnostics to check if the differences in depth are acceptable 
png(file = "Data_QA_Process_V5_2021/Diagnostics/map_depth_differences.png", bg = "transparent")
plot(papoue, image = TRUE, land = FALSE, lwd = 0.03,
     bpal = list(c(0, max(papoue), greys),
                 c(min(papoue), 0, blues)))
plot(papoue, n = 1, lwd = 0.04, add = TRUE)
cols<-heat_hcl(13, c = c(80, 30), l=c(30,90), power=c(1/5, 1.5))
hauls$Survey<-(as.factor(hauls$Survey))
hauls$Diff_dep<-sqrt((hauls$Depth-hauls$EstDepth)^2)
summary(hauls$Diff_dep)
radius<-sqrt(hauls$Diff_dep/pi)
symbols(hauls$ShootLong, hauls$ShootLat, circles=radius, inches=0.2, fg="white",
        bg=cols[hauls$Survey], xlab="Longitude", ylab="Latitude", add=T)
legend.text<-c(1,10,100,1000, 2000)
x<-c(9,9,9,9,9)
y<-c(50,49.8, 49.5, 48.5,47.5)
symbols(x,y, circles=sqrt(legend.text/pi), inches=0.2, fg="black",
        bg="grey", add=T)
text(9.9, 50, "1m")
text(9.9, 49.8, "10m")
text(9.9, 49.5, "100m")
text(9.9, 48.5, "1000m")
text(9.9, 47.5, "2000m")
dev.off()

png(file = "Data_QA_Process_V5_2021/Diagnostics/box_plot_depth_differences_survey.png", bg = "transparent")
plot(hauls$Survey,hauls$Diff_dep, col=cols[hauls$Survey], pch=19)
dev.off()

find<-subset(hauls, hauls$DepthNew<5,)
######################
# 4.1.3 Sweep Length #
######################
summary(as.factor(hauls$SweepLngt))

# Sweep Lenght Values are sometimes incorrect or missing, 
# delete incorrect values 
hauls$EstSweepLngt<-hauls$SweepLngt
#hauls$EstSweepLngt[hauls$SweepLngt>121&hauls$Gear=="GOV"]<-"-9"
hauls$EstSweepLngt[hauls$SweepLngt>121&hauls$Gear=="GOV"]<-NA #RK edit - i don't like -9's!
#hauls$EstSweepLngt[is.na(hauls$SweepLngt)]<-"-9"
summary(as.factor(hauls$EstSweepLngt))

hauls$Gear[which(hauls$SweepLngt == 0)] ## There should not be 0's in GOV gear sweep lengths.. RK

hauls$EstSweepLngt[which(hauls$EstSweepLngt == 0)]  <-NA # RK 2021

# find out extent of issue
sweepsummary<-ddply(hauls, c("Survey","Country", "Year", "Quarter", "EstSweepLngt"),
                    summarise, N=length(EstSweepLngt))
summary(as.factor(hauls$Survey))
# BTS Surveys don't record a sweep - should be NA
# ROT surveys - no sweep - should be NA
# NCT surveys - no sweeps 
hauls$EstSweepLngt[hauls$Survey=="BTS"]<-NA
#hauls$EstSweepLngt[hauls$Survey=="BTS-VIIa"]<-NA
hauls$EstSweepLngt[hauls$Survey=="NIGFS"]<-NA
hauls$EstSweepLngt[hauls$Survey=="PT-IBTS"]<-NA

# in 1983/1984 no country recorded sweep, but in 1985+ countries 
# reported using "Recommended Sweeps" so gonna apply the 60/110 rule to these 
# years
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Year=="1983" & hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Year=="1983" &hauls$DepthNew>75] <-110
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Year=="1984" &hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Year=="1984" & hauls$DepthNew>75]<-110
# Denmark: Apply standard as in Manual
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Country=="DK" & hauls$DepthNew<76] <-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Country=="DK" & hauls$DepthNew>75]<-110
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Year<2005 &
                     hauls$Quarter=="3" & hauls$Country=="DK"]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Year>2004 &
                     hauls$Quarter=="3" & hauls$Country=="DK"& 
                     hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="3"  
                   & hauls$Country=="DK" & hauls$Year>2004 &
                     hauls$DepthNew>75]<-110

# Germany next - Q1 and Q 3 rules differ
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"& 
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Country=="DE" & hauls$DepthNew<76] <-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Country=="DE" & hauls$DepthNew>75]<-110
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Year<2004 &
                     hauls$Quarter=="3" & hauls$Country=="DE"]<-60

#Netherlands next only 2 years missing data in quarter 1
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) &
                     hauls$Quarter=="1" & hauls$Country=="NL"]<-60

#Norway next
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) &
                     hauls$Quarter=="3" & hauls$Country=="NO"]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Country=="NO" & hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Country=="NO" & hauls$DepthNew>75]<-110
# Sweden
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) &
                     hauls$Quarter=="3" & hauls$Country=="SE"]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Country=="SE" & hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Country=="SE" & hauls$DepthNew>75]<-110
# Scotland
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) &
                     hauls$Quarter=="1" & hauls$Country=="GB-SCT"]<-60

# SWC Survey
hauls$EstSweepLngt[hauls$Survey=="SWC-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Year<2011] <-60

# Rockall
hauls$EstSweepLngt[hauls$Survey=="ROCKALL"&
                     is.na(hauls$EstSweepLngt) & hauls$Year<2011]<-60
# Check all new sweeps
sweepsummary<-ddply(hauls, c("Survey","Country", "Year", "Quarter", "EstSweepLngt"),
                    summarise, N=length(EstSweepLngt))

#Assign cat of short or long to sweep length
summary(as.factor(hauls$EstSweepLngt))
hauls$EstSweepCat<-hauls$EstSweepLngt
hauls$EstSweepCat[hauls$EstSweepLngt<61]<-"short"
hauls$EstSweepCat[hauls$EstSweepLngt==97|hauls$EstSweepLngt==110|
                    hauls$EstSweepLngt==100|hauls$EstSweepLngt==120]<-"long"

hauls$EstSweepCat[hauls$EstSweepLngt==200]<-"long" ## Added by RK for long sweeps in SP-ARSA BAK trawls


sweepcatsummary<-ddply(hauls, c("Survey","Country", "Year", "Quarter", "EstSweepCat"),
                       summarise, N=length(EstSweepCat))

#######################
# 4.1.4 Haul Duration #
#######################
# Check range >66 and <13 not within bounds
summary(hauls$HaulDur)

######################
# 4.1.5 Ground Speed #
######################
### Draw some plots to look at data
#make a plot to look at the current groundspeed*time against distance 
# if perfect we expect an intercept of 0 and a slope of 1

hauls[which(hauls$Distance == 45074),] ## belgian BTS being off!
png(file = "Data_QA_Process_V5_2021/Diagnostics/distance_speed_time_differences.png", bg = "transparent")
par(xpd=FALSE)
plot(hauls$GroundSpeed*1852/60*hauls$HaulDur, hauls$Distance, 
     pch=19, col="black", cex=0.5, xlab="Speed X Time", ylab="Distance")
abline(a=0, b=1, col="lightgrey", lwd=2)
dev.off()
png(file="Data_QA_Process_V5_2021/Diagnostics/groundspeed_diagnostics.png", bg="transparent")
plot(hauls$HaulDur, hauls$GroundSpeed, pch=19, xlab="Time", ylab="Speed (knots)")
abline(h=6, col="red")
abline(h=2, col="red")
dev.off()
# Change the confidence interval fill color
png(file = "Data_QA_Process_V5_2021/Diagnostics/distance_speed_time_differences_with_CI.png", bg = "transparent")
p1<-ggplot(hauls, aes(x=hauls$GroundSpeed*1852/60*hauls$HaulDur,
                      y=hauls$Distance)) + 
  geom_point(shape=18, color="black")+
  geom_smooth(method=lm,  linetype="dashed",
              color="lightgrey", fill="darkgrey", se=TRUE, fullrange=FALSE, level=.95)
p1 + scale_color_grey()+theme_classic()
dev.off()
# check ground speed
png(file="Data_QA_Process_V5_2021/Diagnostics/groundspeed_boxplot.png", bg="transparent")
plot(hauls$Survey, hauls$GroundSpeed, pch=19, xlab="Survey", 
     ylab="GroundSpeed (knots)", col="lightgrey")
dev.off()
# outliers in Groundspeed found
check<-hauls[which(hauls$GroundSpeed<3|hauls$GroundSpeed>5), ]
png(file="Data_QA_Process_V5_2021/Diagnostics/groundspeed_distance_comparision.png", bg="transparent")
plot(check$GroundSpeed*check$HaulDur*1852/60, check$Distance, pch=19, col="black")
dev.off()
# In Ns-IBTS 1995 it seems some of the french ground speeds are in the Speed Water Column
# and the Speed water is in the Ground Speed Col.
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/2/GOV"]<-4
hauls$SpeedWater[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/2/GOV"]<-2
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/3/GOV"]<-4
hauls$SpeedWater[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/3/GOV"]<-2
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/6/GOV"]<-4
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/9/GOV"]<-3.8
hauls$SpeedWater[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/9/GOV"]<-1.9
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/11/GOV"]<-3.9
hauls$SpeedWater[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/11/GOV"]<-1.9

# really fast ground speed check outlat long distance matches but in others it was way out
check<-hauls[which(hauls$GroundSpeed<3|hauls$GroundSpeed>5) ,]


# GROUNDSPEED NOT RECORDED BUT DISTANCE AND DURATION RECORDED
hauls$Realised_groundspeed<-hauls$Distance/hauls$HaulDur/1852*60
##summary(hauls$Estimated_groundspeed) # variable doesn't exist - RK

png(file="Data_QA_Process_V5_2021/Diagnostics/groundspeed_predictedVdistance_divided_by_time.png", bg="transparent")
plot(hauls$Realised_groundspeed, hauls$GroundSpeed, 
     pch=19, col="lightgrey",
     xlim=c(1,6), ylim=c(1,6))
abline(0,1, col="black")
dev.off()

png(file = "Data_QA_Process_V5_2021/Diagnostics/RecordedVRealisedGS.png", bg = "transparent")
plot(hauls$GroundSpeed, hauls$Distance/hauls$HaulDur/1852*60,
     pch=19, col="black", cex=1, xlab="Recorded Groundspeed (knots)", 
     ylab="Realised Groundspeed (knots)", xlim=c(0,9), ylim=c(0,9))
abline(a=0, b=1, col="lightgrey", lwd=2)
draw.circle(4,7.5, 1.2, border="red", lwd=2)
abline(h=6, col="lightgrey", lwd=1, lty=2)
abline(v=6, col="lightgrey", lwd=1, lty=2)
abline(h=2, col="lightgrey", lwd=1, lty=2)
abline(h=1, col="lightgrey", lwd=1, lty=2)
draw.circle(8.4, 8.4, 0.1, border="red", lty=4, lwd=2)
dev.off()

##

gs_model1<-lm(GroundSpeed~Quarter:Ship:Gear, data=hauls)
# pretty decent model here - problem is it won't work for all ships as I have some 
# ships with no data at all on Ground speed to inform model
summary(hauls$GroundSpeed)
needSpeed <- hauls[is.na(hauls$GroundSpeed),]
needSpeedShip <- unique(needSpeed$Ship)
withSpeedShip <- unique(hauls$Ship[!is.na(hauls$GroundSpeed)])
needSpeedGear<-unique(needSpeed$Gear)
# ID ships that have some missing GroundSpeed records
canSpeedShip <- needSpeedShip[needSpeedShip %in% withSpeedShip]
canSpeed <- hauls[hauls$Ship %in% canSpeedShip, ]
# Split the data into the two types: only a few missing and completely missing
canSpeedNA <- canSpeed[is.na(canSpeed$GroundSpeed) & is.na(canSpeed$Distance),]
canSpeedOK <- canSpeed[!is.na(canSpeed$GroundSpeed),]

gs_model2<-lm(GroundSpeed~Quarter:Gear, data=hauls)

AIC(gs_model1, gs_model2)
anova(gs_model1,gs_model2)
#predictedGS<-predict(gs_model2, hauls, allow.new.levels=F)
# allmissing observations
canSpeed<-subset(canSpeed, !canSpeed$Gear=="ROT",)
canSpeed$predicted_groundspeed_gs1<-predict(gs_model1, canSpeed , allow.new.levels=T)
summary(canSpeed$predicted_groundspeed_gs1)
plot(canSpeed$predicted_groundspeed_gs1,canSpeed$GroundSpeed, pch=19, col='grey' )
gs5_dat<-subset(hauls, !hauls$Gear=="ROT",)
gs5_dat$predicted_groundspeed_gs2<-predict(gs_model2, gs5_dat , allow.new.levels=T)
summary(gs5_dat$predicted_groundspeed_gs2)
summary(canSpeed$predicted_groundspeed_gs1)
# attach predictions to the hauls dataset
list<-canSpeed$UniqueID

hauls$predicted_groundspeed_gs1[hauls$UniqueID%in%list]<-canSpeed$predicted_groundspeed_gs1[hauls$UniqueID%in%list]
list<-gs5_dat$UniqueID
hauls$predicted_groundspeed_gs2[hauls$UniqueID%in%list]<-gs5_dat$predicted_groundspeed_gs2[hauls$UniqueID%in%list]

# If real data available use that
hauls$GroundSpeed_Used<-hauls$GroundSpeed
hauls$GroundSpeed_Used[hauls$GroundSpeed_Used<2]<-NA
hauls$GroundSpeed_Used[hauls$GroundSpeed_Used>6]<-NA
hauls$GroundSpeed_Used[hauls$UniqueIDP=="IE-IGFS/2015/4/CEXP/100/GOV"]<-NA
hauls$GroundSpeed_Quality_Code[!is.na(hauls$GroundSpeed_Used)]<-"Recorded_Groundspeed"
# If gear is ROT then no data ever available
hauls$GroundSpeed_Quality_Code[hauls$Gear=="ROT"]<-"Manual_Speed"
hauls$GroundSpeed_Used[hauls$Gear=="ROT"]<-4
# If ground speed available for Ship and Gear - use that
hauls$GroundSpeed_Quality_Code[is.na(hauls$GroundSpeed_Used)]<-"model_1"
hauls$GroundSpeed_Used[is.na(hauls$GroundSpeed_Used)]<-hauls$predicted_groundspeed_gs1[is.na(hauls$GroundSpeed_Used)]
# if ground speed available for Gear
summary(hauls$predicted_groundspeed_gs2)
hauls$GroundSpeed_Quality_Code[is.na(hauls$GroundSpeed_Used)]<-"model_2"
hauls$GroundSpeed_Used[is.na(hauls$GroundSpeed_Used)]<-hauls$predicted_groundspeed_gs2[is.na(hauls$GroundSpeed_Used)]
# If no model fits use Manual speed
hauls$GroundSpeed_Quality_Code[is.na(hauls$GroundSpeed_Used)]<-"Manual_Speed"
hauls$GroundSpeed_Used[is.na(hauls$GroundSpeed_Used)]<-4

summary(hauls$GroundSpeed_Used)
summary(as.factor(hauls$GroundSpeed_Quality_Code))

hauls$GroundSpeed_meters_per_min<-hauls$GroundSpeed_Used * 1852 / 60
summary(hauls$GroundSpeed_meters_per_min)
# Spain within expected bounds
summary(hauls$GroundSpeed_Used[hauls$Country=="PT"])
# Portugal within expected bounds
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT7"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT8"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT4A"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="GOV"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="ROT"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BAK"]) # RK
summary(hauls$GroundSpeed_Used[hauls$Gear=="NCT"]) # RK

#get equation for gs1
formula(gs_model1)
summary(gs_model1)
coeffs=(coefficients(gs_model1))
formula(gs_model2)
coeffs=(coefficients(gs_model2))
# draw a graph showing predicted v actual ground speeds.
png(file="groundspeed_predictedVactual.png", bg="transparent")
predict<-predict(gs_model1)
gs<-subset(hauls, !is.na(hauls$GroundSpeed),)
plot(predict, gs$GroundSpeed, xlab="Predicted GroundSpeed (knots)",
     ylab="Recorded Groundspeed (knots)", pch=19)
predict1<-predict(gs_model2)
points(predict1, gs$GroundSpeed, pch=19, col="lightgrey")
abline(0,1, col="red")
dev.off()

########################
# 4.1.6 Towed Distance #
########################
## Calculate haversine distance with shoot and haul coordinates ##
names(hauls)
hauls$LatLongDistance<-(earth.dist(long1 = hauls$ShootLong,
                                      lat1 = hauls$ShootLat,
                                      long2 = hauls$HaulLong,
                                      lat2 = hauls$HaulLat) * 1000)
summary(hauls$LatLongDistance)

hauls1<-as.data.table(hauls)
## RAW DISTANCE ##
hauls1[!is.na(Distance), c("newDist", "qualityDistance") :=
        list(Distance, "rawDistance")]

## HAVERSINE DISTANCE ##
# if haul and shoot coordinates are the same (i.e., equal to zero) then leave as NA
# Also ingore hauls with funny implied speed.
hauls1[is.na(Distance) & !is.na(LatLongDistance) & LatLongDistance > 1 &
        LatLongDistance/HaulDur>61.73 & LatLongDistance/HaulDur<185.2 ,
      c("newDist", "qualityDistance") :=
        list(LatLongDistance, "LatLongDistance")]

## DURATION X SPEED ##
# HaulDur x GroundSpeed
hauls1[, SpeedTimeDist := hauls1$GroundSpeed_meters_per_min*HaulDur]

hauls1[ !is.na(SpeedTimeDist) &is.na(newDist)&
         SpeedTimeDist/HaulDur>61.73 & SpeedTimeDist/HaulDur<185.2,
       c("newDist", "qualityDistance") :=
         list( SpeedTimeDist, "SpeedHaulDur")]

# Hauls that don't have raw distance, shoot/haul coordinates, or GroundSpeed can be estimated
# using linear models to predict GroundSpeed. Two different types of missing data are found:
# 1) ships that have no GroundSpeed records and we need to estimate from other ships, and
# 2) ships that have only a few missing GroundSpeed records and we can use ship as a factor in the lm
# Model for estimating Ground Speed has been reviewed based on feedback 
# from the IBTS working Group (V.T)
summary(as.numeric(hauls1$newDist))

# check new distances relationships
png(file="Data_QA_Process_V5_2021/Diagnostics/distance_speed_time_differences.png", bg = "transparent")
par(xpd=FALSE)
plot(hauls1$SpeedTimeDist, hauls1$newDist, 
     pch=19, col="black", cex=0.5, xlab="Speed X Time", ylab="Distance")
abline(a=0, b=1, col="lightgrey", lwd=2)
dev.off()
# Check distances within bounds
png(file="Data_QA_Process_V5_2021/Diagnostics/distance_speed_time_differences_bounds.png", bg = "transparent")
par(xpd=FALSE)
plot(hauls1$SpeedTimeDist, hauls1$newDist, 
     pch=19, col="black", cex=0.5, xlab="Speed X Time", ylab="Distance")
abline(a=0, b=1, col="lightgrey", lwd=2)
abline(a=0, b=.5, col="red", lwd=2, lty=2)
abline(a=0, b=1.5, col="red", lwd=2, lty=2)
dev.off()
############################

plot(hauls1$HaulDur, hauls1$newDist, pch=19, cex=0.5, xlab="Time", ylab="Distance")
# perfect speed - 4 knots, bounds 2- 6 knots
x<-c(13:66)
points(x, x*4*1852/60, type="l", col="red", lwd=3)
points(x, x*2*1852/60, type="l", col="red", lty=2, lwd=2)
points(x, x*6*1852/60, type="l", col="red", lty=2, lwd=2)
# Set up extra check col
hauls1$manual_speed_distance<-hauls1$HaulDur*4*1852/60
# distance checks required as some of them are really odd.
hauls1$LatLongDistance[hauls1$LatLongDistance==0]<-NA
outside_bounds<-subset(hauls1, hauls1$newDist/hauls1$manual_speed_distance>1.50|
                         hauls1$newDist/hauls1$manual_speed_distance<.5, )

points(outside_bounds$HaulDur,outside_bounds$newDist, col="red", pch=19)
# only 177 to check :) - RK - Meadhbh had 127
# if haversine is available use that
list<-outside_bounds$NewUniqueID2
hauls1$newDist[hauls1$NewUniqueID2%in%list]<-hauls1$LatLongDistance[hauls1$NewUniqueID2%in%list]
hauls1$qualityDistance[hauls1$NewUniqueID2%in%list]<-"LatLongDistance"
# recheck 
outside_bounds<-subset(hauls1, hauls1$newDist/hauls1$manual_speed_distance>1.50|
                         hauls1$newDist/hauls1$manual_speed_distance<.5, )

points(outside_bounds$HaulDur,outside_bounds$newDist, col="blue", pch=19)
# still not gone, use speed*time
list<-outside_bounds$NewUniqueID2
hauls1$newDist[hauls1$NewUniqueID2%in%list]<-hauls1$SpeedTimeDist[hauls1$NewUniqueID2%in%list]
hauls1$qualityDistance[hauls1$NewUniqueID2%in%list]<-"SpeedHaulDur"
# recheck
outside_bounds<-subset(hauls1, hauls1$newDist/hauls1$manual_speed_distance>1.50|
                         hauls1$newDist/hauls1$manual_speed_distance<.5, )

# all within bounds now

# next refine estimates
# is distance used within 20% of lat long distance 
check__dist_haversine_match<-subset(hauls1, hauls1$newDist/hauls1$LatLongDistance>1.2|
                                      hauls1$newDist/hauls1$LatLongDistance<.8,)
points(check__dist_haversine_match$HaulDur,check__dist_haversine_match$newDist, col="green", pch=19)
# so 1436 points more than 20% away - all rest are fine
check_dist_speed_match<-subset(check__dist_haversine_match, 
                               check__dist_haversine_match$newDist/check__dist_haversine_match$SpeedTimeDist>1.2|
                                 check__dist_haversine_match$newDist/check__dist_haversine_match$SpeedTimeDist<.8,)
# 121 not okay with speed
points(check_dist_speed_match$HaulDur,check_dist_speed_match$newDist, col="yellow", pch=19)

check_lat_speed_match<-subset(check_dist_speed_match, 
                              check_dist_speed_match$LatLongDistance/check_dist_speed_match$SpeedTimeDist<1.2&
                                check_dist_speed_match$LatLongDistance/check_dist_speed_match$SpeedTimeDist>.8,)
# so 45 of the haversine and speed X time are within 20% - use the lat long rather than the 
# recorded distance
list<-check_lat_speed_match$NewUniqueID2
hauls1$newDist[hauls1$NewUniqueID2%in%list]<-hauls1$LatLongDistance[hauls1$NewUniqueID2%in%list]
hauls1$qualityDistance[hauls1$NewUniqueID2%in%list]<-"LatLongDistance"
# So whats left
check_no_match<-subset(check_dist_speed_match, 
                       check_dist_speed_match$LatLongDistance/check_dist_speed_match$SpeedTimeDist>1.2|
                         check_dist_speed_match$LatLongDistance/check_dist_speed_match$SpeedTimeDist<.8,)

points(check_no_match$HaulDur,check_no_match$newDist, col="black", pch=19)
# Does value lie within +/- 25% of Man Speed
check_within_bounds<-subset(check_no_match, 
                            check_no_match$newDist/check_no_match$manual_speed_distance>1.25|
                              check_no_match$newDist/check_no_match$manual_speed_distance<.75, )
# check all distances available?
summary(hauls1$newDist)
# problem with some lat long distances not available
# Use manual speed 
hauls1$qualityDistance[is.na(hauls1$newDist)]<-"SpeedHaulDur"
hauls1$newDist[is.na(hauls1$newDist)]<-hauls1$SpeedTimeDist[is.na(hauls1$newDist)]
summary(hauls1$newDist)

# all vaules within accepted bounds.
png(file="Data_QA_Process_V5_2021/Diagnostics/distances_cleaned.png", bg = "transparent")
plot(hauls1$HaulDur, hauls1$newDist, pch=19, cex=0.5, xlab="Time", ylab="Distance")
# perfect speed - 4 knots, bounds 2- 6 knots
x<-c(13:66)
points(x, x*4*1852/60, type="l", col="red", lwd=3)
points(x, x*2*1852/60, type="l", col="red", lty=2, lwd=2)
points(x, x*6*1852/60, type="l", col="red", lty=2, lwd=2)
dev.off()
# all distances in newDist are withing acceptable ranges the best estimate is applied in
# each situation
write.csv(hauls1, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/Working_hauls_12-06-2021.csv")
# remove all the intermediate files 
summary(as.factor(hauls1$qualityDistance))
#############################################
# 4.1.7 Otter Trawl Net Geometry Parameters #
#############################################
#######################
# 4.1.7.1 Wing Spread #
#######################

###########################################
# 4.1.7.1.1 The GOV Otter Trawl (Model 1) #
###########################################
hauls<-hauls1
# remove outlier from irish data
summary(hauls$WingSpread[hauls$Country=="IE"])
# 38 too big
hauls$WingSpread[hauls$Country=="IE" & hauls$WingSpread==38]<-NA
#Centered data for model
summary(hauls$WingSpread)
summary(hauls$DoorSpread)
summary(hauls$Netopening)
# all -9 or 0 values need to be NA
hauls$WingSpread[which(hauls$WingSpread %in% c(0,-9))]<-NA
hauls$DoorSpread[hauls$DoorSpread%in% c(0,-9)]<-NA
hauls$Netopening[hauls$Netopening%in% c(0,-9)]<-NA
summary(hauls$WingSpread)
summary(hauls$DoorSpread)
summary(hauls$Netopening)
summary(hauls$DepthNew)
str(hauls)
# Add Sweep Cats - See Est Sweep Cats above.
hauls[!is.na(Year), "Yearfac":=
        as.factor(Year)]
hauls[!is.na(Quarter), "Qfac":=
        as.factor(Quarter)]
hauls[!is.na(Ship), "Shipfac":=
        as.factor(Ship)]
summary(as.factor(hauls$EstSweepCat[hauls$Gear=="GOV"]))
options(show.signif.stars = T)
library(lmerTest)
#Center data to give model stability 
hauls[!is.na(DepthNew), c("meanDepth") :=
        mean(DepthNew)]
hauls[!is.na(WingSpread), c("meanWingSpread") :=
        mean(WingSpread)]
hauls[!is.na(Netopening), c("meanNetopening") :=
        mean(Netopening)]
hauls[!is.na(DoorSpread), c("meanDoorSpread"):=
        mean(DoorSpread)]
hauls[!is.na(Depth), c("DepthCenter") :=
        Depth-meanDepth]
hauls[!is.na(DepthNew), c("LogDepthCenter") :=
        log(DepthNew)-log(meanDepth)]
hauls[!is.na(WingSpread), c("WingSpreadCenter") :=
        WingSpread-meanWingSpread]
hauls[!is.na(DoorSpread), c("DoorSpreadCenter") :=
        DoorSpread-meanDoorSpread]
hauls[!is.na(WingSpread), c("LogWingSpreadCenter") :=
        log(WingSpread)-log(meanWingSpread)]
hauls[!is.na(DoorSpread), c("LogDoorSpreadCenter") :=
        log(DoorSpread)-log(meanDoorSpread)]
hauls[!is.na(Netopening), c("NetopeningCenter") :=
        Netopening-meanNetopening]
hauls[!is.na(Netopening), c("LogNetopeningCenter") :=
        log(Netopening)-log(meanNetopening)]
hauls$SweepCat<-as.factor(hauls$SweepLngt)
summary(as.factor(hauls$Ship))
summary(hauls)
train<-subset(hauls, Gear=="GOV"& (!is.na(WingSpread)) & (!is.na(DoorSpread))
              &(!is.na(Netopening)),)

library(lme4)
ws_model1<- lmer(log(WingSpread) ~ LogDepthCenter:EstSweepCat
           + (1|Ship:EstSweepCat),
           data=train, REML=FALSE) 

r.squaredGLMM(ws_model1)
unique(train$Ship)
summary(ws_model1)
cols<-rainbow(6)

data<-predict(ws_model1)
summary(as.factor(train$Survey))
summary(train$Survey)
cols<-rainbow(13)
png(file="Data_QA_Process_V5_2021/Diagnostics/GOV_Wingspreads_model1.png", bg="transparent")
plot(train$DepthNew, train$WingSpread, col=cols[as.factor(train$Survey)], 
     pch=15, xlab="Depth (m)", ylab="WingSpread(m)")
points(train$DepthNew, exp(data), pch=21, col=cols[as.factor(train$Survey)],
       bg="black")
lines(lowess(train$WingSpread~train$DepthNew), col="black", lwd=2)
lines(lowess(exp(data)~train$DepthNew), col="red", lwd=3, lty=2)
legend(300, 40, levels(as.factor(hauls$Survey[hauls$Gear=="GOV"])), 
       col=cols, pch=15, ncol=3, cex=1, bty="o")
dev.off()
# set up user data using selected model
# subset all GOV gear
hauls$EstSweepCat<-as.factor(hauls$EstSweepCat)
hauls$Ship<-as.factor(hauls$Ship)
the_gov<-subset(hauls, Gear=="GOV")
summary(the_gov$WingSpread)
summary(the_gov$LogDoorSpreadCenter)
str(the_gov)
# 30442 obs
the_gov$mod1_wingspread_gov<-exp(predict(ws_model1, the_gov, allow.new.levels=T))
summary(the_gov$mod1_wingspread_gov)
# If real values are available use these
hauls$Use_WingSpread[!is.na(hauls$WingSpread)&
                       hauls$Gear=="GOV"]<-hauls$WingSpread[!is.na(hauls$WingSpread)&
                                                              hauls$Gear=="GOV"]
hauls$QualityWing[!is.na(hauls$WingSpread)]<-"raw_wingspread"

list<-the_gov$UniqueID
hauls$mod1_wingspread_gov[hauls$UniqueID%in%list]<-the_gov$mod1_wingspread_gov[the_gov$UniqueID%in%list]

hauls$Use_WingSpread[is.na(hauls$WingSpread)&
                       hauls$Gear=="GOV"]<-hauls$mod1_wingspread_gov[is.na(hauls$WingSpread)&
                                                                       hauls$Gear=="GOV"]
hauls$QualityWing[is.na(hauls$WingSpread)&!is.na(hauls$mod1_wingspread_gov)&
                    hauls$Gear=="GOV"]<-"mod1_wing_gov"
#check wing speads 
summary((hauls$Use_WingSpread[hauls$Gear=="GOV"]))
summary(hauls$Use_WingSpread)
# get model outputs
formula(ws_model1)
summary(ws_model1)
coeffs=(coefficients(ws_model1))

##########################
# 4.1.7.1.4 The NCT Gear #
##########################
# WingSpread = 15.1
hauls$WingSpread[hauls$Gear=="NCT"]<-15.1
hauls$Use_WingSpread[hauls$Gear=="NCT"]<-15.1
hauls$QualityWing[hauls$Gear=="NCT"]<-"mean_wingspread"
#######################
# 4.1.8.1 Door Spread #
#######################
###########################################
# 4.1.8.1.1 The GOV Otter Trawl (Model 1) #
###########################################
# for model election set up training data set
train<-subset(hauls, Gear=="GOV"& (!is.na(WingSpread)) & (!is.na(DoorSpread))
              &(!is.na(Netopening)),)

model1_ds<- lmer(log(DoorSpread) ~ LogDepthCenter:EstSweepCat
           + (1|Ship:EstSweepCat), 
           data=train, REML=FALSE)
summary(model1_ds)
r.squaredGLMM(model1_ds)
cols<-rainbow(13)
data<-predict(model1_ds)
summary(as.factor(train$Survey))
png(file="Data_QA_Process_V5_2021/Diagnostics/GOV_Doorspreads_model1.png", bg="transparent")
plot(train$DepthNew, train$DoorSpread, col=cols[as.factor(train$Survey)], 
     pch=15, xlab="Depth (m)", ylab="WingSpread(m)")
points(train$DepthNew, exp(data), pch=21, col=cols[as.factor(train$Survey)],
       bg="black")
lines(lowess(train$DoorSpread~train$DepthNew), col="black", lwd=2)
lines(lowess(exp(data)~train$DepthNew), col="lightgrey", lwd=3, lty=2)
legend(530, 80, levels(as.factor(hauls$Survey[hauls$Gear=="GOV"])), 
       col=cols, pch=15, ncol=2, cex=.9, bty="o")
dev.off()
# note - less compex limear model can't explain as much variance as mixed model
# the random effects are accounting for quite a lot of variance
# set up user data using selected mode # subset all GOV gear
the_gov<-subset(hauls, Gear=="GOV")
# 30538 obs
the_gov$mod1_doorspread_gov<-exp(predict(model1_ds, the_gov, allow.new.levels=T))
summary(the_gov$mod1_doorspread_gov)
# If real values are available use these
# predict results for spains net opening data
hauls$Use_DoorSpread[!is.na(hauls$DoorSpread)&
                       hauls$Gear=="GOV"]<-hauls$DoorSpread[!is.na(hauls$DoorSpread)&
                                                              hauls$Gear=="GOV"]
hauls$QualityDoor[!is.na(hauls$DoorSpread)]<-"raw_doorspread"

list<-the_gov$UniqueID
hauls$mod1_doorspread_gov[hauls$UniqueID%in%list]<-the_gov$mod1_doorspread_gov[the_gov$UniqueID%in%list]

hauls$Use_DoorSpread[is.na(hauls$DoorSpread)&
                       hauls$Gear=="GOV"]<-hauls$mod1_doorspread_gov[is.na(hauls$DoorSpread)&
                                                                       hauls$Gear=="GOV"]
hauls$QualityDoor[is.na(hauls$DoorSpread)&!is.na(hauls$mod1_doorspread_gov)&
                    hauls$Gear=="GOV"]<-"mod1_door_gov"
#check door spreads
summary((hauls$Use_DoorSpread[hauls$Gear=="GOV"]))
summary(as.factor(hauls$QualityDoor[hauls$Gear=="GOV"]))
summary(hauls$Use_DoorSpread)
summary(model1_ds)
coeffs=coefficients(model1_ds);coeffs

#####################################
# 4.1.8.1.2 The ROT Trawl (Model 2) #
#####################################
summary(hauls$DoorSpread[hauls$Gear=="ROT"])
summary(hauls$DepthNew[hauls$Gear=="ROT"])

# Doorspread can be sorted first
# DoorSpread only has 509 missing values to be estimated

png(file = "Data_QA_Process_V5_2021/Diagnostics/doorspread_ROT.png", bg = "transparent")
plot(hauls$Depth[hauls$Gear=="ROT"], hauls$DoorSpread[hauls$Gear=="ROT"], 
     pch=19, xlab="Depth (m)",
     ylab="Door Spread (m)")
dev.off()

as.data.frame(hauls[hauls$Gear=="ROT" & hauls$DoorSpread < 20,]) ## checked raw data and this is as recorded in trawl survey trawl performances

x<-hauls$Depth[hauls$Gear=="ROT"]
y<-hauls$DoorSpread[hauls$Gear=="ROT"]

plot(y~x,type="n")
m = lm(y~x)
wx = par("usr")[1:2]
new.x = seq(wx[1],wx[2],len=100)
pred = predict(m, new=data.frame(x=new.x), interval="conf", level=.95)
lines(new.x,pred[,"fit"],lwd=2)
lines(new.x,pred[,"lwr"],lty=3)
lines(new.x,pred[,"upr"],lty=3)
points(x,y,pch=16,col="steelblue")
# raw data looks good - no worrying outliers
corrhaul_rot<-subset(hauls, Gear=="ROT",
                     select=c(Year, Depth, Distance,
                              DoorSpread))
summary(corrhaul_rot)

png(file = "Data_QA_Process_V5_2021/Diagnostics/corrhaul_ROT.png", bg = "transparent")
corrgram(corrhaul_rot, order="PCA", lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Hauls Data NI") 
dev.off()
# the only variable available to estimate doorspread is depth for ROT ship
# no sweep, no changes in gear , no wing or netopening

# model1_ds<- lmer(log(DoorSpread) ~ LogDepthCenter:EstSweepCat
#                  + (1|Ship:EstSweepCat), 
#                  data=train, REML=FALSE)
#model2<-lm(log(DoorSpread[hauls$Gear=="ROT"])~log(Depth[hauls$Gear=="ROT"]), data=hauls)

### model updated by Ruth Kelly - AFBI - 03/08/2021
### based on approaches now used in North East atlantic SISP

model2a<-lm(DoorSpread[hauls$Gear=="ROT"]~log(Depth[hauls$Gear=="ROT"]), data=hauls)
AIC(model2a)
summary(model2a)

model2b<-lm(DoorSpread[hauls$Gear=="ROT"]~log(Depth[hauls$Gear=="ROT"]) + Year[hauls$Gear=="ROT"], data=hauls)
AIC(model2b)
summary(model2b)

model2c<-lm(DoorSpread[hauls$Gear=="ROT"]~log(Depth[hauls$Gear=="ROT"]) +
              Year[hauls$Gear=="ROT"] + Ship[hauls$Gear=="ROT"], data=hauls)
AIC(model2c)
summary(model2c)

model2d<-lm(DoorSpread[hauls$Gear=="ROT"]~log(Depth[hauls$Gear=="ROT"]) +
               Ship[hauls$Gear=="ROT"], data=hauls)
AIC(model2d)

model2e<-lm(DoorSpread~log(Depth) +
              Year + Ship*Year, 
            data=hauls[hauls$Gear=="ROT"])

AIC(model2a, model2b, model2c,model2d, model2e)

#          df      AIC
# model2a  3 12683.94
# model2b  4 12635.23
# model2c  5 12622.96
# model2d  4 12670.93
# model2e  6 12124.44

### best is model 2e
# summary(model2e)
# Call:
#   lm(formula = DoorSpread ~ log(Depth) + Year + Ship * Year, data = hauls[hauls$Gear == 
#                                                                             "ROT"])

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -22.3634  -1.7139   0.0584   1.6968  14.4405 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.122e+03  6.178e+01   18.16   <2e-16 ***
#   log(Depth)     8.918e+00  1.231e-01   72.44   <2e-16 ***
#   Year          -5.601e-01  3.092e-02  -18.12   <2e-16 ***
#   Ship74RY      -1.605e+03  6.826e+01  -23.51   <2e-16 ***
#   Year:Ship74RY  8.012e-01  3.410e-02   23.50   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.626 on 2536 degrees of freedom
# (516 observations deleted due to missingness)
# Multiple R-squared:  0.6853,	Adjusted R-squared:  0.6848 
# F-statistic:  1381 on 4 and 2536 DF,  p-value: < 2.2e-16
####

plot(hauls$DoorSpread[hauls$Gear == "ROT"] ~ hauls$Year[hauls$Gear == "ROT"], col = as.numeric(hauls$Ship[hauls$Gear == "ROT"]))


#### return to this when more info is in on when Gear changes occured

plot(DoorSpread[hauls$Gear=="ROT"]~log(Depth[hauls$Gear=="ROT"]), col = as.numeric(hauls$Ship[hauls$Gear=="ROT"]), data=hauls)


summary(model2e)
coeff=coefficients(model2e); coeff
png(file = "doorspread_ROT_model2e_03-08-2021.png", bg = "transparent")
plot(log(hauls$DepthNew[hauls$Gear=="ROT"]), log(hauls$DoorSpread[hauls$Gear=="ROT"]), 
     col = as.numeric(hauls$Ship[hauls$Gear=="ROT"]),
     pch=19, xlab="logged Depth (m)",
     ylab="Doorspread (m)")
#abline(a=(2.558093), b=(0.261106) , col="red", lwd=2)
dev.off()


#### predict the doorspread from the model
predFrom <- data.frame(Depth = hauls$DepthNew[hauls$Gear=="ROT"], Ship = hauls$Ship[hauls$Gear=="ROT"], Year = hauls$Year[hauls$Gear == "ROT"] )
predFrom$Gear <- "ROT"
predFrom <- droplevels(predFrom)


x1 <- predict.glm(model2e, newdata = predFrom, type = "response")
length(x1)
summary(x1)
summary(hauls$DoorSpread[hauls$Gear == "ROT"])
hauls$mod_doorspread_rot[hauls$Gear=="ROT"]<-x1

## old model
#hauls$mod_doorspread_rot[hauls$Gear=="ROT"]<-exp(coeff[1]+coeff[2]*log(hauls$DepthNew[hauls$Gear=="ROT"]))
# set up user values for doorspread

plot(mod_doorspread_rot ~ DoorSpread, data = hauls[hauls$Gear == "ROT",])
plot(mod_doorspread_rot ~ Depth, data = hauls[hauls$Gear == "ROT" ,])
points(DoorSpread ~ Depth, data = hauls[hauls$Gear == "ROT",], col = "blue")


### add model values where doorspread is missing

hauls$Use_DoorSpread[!is.na(hauls$DoorSpread)&
                       hauls$Gear=="ROT"]<-hauls$DoorSpread[!is.na(hauls$DoorSpread)&
                                                              hauls$Gear=="ROT"]
hauls$QualityDoor[!is.na(hauls$DoorSpread)&hauls$Gear=="ROT"]<-"raw_doorspread"
hauls$Use_DoorSpread[is.na(hauls$DoorSpread)&hauls$Gear=="ROT"]<-hauls$mod_doorspread_rot[is.na(hauls$DoorSpread)&hauls$Gear=="ROT"]
hauls$QualityDoor[is.na(hauls$DoorSpread)&hauls$Gear=="ROT"]<-"model_doorspread_rot"

summary(hauls$mod_doorspread_rot[hauls$Gear=="ROT"])
summary(hauls$Use_DoorSpread[hauls$Gear=="ROT"])
summary(as.factor(hauls$QualityDoor))

#####################################
# 4.1.7.1.2 The ROT Trawl (Model 2) #
#####################################
summary(hauls$WingSpread[hauls$Gear=="ROT"])
# 2333 estimated values needed # 2842 in 2021

### model fitted based on relationship between doorspread and wingspread first 
### wingspread and depth where doorspread is missing, based on models 
### fitted for Datras flexfile products in 2021. 
### but with additional data from 2014/2015 not yet on Datras, but in national database here.


png(file = "Data_QA_Process_V5_2021/Diagnostics/wingspread_doorspread_ROT.png", bg = "transparent")
plot(hauls$DoorSpread[hauls$Gear=="ROT"],
     hauls$WingSpread[hauls$Gear=="ROT"],
     pch=19, xlab="Doorspread",
     ylab="Wingspread (m)" )
abline(lm(hauls$WingSpread[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"] ~  hauls$DoorSpread[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"]),col="red")
dev.off()


png(file = "Data_QA_Process_V5_2021/Diagnostics/wingspread_depth_ROT.png", bg = "transparent")
plot(log(hauls$DepthNew[hauls$Gear=="ROT"]),
     hauls$WingSpread[hauls$Gear=="ROT"],
     pch=19, xlab="log(Depth (m))",
     ylab="Wingspread (m)" )
abline(lm(hauls$WingSpread[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"] ~  log(hauls$DepthNew[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"])),col="red")
dev.off()

# given how data poor the situation is this must be kept very simple! 2016 
# data is a bit better now 2021!
ws_dat<-subset(hauls, !is.na(hauls$WingSpread) & hauls$Gear=="ROT", )
summary(ws_dat)

# need doorspread first
# Great we can keep this really simple and have a really strong
# Adjusted R sqd of 0.952
# has the best AIC score (-153.47.6)
#hauls$mod2_wingspread_rot[hauls$Gear=="ROT"]<-exp(0.3798356+0.6489731*log(hauls$Use_DoorSpread[hauls$Gear=="ROT"]))

mod_wsa <- lm(WingSpread ~ DoorSpread, data = hauls[hauls$Gear=="ROT" & !is.na(hauls$WingSpread),])
summary(mod_wsa)
AIC(mod_wsa)


mod_wsb <- lm(WingSpread ~ DoorSpread + Year, data = hauls[hauls$Gear=="ROT" & !is.na(hauls$WingSpread),])
AIC(mod_wsb) ### no support for a difference between years. 

summary(mod_wsa)

# Call:
#   lm(formula = WingSpread ~ DoorSpread, data = hauls[hauls$Gear == 
#                                                        "ROT" & !is.na(hauls$WingSpread), ])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.34522 -0.27794  0.00787  0.26452  1.42633 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.620897   0.352621   13.10   <2e-16 ***
#   DoorSpread  0.285865   0.009154   31.23   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4964 on 171 degrees of freedom
# Multiple R-squared:  0.8508,	Adjusted R-squared:  0.8499 
# F-statistic: 975.1 on 1 and 171 DF,  p-value: < 2.2e-16


Coef_wsa <- coefficients(mod_wsa)
Coef_wsa

fit_wsa <- Coef_wsa[[1]] + Coef_wsa[[2]] * hauls$Use_DoorSpread[hauls$Gear=="ROT"]

hauls$mod2_wingspread_rot[hauls$Gear=="ROT"] <- fit_wsa

#

# set up user values for doorspread
#RAW WINGSPREAD
hauls$Use_WingSpread[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"]<-hauls$WingSpread[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"]
hauls$QualityWing[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"]<-"raw_wingspread"
hauls$Use_WingSpread[is.na(hauls$WingSpread)&hauls$Gear=="ROT"]<-hauls$mod2_wingspread_rot[is.na(hauls$WingSpread)&hauls$Gear=="ROT"]
hauls$QualityWing[is.na(hauls$WingSpread)&hauls$Gear=="ROT"]<-"model_wingspread_rot"

summary(hauls$mod2_wingspread_rot[hauls$Gear=="ROT"])
summary(hauls$Use_WingSpread[hauls$Gear=="ROT"])


##########################
# 4.1.8.1.4 The NCT Gear #
##########################
# No sensors on this gear so mean is applied as supplied by ICES 2010
# DoorSpread 45.7m
hauls$DoorSpread[hauls$Gear=="NCT"]<- 45.7
# set up user values
hauls$Use_DoorSpread[hauls$Gear=="NCT"]<-45.7
hauls$QualityDoor[hauls$Gear=="NCT"]<-"mean_doorspread"
#######################
# 4.1.9.1 Net Opening #
#######################
###########################################
# 4.1.9.1.1 The GOV Otter Trawl (Model 1) #
###########################################
###################
# Net Opening GOV #
###################
plot(train$DepthNew,train$Netopening )
plot(train$WingSpread,train$Netopening )

cols<-rainbow(6)
#data<-predict(lm5)
summary(as.factor(train$Survey))
png(file="Data_QA_Process_V5_2021/Diagnostics/GOV_Netopening_model1.png", bg="transparent")
plot(train$DepthNew, train$Netopening, col=cols[as.factor(train$Survey)], 
     pch=15, xlab="Depth (m)", ylab="Netopening(m)")
points(train$DepthNew, exp(data), pch=21, col=cols[as.factor(train$Survey)],
       bg="black")
lines(lowess(train$Netopening~train$DepthNew), col="black", lwd=2)
lines(lowess(exp(data)~train$DepthNew), col="lightgrey", lwd=3, lty=2)
legend(600, 9, levels(as.factor(hauls$Survey[hauls$Gear=="GOV"])), 
       col=cols, pch=15, ncol=1, cex=1, bty="n")
dev.off()
# note - less compex model can't explain as much variance as mixed model
# the random effects are accounting for quite a lot of variance
# set up user data using selected model
# subset all GOV gear
the_gov<-subset(hauls, Gear=="GOV")
# 30885 obs
nm1<- lmer(log(Netopening) ~ LogDepthCenter:EstSweepCat
           + (1|Ship:EstSweepCat), 
           data=train, REML=FALSE)
summary(nm1)
r.squaredGLMM(nm1)
formula(nm1)
coeffs=coefficients(nm1);coeffs
the_gov$mod1_net_gov<-exp(predict(nm1, the_gov, allow.new.levels=T))
summary(the_gov$mod1_net_gov)
# If real values are available use these
# predict results for spains net opening data
hauls$Use_Netopening[!is.na(hauls$Netopening)&
                       hauls$Gear=="GOV"]<-hauls$Netopening[!is.na(hauls$Netopening)&
                                                              hauls$Gear=="GOV"]
hauls$QualityNet[!is.na(hauls$Netopening)]<-"raw_netopening"

list<-the_gov$NewUniqueID2

hauls$mod1_net_gov[hauls$NewUniqueID2%in%list]<-the_gov$mod1_net_gov[the_gov$NewUniqueID2%in%list]

hauls$Use_Netopening[is.na(hauls$Netopening)&
                       hauls$Gear=="GOV"]<-hauls$mod1_net_gov[is.na(hauls$Netopening)&
                                                                hauls$Gear=="GOV"]
hauls$QualityNet[is.na(hauls$Netopening)&!is.na(hauls$mod1_net_gov)&
                   hauls$Gear=="GOV"]<-"mod1_net_gov"

#check door spreads
summary((hauls$Use_Netopening[hauls$Gear=="GOV"]))
summary(as.factor(hauls$QualityNet[hauls$Gear=="GOV"]))

summary((hauls$Use_Netopening))
summary(as.factor(hauls$QualityNet))
summary((hauls$Use_DoorSpread))
summary(as.factor(hauls$QualityDoor))
summary((hauls$Use_WingSpread))
summary(as.factor(hauls$QualityWing))
#####################################
# 4.1.9.1.2 The ROT Trawl (Model 2) #
#####################################


# Matt sent me the Standard Gear Specs- IBTS Report 2013
## net opening should be 3. 


### 2021 seems we now have enough data for a model. 
length(hauls$Netopening[hauls$Gear=="ROT" & !is.na(hauls$Netopening)])

table(hauls$Year[hauls$Gear=="ROT" & !is.na(hauls$Netopening)])

## check for extreme outliers.. 
trainRot <- hauls[hauls$Gear == "ROT",]
plot(log(trainRot$DepthNew),log(trainRot$Netopening) )
plot(trainRot$DepthNew,trainRot$Netopening)

### some nasty values in here given that the gear spec suggests it should be 3. 

quantile(trainRot$Netopening, 0.95, na.rm = TRUE)

## 95% below 3.5. 

quantile(trainRot$Netopening, 0.99, na.rm = TRUE)

table(trainRot$Year[trainRot$Netopening > 15])

table(trainRot$Year)

### leave these in for now to match Meadhbh's manual. 
### discuss with Stephen/Peter, likelihood of these values would we have paper copies?




nm_rot<- lm(log(Netopening) ~ log(DepthNew), 
            data=trainRot)

### terrible model - R2 < 0.001

nm_rot<- lm(Netopening ~ log(DepthNew), 
            data=trainRot)

### terrible model - R2 < 0.001

#####################################
# Not enough data to model this attribute - either use mean value or don't do 
# vol estimates with ROT gear - similar to Beam gears!
# Matt sent me the Standard Gear Specs- IBTS Report 2013
hauls$Use_Netopening[!is.na(hauls$Netopening)&
                       hauls$Gear=="ROT"]<-hauls$Netopening[!is.na(hauls$Netopening)&
                                                              hauls$Gear=="ROT"]
hauls$QualityNet[!is.na(hauls$Netopening)&
                   hauls$Gear=="ROT"]<-"raw_netopening"
hauls$Use_Netopening[is.na(hauls$Netopening)&hauls$Gear=="ROT"]<-3
hauls$QualityNet[is.na(hauls$Netopening)&hauls$Gear=="ROT"]<-"mean_netopening"
summary(hauls$Use_Netopening[hauls$Gear=="ROT"])



##########################
# 4.1.9.1.4 The NCT Gear #
##########################
# netopening 4.6m
hauls$Netopening[hauls$Gear=="NCT"]<-4.6
hauls$Use_Netopening[hauls$Gear=="NCT"]<-4.6
hauls$QualityNet[hauls$Gear=="NCT"]<-"mean_netopening"
summary(hauls$Use_Netopening[hauls$Gear=="NCT"])
#####################################################
# Beam Trawl DoorSpread, WingSpread, and Netopening #
#####################################################
# Beams have a set "wing spread" so set wing and door as the set width
# net opening will be set to NA
summary(as.factor(hauls$Gear))
# Netherlands
# DoorSpread 8m (BTS manual 2009)
hauls$DoorSpread[hauls$Gear=="BT8"]<- 8
# WingSpread = 8
hauls$WingSpread[hauls$Gear=="BT8"]<-8
# netopening NA
hauls$Netopening[hauls$Gear=="BT8"]<-.8
# Germany 
# DoorSpread 7.2m (BTS manual 2009)
hauls$DoorSpread[hauls$Gear=="BT7"]<-7.2
# WingSpread = 7.2
hauls$WingSpread[hauls$Gear=="BT7"]<-7.2
# netopening NA
hauls$Netopening[hauls$Gear=="BT7"]<-.6
# UK  (BTS manual 2009)
# DoorSpread 7.2m (BTS manual 2009)
hauls$DoorSpread[hauls$Gear=="BT4A"]<-4
# WingSpread = 7.2
hauls$WingSpread[hauls$Gear=="BT4A"]<-4
# netopening NA
hauls$Netopening[hauls$Gear=="BT4A"]<-.525
# setup user values 
hauls$Use_DoorSpread[hauls$Gear=="BT4A"]<-4
hauls$QualityDoor[hauls$Gear=="BT4A"]<-"raw_doorspread"
hauls$Use_WingSpread[hauls$Gear=="BT4A"]<-4
hauls$QualityWing[hauls$Gear=="BT4A"]<-"raw_wingspread"
hauls$Use_Netopening[hauls$Gear=="BT4A"]<-.525
hauls$QualityNet[hauls$Gear=="BT4A"]<-"raw_netopening"

hauls$Use_DoorSpread[hauls$Gear=="BT7"]<-7.2
hauls$QualityDoor[hauls$Gear=="BT7"]<-"raw_doorspread"
hauls$Use_WingSpread[hauls$Gear=="BT7"]<-7.2
hauls$QualityWing[hauls$Gear=="BT7"]<-"raw_wingspread"
hauls$Use_Netopening[hauls$Gear=="BT7"]<-.6
hauls$QualityNet[hauls$Gear=="BT7"]<-"raw_netopening"

hauls$Use_DoorSpread[hauls$Gear=="BT8"]<-8
hauls$QualityDoor[hauls$Gear=="BT8"]<-"raw_doorspread"
hauls$Use_WingSpread[hauls$Gear=="BT8"]<-8
hauls$QualityWing[hauls$Gear=="BT8"]<-"raw_wingspread"
hauls$Use_Netopening[hauls$Gear=="BT8"]<-.8
hauls$QualityNet[hauls$Gear=="BT8"]<-"raw_netopening"

summary(hauls$Use_WingSpread[hauls$Gear=="BT4A"])

### RK 2021 I still have NA's in key fields.. bound to be the BAK gear

table(hauls$Gear[is.na(hauls$Use_WingSpread)])
table(hauls$Gear[is.na(hauls$Use_DoorSpread)])
table(hauls$Gear[is.na(hauls$Use_Netopening)])
### yup!
table(hauls$Gear[is.na(hauls$Use_Distance)])
table(hauls$Gear[is.na(hauls$Use_Depth)])
summary(hauls$GroundSpeed_Used)
### these are okay though

### ommitting BAK because there are odd outliers in 
# wingspread and doorspread plots when calculated

hauls <- hauls[hauls$Gear != "BAK",]

# ###### Dealing with BAK - code from 29-09-2016 code files
# 
# #########################################
# # 4.1.7.1.3 The BAK Trawl (Model 3 & 4) #
# #########################################
# # set up data set for model selection
# spain<-hauls[!is.na(hauls$DoorSpread) & !is.na(hauls$WingSpread)
#              & !is.na(hauls$Netopening)&hauls$Country=="ES", ]
# plot(spain$WingSpread, spain$DoorSpread, pch=19, col=cols[as.factor(spain$Gear)])
# plot(spain$DepthNew, spain$WingSpread, pch=19,col=cols[as.factor(spain$Gear)])
# 
# # 355 observations in dataset.
# door_dat<-hauls[!is.na(hauls$DoorSpread),]
# wing_dat<-spain[!is.na(spain$WingSpread),]
# summary(wing_dat$DoorSpread)
# 
# model3<-lm(log(WingSpread) ~ LogDoorSpreadCenter:SweepCat,data=wing_dat)
# summary(model3)
# predict<-exp(predict(model3))
# col1<-c( "darkred","darkgreen")
# points(wing_dat$DepthNew, predict, col=col1[as.factor(wing_dat$Gear)], pch=19)
# # first I need all the centered and logged data for hauls
# str(hauls)
# #Get Means
# hauls$meanDepth<-mean(hauls$DepthNew)
# mean(hauls$WingSpread[!is.na(hauls$WingSpread)])
# hauls$meanWingSpread[!is.na(hauls$WingSpread)]<-15.10081
# mean(hauls$Netopening[!is.na(hauls$Netopening)])
# hauls$meanNetopening[!is.na(hauls$Netopening)]<-3.696181
# mean(hauls$DoorSpread[!is.na(hauls$DoorSpread)])
# hauls$meanDoorSpread[!is.na(hauls$DoorSpread)]<-61.43747
# # Get Centered data
# hauls$DepthCenter<-hauls$DepthNew-hauls$meanDepth
# hauls$WingSpreadCenter<-hauls$WingSpread-hauls$meanWingSpread
# hauls$DoorSpreadCenter<-hauls$DoorSpread-hauls$meanDoorSpread
# hauls$LogDoorSpreadCenter<-hauls$Netopening-hauls$meanNetopening
# # Log and Center the Data
# hauls$LogDepthCenter<-log(hauls$DepthNew)-log(hauls$meanDepth)
# hauls$LogWingSpreadCenter<-log(hauls$WingSpread)-log(hauls$meanWingSpread)
# hauls$LogDoorSpreadCenter<-log(hauls$DoorSpread)-log(hauls$meanDoorSpread)
# hauls$LogNetopeningCenter<-log(hauls$Netopening)-log(hauls$meanNetopening)
# hauls$SweepCat<-as.factor(hauls$SweepLngt)
# summary(as.factor(hauls$Ship))
# 
# # model3: model3<-lm(log(WingSpread) ~ LogDoorSpreadCenter:SweepCat,data=alldat)
# wing_dat<-subset(hauls, Country=="ES" & !is.na(DoorSpread))
# model3<-lm(log(WingSpread) ~ LogDoorSpreadCenter:SweepCat,
#            data=wing_dat)
# summary(model3)
# wing_dat_len <- nrow(wing_dat)
# wing_dat$predict_lm_wing_dat<-exp(predict(model3, wing_dat, allow.new.levels=T))
# summary(wing_dat$predict_lm_wing_dat)
# plot(wing_dat$DepthNew,wing_dat$predict_lm_wing_dat,
#      pch=19, col=cols[as.factor(wing_dat$Gear)])
# # model2: fm2<-lm(log(WingSpread) ~ LogDepthCenter:SweepCat:Gear,data=alldat)
# wing_dat2<-subset(hauls, Country=="ES",)
# model4<-lm(log(WingSpread) ~ LogDepthCenter:SweepCat,data=wing_dat2)
# 
# summary(model4)
# wing_dat_len <- nrow(wing_dat2)
# wing_dat2$predict_lm_wing_dat<-exp(predict(model4, wing_dat2, allow.new.levels=T))
# summary(wing_dat2$predict_lm_wing_dat)
# plot(wing_dat2$DepthNew,wing_dat2$predict_lm_wing_dat,
#      pch=19, col=cols[as.factor(wing_dat2$Gear)])
# list<-wing_dat2$NewUniqueID2
# formula(model4)
# summary(model4)
# coeffs=(coefficients(model4))
# # predict results for spains wing spread data
# hauls$Use_WingSpread[!is.na(hauls$WingSpread)&hauls$Country=="ES"] <-hauls$WingSpread[!is.na(hauls$WingSpread)&hauls$Country=="ES"]
# hauls$QualityWing[!is.na(hauls$WingSpread)&hauls$Country=="ES"]<-"raw_wingspread"
# 
# list<-wing_dat$NewUniqueID2
# hauls$mod1_wingspread_spa[hauls$NewUniqueID2%in%list]<-wing_dat$predict_lm_wing_dat[wing_dat$NewUniqueID2%in%list]
# list<-wing_dat2$NewUniqueID2
# hauls$mod2_wingspread_spa[hauls$Country=="ES"]<-wing_dat2$predict_lm_wing_dat[wing_dat$NewUniqueID2%in%list]
# 
# hauls$Use_WingSpread[is.na(hauls$WingSpread)&hauls$Country=="ES"]<-hauls$mod1_wingspread_spa[is.na(hauls$WingSpread)&hauls$Country=="ES"]
# hauls$QualityWing[is.na(hauls$WingSpread)&!is.na(hauls$mod1_wingspread_spa)&
#                     hauls$Country=="ES"]<-"mod3_wingspread_spa"
# 
# hauls$Use_WingSpread[is.na(hauls$Use_WingSpread)&
#                        hauls$Country=="ES"]<-hauls$mod2_wingspread_spa[is.na(hauls$Use_WingSpread)&hauls$Country=="ES"]
# hauls$QualityWing[is.na(hauls$QualityWing)&
#                     hauls$Country=="ES"]<-"mod4_wingspread_spa"
# summary(hauls$Use_WingSpread[hauls$Country=="ES"])
# summary(as.factor(hauls$QualityWing[hauls$Country=="ES"]))
# png(file="Data_QA_Process_V5_2021/Diagnostics/WingSpread_data_spain_col.png", bg="transparent")
# cols<- rainbow(3)
# plot(hauls$DepthNew[hauls$Country=="ES"], hauls$Use_WingSpread[hauls$Country=="ES"],
#      col=cols[as.factor(hauls$QualityWing[hauls$Country=="ES"])], pch=19,
#      xlab="Depth (m)", ylab="WingSpread (m)")
# 
# legend(5, 30, levels(as.factor(hauls$QualityWing[hauls$Country=="ES"])), 
#        col=cols, 
#        pch=15, ncol=1, cex=1, bty="n")
# dev.off()
# 
# ###
# 
# #########################################
# # 4.1.8.1.3 The BAK Trawl (Model 3 & 4) #
# #########################################
# # 2 models selected - model 3 and model 4
# 
# # model3: dm3<-lm(log(DoorSpread) ~ LogWingSpreadCenter:SweepCat:Gear,data=spain)
# door_dat<-subset(hauls, Country=="ES"& !is.na(WingSpread),)
# dm3<-lm(log(DoorSpread) ~ LogWingSpreadCenter:SweepCat,data=door_dat)
# summary(dm3)
# door_dat_len <- nrow(door_dat)
# door_dat$predict_lm_door_dat<-exp(predict(dm3, door_dat, allow.new.levels=T))
# plot(door_dat$DepthNew,(door_dat$predict_lm_door_dat),
#      pch=19, col=cols[as.factor(door_dat$Gear)])
# 
# 
# # model2: dm4<-lm(log(DoorSpread) ~ LogDepthCenter:SweepCat:Gear,data=spain)
# door_dat2<-subset(hauls, Country=="ES",)
# dm4<-lm(log(DoorSpread) ~ LogDepthCenter:SweepCat, data=door_dat2)
# summary(dm4)
# door_dat_len <- nrow(door_dat2)
# door_dat2$predict_lm_door_dat<-exp(predict(dm4, door_dat2, allow.new.levels=T))
# summary(door_dat2$predict_lm_door_dat)
# plot(door_dat2$DepthNew,door_dat2$predict_lm_door_dat,
#      pch=19, col=cols[as.factor(door_dat2$Gear)])
# formula(dm4)
# summary(dm4)
# coeffs=(coefficients(dm4))
# coeffs
# # predict results for spains door spread data
# hauls$Use_DoorSpread[!is.na(hauls$Doorspread)&hauls$Country=="ES"] <-hauls$DoorSpread[!is.na(hauls$DoorSpread)&hauls$Country=="ES"]
# hauls$QualityWing[!is.na(hauls$DoorSpread)&hauls$Country=="ES"]<-"raw_doorSpread"
# 
# 
# 
# list<-door_dat2$NewUniqueID2
# hauls$mod2_doorspread_spa[hauls$NewUniqueID2%in%list]<-door_dat2$predict_lm_door_dat[door_dat2$NewUniqueID2%in%list]
# list<-door_dat$NewUniqueID2
# hauls$mod1_doorspread_spa[hauls$NewUniqueID2%in%list]<-door_dat$predict_lm_door_dat[door_dat$NewUniqueID2%in%list]
# 
# 
# hauls$Use_DoorSpread[is.na(hauls$DoorSpread)&
#                        hauls$Country=="ES"]<-hauls$mod1_doorspread_spa[is.na(hauls$DoorSpread)&
#                                                                           hauls$Country=="ES"]
# hauls$QualityDoor[is.na(hauls$DoorSpread)&!is.na(hauls$mod1_doorspread_spa)]<-"mod1_doorpread_spa"
# 
# hauls$Use_DoorSpread[is.na(hauls$Use_DoorSpread)&
#                        hauls$Country=="ES"]<-hauls$mod2_doorspread_spa[is.na(hauls$Use_DoorSpread)&hauls$Country=="ES"]
# hauls$QualityDoor[is.na(hauls$DoorSpread)&is.na(hauls$mod1_doorspread_spa)&
#                     hauls$Country=="ES"]<-"mod2_doorpread_spa"
# 
# summary(hauls$Use_DoorSpread[hauls$Country=="ES"])
# summary(as.factor(hauls$QualityDoor[hauls$Country=="ES"]))
# png(file="Data_QA_Process_V5_2021/Diagnostics/DoorSpread_data_spain_col.png", bg="transparent")
# plot(hauls$DepthNew[hauls$Country=="ES"], hauls$Use_DoorSpread[hauls$Country=="ES"],
#      col=cols[as.factor(hauls$QualityDoor[hauls$Country=="ES"])], pch=19,
#      xlab="Depth (m)", ylab="DoorSpread (m)")
# legend(400, 75, levels(as.factor(hauls$QualityDoor[hauls$Country=="ES"])), 
#        col=cols, 
#        pch=15, ncol=1, cex=1, bty="n")
# dev.off()
# 
# ### net opening ## 
# 
# #########################################
# # 4.1.9.1.3 The BAK Trawl (Model 3 & 4) #
# #########################################
# # door with no net 43 obs and wing 148 obs - less than 5% of the data needing estimated
# # use model 3 it explains about 21 % of variation - poor in comparsion to Wing
# # and Door but better than using a straight mean.
# spain<-hauls[hauls$Country=="ES", ]
# 
# net_model3<-lm(log(Netopening) ~ LogDepthCenter:SweepCat, data=spain)
# summary(net_model3)
# formula(net_model3)
# coeffs=(coefficients(net_model3))
# 
# spain_len <- nrow(spain)
# spain$predict_lm_net_dat<-exp(predict(net_model3, spain, allow.new.levels=T))
# plot(spain$DepthNew,(spain$predict_lm_net_dat),
#      pch=19, col=cols[as.factor(spain$Gear)])
# # predict results for spains net opening data
# hauls$Use_Netopening[!is.na(hauls$Netopening)&
#                        hauls$Country=="ES"]<-hauls$Netopening[!is.na(hauls$Netopening)&
#                                                                  hauls$Country=="ES"]
# hauls$QualityNet[!is.na(hauls$Netopening)]<-"raw_netopening"
# 
# list<-spain$NewUniqueID2
# hauls$mod1_netopening_spa[hauls$NewUniqueID2%in%list]<-spain$predict_lm_net_dat[spain$NewUniqueID2%in%list]
# summary(hauls$mod1_netopening_spa[hauls$Country=="ES"])
# hauls$Use_Netopening[is.na(hauls$Netopening)&
#                        hauls$Country=="ES"]<-hauls$mod1_netopening_spa[is.na(hauls$Netopening)&
#                                                                           hauls$Country=="ES"]
# hauls$QualityNet[is.na(hauls$Netopening)&!is.na(hauls$mod1_netopening_spa)]<-"mod1_net_spa"
# 
# summary(hauls$Use_Netopening[hauls$Country=="ES"])
# summary(as.factor(hauls$QualityNet[hauls$Country=="ES"]))
# png(file="Data_QA_Process_V5_2021/Diagnostics/NetOpening_data_spain_col.png", bg="transparent")
# plot(hauls$DepthNew[hauls$Country=="ES"], hauls$Use_Netopening[hauls$Country=="ES"],
#      col=cols[as.factor(hauls$QualityNet[hauls$Country=="ES"])], pch=19,
#      xlab="Depth (m)", ylab="Net Opening (m)")
# legend(0, 4, levels(as.factor(hauls$QualityNet[hauls$Country=="ES"])), 
#        col=cols, 
#        pch=15, ncol=1, cex=1, bty="n")
# dev.off()
# summary(hauls$Use_Netopening[hauls$Country=="ES"])
# summary(hauls$Use_Netopening)

########################################
# so at this point some of the results haven't transfered over - code reviewed
# and changes made

write.csv(hauls, "Data_QA_Process_V5_2021/Diagnostics/Diagnostic_data/hauls_monster_file_04_08_2021.csv")

###########################################################
# 4.1.8 Calculation of the Area/Volume Swept by the Trawl #
###########################################################
summary(hauls$Use_WingSpread)
summary(hauls$Use_DoorSpread)
summary(hauls$Use_Netopening)
summary(hauls$newDist)

plot(hauls$DepthNew[hauls$Country=="IE"], hauls$Use_WingSpread[hauls$Country=="IE"])

hauls$SweptArea_wing_m_sqrd<-hauls$Use_WingSpread*hauls$newDist
hauls$SweptArea_wing_km_sqrd<-hauls$SweptArea_wing_m_sqrd/1000/1000
summary(hauls$SweptArea_wing_km_sqrd)
summary(hauls$Survey)
cols<-topo.colors(13)
plot(hauls$HaulDur,hauls$newDist, pch=19, col=cols[hauls$Survey])

plot(hauls$HaulDur,hauls$SweptArea_wing_km_sqrd, pch=19, col=cols[hauls$Survey])


hauls[, c("QualityWing_SweptArea") := list(paste0(qualityDistance, hauls$QualityWing)),]  
check_speed<-hauls$newDist/hauls$HaulDur

summary(check_speed)


hauls[, c("Wing/Door(Ratio)"):= list(hauls$Use_WingSpread/hauls$Use_DoorSpread),]
# Save "raw" files - before all estimated data is added
write.csv(HH, "Data_QA_Process_V5_2021/Raw_Combined_Data-end-haul-QA.csv")
# remove from R environment
# Save monster HH Chron File
write.csv(hauls, "Data_QA_Process_V5_2021/final_full_cleaned_hauls-end-haul-QA.csv")

# take a list of haul IDs for use in the HL observation selection 
list<-unique(hauls$NewUniqueID2)
list1<-unique(HL$NewUniqueID2)
length(list)-length(list1)
setdiff(list1, list)
setdiff( list, list1) ### these are due to removing 'BAK'


HL1<-subset(HL, NewUniqueID2%in%list)
write.csv(HL1, "Data_QA_Process_V5_2021/HL_data_end_of_script6-QA.csv")


