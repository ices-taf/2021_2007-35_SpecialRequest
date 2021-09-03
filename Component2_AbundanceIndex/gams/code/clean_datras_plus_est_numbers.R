#############################################################
#### Code to download and clean survey data from DATRAS
#### Adapted to build abundance indices of sensitive and by-caught species
#### Related paper: Are fish sensitive to trawling recovering in the Northeast Atlantic?
#### Coding: Aurore Maureaud, July 2020
#### Updated: Tobias Mildenberger, June 2021
#############################################################

setwd("../")

##########################################################################################
#### LOAD LIBRARIES
##########################################################################################
## library(data.table)
library(dplyr)
library(icesDatras)
library(openxlsx)
library(xtable)
library(rgdal)

## increase memory on linux
unix::rlimit_as(1e13, 1e13)


## Additional script
source("code/extra.functions.R")

## variables
last.year <- 2020
dbg <- 0
download <- FALSE


##########################################################################################
#### LOAD FILES
##########################################################################################
## load swept area estimated by Klaas
load("data/hh_swept_area_v3.RData")
## make haul id for this data set
hh$HaulID <- paste(hh$Survey, hh$Year,hh$Quarter, hh$Country, hh$Ship, hh$Gear,
                   hh$StNo, hh$HaulNo, sep = ":")
range(hh$Depth_gam, na.rm=TRUE)


## Some 0 in Swept area index
if(FALSE){
    hh$SweptArea <- hh$SweptAreaBWKM2
    hh$SweptArea[is.na(hh$SweptArea)] <- hh$SweptAreaWSKM2[is.na(hh$SweptArea)]
    range(hh$SweptArea)
    ##
    ind <- which(hh$SweptArea == 0)
    length(ind)
    hh$DoorSpread[ind]
    hh$WingSpread[ind]
    hh$Distance[ind]
    hh$GroundSpeed[ind]
    hh$HaulDur[ind]
    hh$Depth[ind]
    hh$SweepLngt[ind]
    hh$Warplngt[ind]
    hh$ShootLong[ind]
    hh$ShootLat[ind]
    hh$HaulLong[ind]
    hh$HaulLat[ind]
}

## only keep relevant columns:
hh_sa <- hh %>% select(HaulID,
                       SweptAreaDSKM2, SweptAreaWSKM2, SweptAreaBWKM2,
                       Depth_gam)



gears <- read.xlsx("data/Fishing_gears_input_RKynoch_FBurns.xlsx")
gear_cats <- gears[,c(1,2,6,7)]
colnames(gear_cats) <- c("Survey","Gear","GearCat","Comment")
## no duplicates:
any(duplicated(paste0(gear_cats$Survey,"-",gear_cats$Gear)))


## Load ICES rectangles shapefiles
rect <- readOGR(dsn = "data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp",
                layer="ICES_Statistical_Rectangles_Eco")
rect2 <- readOGR(dsn = "data/statRec_to_ICESarea/StatRec_map_Areas_Full_20170124.shp")




## download/load raw data (overwrites Klaas' hh)
if(download){
    source("code/download_datras.R")
}else{
    load("data/hh_hl.RData")
}


## Subset data sets to reduce memory usage and prevent R collapse
## -----------
hh <- hh %>%
    select("Survey","Quarter","Country","Ship","Gear","StNo",
           "HaulNo","Year","Month","Day","TimeShot",
           "DepthStratum","HaulDur","DayNight",
           "ShootLat","ShootLong","StatRec","Depth",
           "HaulVal","StdSpecRecCode","BySpecRecCode","DataType")

## Already in hh are a few years missing:
## This proportion increases, as not available in HL, invalid codes, etc.
## ----------------
tmp <- aggregate(list(Years = hh$Year),
                 by=list(Survey=hh$Survey,Quarter=hh$Quarter),
                 unique)
tmp <- tmp[order(tmp$Survey),]
tmp2 <- cbind(tmp[,1:2],
      should = sapply(tmp$Years,max) - sapply(tmp$Years,min) + 1,
      is = sapply(tmp$Years,length))
as.vector(apply(tmp2[which(tmp2[,3] != tmp2[,4]),1:2],1,paste,collapse="-"))
## Problem with:
## [1] "NIGFS-4"    "PT-IBTS-4"  "ROCKALL-3"  "SNS-3"      "SNS-4"
## [6] "SP-ARSA-1"  "SP-ARSA-4"  "SP-NORTH-3" "SP-NORTH-4"



##########################################################################################
#### CREATE A UNIQUE HAUL ID
##########################################################################################
hl$HaulID <- paste(hl$Survey, hl$Year,hl$Quarter, hl$Country, hl$Ship, hl$Gear,
                   hl$StNo, hl$HaulNo, sep = ":")
hh$HaulID <- paste(hh$Survey, hh$Year,hh$Quarter, hh$Country, hh$Ship, hh$Gear,
                   hh$StNo, hh$HaulNo, sep = ":")

writeLines(paste0("Hauls: ", nrow(hh)))

## HaulID unique in hh?
tmp <- duplicated(hh$HaulID)
ind <- which(tmp)
if(length(ind) > 0 && dbg > 0){
    for(i in 1:length(ind)){
        dups <- hh[which(hh$HaulID==hh$HaulID[ind[i]]),]
        flag <- all(apply(dups,2,function(x) all(x == x[1],na.rm = TRUE)))
        ## writeLines(paste0(i, ": ", flag))
        if(!flag){
            for(j in 2:nrow(dups)){
                writeLines(paste0(colnames(dups)[which(dups[j,] != dups[1,])]))
                writeLines(paste0(dups[c(1,j),which(dups[j,] != dups[1,])]))
            }
        }
    }
}
## Only differences between duplicated HaulIDs in WindSpeed -> delete duplicates
hh <- hh[-ind,]
nrow(hh) == length(unique(hh$HaulID))
writeLines(paste0("Removing duplicated hauls."))
writeLines(paste0("Hauls: ", nrow(hh)))


## same for hh_sa
## HaulID unique in hh?
tmp <- duplicated(hh_sa$HaulID)
ind <- which(tmp)
if(length(ind) > 0 && dbg > 0){
    for(i in 1:length(ind)){
        dups <- hh_sa[which(hh_sa$HaulID==hh_sa$HaulID[ind[i]]),]
        flag <- all(apply(dups,2,function(x) all(x == x[1],na.rm = TRUE)))
        ## writeLines(paste0(i, ": ", flag))
        if(!flag){
            for(j in 2:nrow(dups)){
                writeLines(paste0(colnames(dups)[which(dups[j,] != dups[1,])]))
                writeLines(paste0(dups[c(1,j),which(dups[j,] != dups[1,])]))
            }
        }
    }
}
## Only differences between duplicated HaulIDs in WindSpeed -> delete duplicates
hh_sa <- hh_sa[-ind,]
nrow(hh_sa) == length(unique(hh_sa$HaulID))
writeLines(paste0("Removing duplicated hauls."))
writeLines(paste0("Hauls: ", nrow(hh_sa)))


## Only keep hauls where there is the length composition.
hh <- subset(hh, hh$HaulID %in% hl$HaulID)
hl <- subset(hl, hl$HaulID %in% hh$HaulID)
hh_sa <- subset(hh_sa, hh_sa$HaulID %in% hh$HaulID)
writeLines(paste0("Removing hauls not in hl."))
writeLines(paste0("Hauls: ", nrow(hh)))
writeLines(paste0("Hauls: ", nrow(hh_sa)))


##########################################################################################
## MERGE HH and HL DATA SETS
##########################################################################################
## subset hl (also to avoid double info in hh and hl after merging)
hl <- hl %>%
    select(c("HaulID","SpecCodeType","SpecCode","SpecVal",
             "Sex","TotalNo","CatIdentifier","SubFactor",
             "LngtClass","HLNoAtLngt","Valid_Aphia"))

haulidhl <- sort(unique(hl$HaulID))
haulidhh <- sort(unique(hh$HaulID))
identical(haulidhh, haulidhl)

writeLines(paste0("Hauls: ", nrow(hh)))

survey <- right_join(hh, hl, by=c('HaulID'))
nrow(survey)==nrow(hl)

rm(hh, hl)
gc()
## pryr::mem_used()


haulid1 <- sort(unique(survey$HaulID))
haulid2 <- sort(unique(hh_sa$HaulID))
identical(haulid1, haulid2)  ## some hauls missing in hh_sa, why?

## Add Swept Area
survey <- left_join(survey, hh_sa, by=c('HaulID'))


## Add missing StatRec
survey$StatRec2 <- NA
tmp <- rect@data[,c(2:6)]
ind <- which(is.na(survey$StatRec))
for(i in 1:length(ind)){
    tmp2 <- tmp$ICESNAME[which(survey$ShootLat[ind[i]] >= tmp$SOUTH &
                                                  survey$ShootLat[ind[i]] < tmp$NORTH &
                                                  survey$ShootLong[ind[i]] >= tmp$WEST &
                                                  survey$ShootLong[ind[i]] < tmp$EAST)]
    print(tmp2)
    if(length(tmp2) == 1) survey$StatRec2[ind[i]] <- tmp2
}

## only 167 couldn't be matched:
ind <- which(!is.na(survey$StatRec2))
length(which(is.na(survey$StatRec)))
length(which(!is.na(survey$StatRec2)))

survey$StatRec[ind] <- survey$StatRec2[ind]


## Add Ecoregion
tmp <- rect@data[,c("ICESNAME","Ecoregion")]
colnames(tmp) <- c("StatRec","Ecoregion")
any(duplicated(tmp$StatRec))
survey <- left_join(survey, tmp, by=c('StatRec'))
ind <- which(is.na(survey$Ecoregion))
length(ind) ## only the ones where StatRec is NA
unique(survey$StatRec[ind])
unique(survey$Survey[ind]) ## DYFS

## Add ICES Area
tmp <- rect2@data[,c("ICESNAME","Area_27")]
colnames(tmp) <- c("StatRec","Area_27")
any(duplicated(tmp$StatRec))
survey <- left_join(survey, tmp, by=c('StatRec'))
ind <- which(is.na(survey$Area_27))
length(ind) ## only the ones where StatRec is NA
unique(survey$StatRec[ind])
unique(survey$Survey[ind]) ## DYFS


## there have been minor changes to the gear in these surveys, it should not affect sensitives
survey <- survey %>%
    mutate(Survey = if_else(Survey=='SCOWCGFS', 'SWC-IBTS', Survey)) %>%
    mutate(Survey = if_else(Survey=='SCOROC','ROCKALL',Survey)) %>%
    mutate(Survey = if_else(Survey=='BTS-VIII','BTS',Survey))  %>% ## legit?
    filter(!(Survey == "BITS" & !Gear %in% c("TVS","TVL")))

## intermediate save
## save(survey, file='data/survey_int.RData', version=2)
## load('data/survey_int.RData')


## Gear Categories
gear_cats[which(!(gear_cats$Gear %in% survey$Gear & gear_cats$Survey %in% survey$Survey)),]
gear_cats <- subset(gear_cats, gear_cats$Gear %in% survey$Gear & gear_cats$Survey %in% survey$Survey) ## BTS - BT4 not in survey

survey <- left_join(survey, gear_cats[,c(1:3)], by=c('Survey','Gear'))
ind <- which(is.na(survey$GearCat))
length(ind)

## Manual corrections:
gear_cats[!is.na(gear_cats$Comment),]
## 1. 27.7g and 27.7a
unique(survey$Area_27)
ind <- which(survey$Survey == "IE-IGFS" & survey$Gear == "GOV" & survey$Area_27 %in% c("7.g","7.a"))
length(ind)
survey$GearCat[ind] <- "GOV_CL"
## 2.
ind <- which(survey$Survey == "SWC-IBTS" & survey$Gear == "GOV" & survey$ShootLat < 57.5)
length(ind)
survey$GearCat[ind] <- "GOV_CL"

## any gear categories NA?
ind <- which(is.na(survey$GearCat))
length(ind)
unique(paste0(survey$Survey[ind],":",survey$Gear[ind]))  ## only some BITS gears which will be removed later

rm(hh_sa, gear_cats)
gc()

survey <- survey %>%
    dplyr::rename(Lon = ShootLong,
                  Lat = ShootLat,
                  AphiaID = Valid_Aphia)

## save(survey, file="survey_int.RData", version=2)
## load("survey_int.RData")


## Overwrite depth
survey$Depth <- survey$Depth_gam

## Combine swept area
survey$SweptArea <- survey$SweptAreaBWKM2
survey$SweptArea[is.na(survey$SweptArea)] <- survey$SweptAreaWSKM2[is.na(survey$SweptArea)]
any(is.na(survey$SweptArea))
any(survey$SweptArea == 0)
ind <- which(survey$SweptArea == 0)
length(ind)

## cut data set (R collapse)
survey <- survey %>%
    dplyr::select(!c("Depth_gam","SweptAreaWSKM2","SweptAreaBWKM2","SweptAreaDSKM2"))



##########################################################################################
#### REMOVE INVALID DATA + CLEAN DATA SET
##########################################################################################

## Convert -9 to NA
## ---------
range(survey$SubFactor)
range(survey$HaulDur)
range(survey$HLNoAtLngt)
writeLines(paste0("Hauls with -9 in SubFactor or HLNoAtLngt: ",
                  length(unique(survey$HaulID[which(survey$SubFactor == -9 | survey$HLNoAtLngt == -9)])),
                  ". Setting -9 equal to NA."
                  ))
survey <- minus9toNA(survey)



## DataType
## ---------
## 43 Hauls (73 entries) with DataType = NA, but for all TotalNo = HLNoAtLngt = -9
## Thus, keep them as they might still be important for zero catches OR
ind <- which(is.na(survey$DataType))
## length(unique(survey$HaulID[ind]))
## survey$TotalNo[ind]
## survey$HLNoAtLngt[ind]
## survey$AphiaID[ind]
survey$DataType[ind] <- "Z"
## EVHOE datatype is entered as ‘C’ in 2018. This is a mistake it should be ‘R’.
ind <- which(survey$Survey == "EVHOE" & survey$Year == 2018)
unique(survey$DataType[ind])
survey$DataType[ind] <- "R"


## AphiaID
## ---------
## 4585 Hauls (6612 entries) with AphiaID == NA
## Keep these Hauls for Zero data set
ind <- which(is.na(survey$AphiaID))
length(unique(survey$HaulID[ind]))
unique(survey$TotalNo[ind])
unique(survey$Year[ind])  ## over many years
unique(survey$Survey[ind]) ## and surveys
## Hauls which are AphiaID = NA, have a SpecCode:
tmp <- unique(survey$SpecCode[ind])
## for(i in 1:length(tmp)){
##     print(survey$AphiaID[which(survey$SpecCode == tmp[i])])
## }
## For all of them AphiaID = NA, no back-transforming possible


## HaulVal
## ---------
## 3656 Hauls (129945 entries) with HaulVal not equal to V
## Remove hauls!
ind <- which(survey$HaulVal != "V")
length(unique(survey$HaulID[ind]))
length(survey$HaulID[ind])


## SpecVal
## ---------
## 15712 hauls (583750 entries) with SpecVal not equal to 1,4,7,10
## unique(survey$SpecVal) ## see: http://vocab.ices.dk/?ref=5
ind <- which(!survey$SpecVal %in% c(1,4,7,10))
length(unique(survey$HaulID[ind]))
length(survey$HaulID[ind])

## 7369 Hauls (68248 entries) with SpecVal equal to 5 or 6
## The other half either NA, 0, or 2 (not useful)
## CHECK: Keep them for presence-absence modelling of rare species?
ind2 <- which(survey$SpecVal %in% c(5,6))
length(unique(survey$HaulID[ind2]))
length(survey$HaulID[ind2])


## StdSpecRecCode
## ---------
## http://vocab.ices.dk/?ref=88
## 0 	No standard species recorded
## 1 	All standard species recorded
## 2 	Pelagic standard species recorded
## 3 	Roundfish standard species recorded
## 4 	Individual standard species recorded
unique(survey$StdSpecRecCode)
ind <- which(survey$StdSpecRecCode != 1)
length(unique(survey$HaulID[ind]))
table(survey$Year[ind])
## only keep StdSpecRecCode == 1


## Lat and Lon
## ---------
ind <- which(is.na(survey$Lat))
length(unique(survey$HaulID[ind]))
ind <- which(is.na(survey$Lon))
length(unique(survey$HaulID[ind]))



## Apply selection
## ---------
survey <- survey %>%
    filter(HaulVal %in% 'V',
           SpecVal %in% c(1,10,4,7,5,6),
           StdSpecRecCode == 1,
           !is.na(Lat),
           !is.na(Lon)
           )

##########################################################################################
#### Clean species names
##########################################################################################
## In historical submissions TSN and NODC species codes were used, which is
## reflected in the SpecCodeTypes T and N in older data.
length(is.na(survey$AphiaID))

aphia_list <- unique(survey$AphiaID)
aphia_list <- aphia_list[!duplicated(aphia_list)]
length(aphia_list)

## convert TSN to AphiaID (at least for rare species with this info)
wkabsens <- read.csv("data/wkabsens_species_list.csv")
length(unique(wkabsens$Species)) # 133 species
wkabsensSel <- wkabsens[which(!is.na(wkabsens$TSN_code)),]

ind <- which(is.na(survey$AphiaID))
tmp <- unique(survey$SpecCode[ind])
res <- rep(NA,length(tmp))
for(i in 1:length(tmp)){
    res[i] <- any(wkabsensSel$TSN_code == tmp[i])
}
any(res)
## No TSN code in WKABSENS list

## add scientific names to survey
specDat <- wkabsens[!is.na(wkabsens$AphiaID),c("Species","AphiaID")]
specDat <- specDat[!duplicated(specDat$AphiaID),]
survey <- left_join(survey, specDat, by='AphiaID')


### Code to integrate from Anna on species bycatch corrections
survey <- survey %>%
  mutate(Species = recode(Species, 'Synaphobranchus kaupii'='Synaphobranchus kaupi',
                          'Dipturus batis'='Dipturus spp','Dipturus flossada'='Dipturus spp',
                          'Dipturus batis-complex'='Dipturus spp','Dipturus intermedia'='Dipturus spp',
                          'Dipturus'='Dipturus spp','Liparis montagui'='Liparis spp',
                          'Liparis liparis'='Liparis spp','Liparis liparis liparis'='Liparis spp',
                          'Chelon aurata'='Chelon spp','Chelon ramada'='Chelon spp',
                          'Mustelus mustelus/asterias'='Mustelus spp','Mustelus'='Mustelus spp',
                          'Mustelus mustelus'='Mustelus spp','Mustelus asterias'='Mustelus spp',
                          'Alosa'='Alosa spp','Alosa alosa'='Alosa spp','Alosa fallax'='Alosa spp',
                          'Argentina'='Argentina spp','Argentinidae'='Argentina spp',
                          'Argentina silus'='Argentina spp','Argentina sphyraena'='Argentina spp',
                          'Callionymus reticulatus'='Callionymus spp','Callionymus maculatus'='Callionymus spp',
                          'Ciliata mustela'='Ciliata spp','Ciliata septentrionalis'='Ciliata spp',
                          'Gaidropsarus'='Gaidropsarus spp','Gaidropsaurus macrophthalmus'='Gaidropsarus spp',
                          'Gaidropsaurus mediterraneus'='Gaidropsarus spp',
                          'Gaidropsaurus vulgaris'='Gaidropsarus spp',
                          'Sebastes'='Sebastes spp','Sebastes norvegicus'='Sebastes spp',
                          'Sebastes mentella'='Sebastes spp',
                          'Sebastes marinus'='Sebastes spp','Syngnathus'='Syngnatus spp',
                          'Syngnathus rostellatus'='Syngnatus spp','Syngnathus acus'='Syngnatus spp',
                          'Syngnathus typhle'='Syngnatus spp','Nerophis ophidion'='Syngnatus spp',
                          'Pomatoschistus'='Pomatoschistus spp','Pomatoschistus microps'='Pomatoschistus spp',
                          'Pomatoschistus minutus'='Pomatoschistus spp','Pomatoschistus pictus'='Pomatoschistus spp',
                          'Lesueurigobius'='Gobius spp','Gobius cobitis'='Gobius spp','Gobius niger'='Gobius spp',
                          'Leusueurigobius friesii'='Gobius spp','Neogobius melanostomus'='Gobius spp',
                          'Neogobius'='Gobius spp')) %>%
    filter(
        !(BySpecRecCode==0 & Survey == 'BTS' &
          !Species %in% c('Chelidonichthys cuculus','Chelidonichthys lucerna','Eutrigla gurnardus',
                          'Gadus morhua','Limanda limanda','Lophius piscatorius',
                          'Merlangius merlangus','Microstomus kitt','Mullus surmuletus',
                          'Mustelus asterias','Pegusa lascaris','Platichthys flesus',
                          'Pleuronectes platessa','Raja brachyura','Raja clavata',
                          'Raja montagui','Scophthalmus maximus','Scophthalmus rhombus',
                          'Scyliorhinus canicula','Solea solea','Trispoterus luscus')),
        !(BySpecRecCode==0 & Survey == 'SP-NORTH' &
          !Species %in% c('Chelidonichthys lucerna','Conger conger','Eutrigla gurnardus',
                          'Galeus melastomus','Helicolenus dactylopterus','Lepidorhombus boscii',
                          'Lepidorhombus whiffiagoni','Leucoraja circularis',
                          'Leucoraja naevus','Lophius budegassa','Lophius piscatorius','Merluccius merluccius',
                          'Micromesistius poutassou','Phycis blennoides', 'Raja clavata','Raja montagui',
                          'Scomber scombrus','Scyliorhinus canicula','Trachurus trachurus',
                          'Trisopterus luscus','Zeus faber')),
        !(BySpecRecCode==0 & Survey == 'SP-PORC' &
          !Species %in% c('Argentina silus','Chelidonichthys lucerna','Conger conger',
                          'Eutrigla gurnardus','Gadus morhua','Galeus melastomus',
                          'Glyptocephalus cynoglossu','Helicolenus dactylopterus','Hexanchus griseus',
                          'Hippoglossoides platessoi','Lepidorhombus boscii','Lepidorhombus whiffiagoni',
                          'Leucoraja circularis','Leucoraja naevus', 'Lophius budegassa','Lophius piscatorius',
                          'Melanogrammus aeglefinus','Merluccius merluccius','Micromesistius poutassou',
                          'Molva dypterygia','Molva molva','Phycis blennoides','Raja clavata',
                          'Raja montagui','Scomber scombrus','Scyliorhinus canicula',
                          'Trachurus trachurus','Zeus faber')),
        !(BySpecRecCode==0 & Survey %in% c('NS-IBTS1','NS-IBTS3') &
          !Species %in% c('Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua',
                          'Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii')),
        !(BySpecRecCode==2 &
          !Species %in% c('Ammodytidae','Anarhichas lupus','Argentina silus','Argentina sphyraena',
                          'Chelidonichthys cuculus','Callionymus lyra','Eutrigla gurnardus',
                          'Lumpenus lampretaeformis', 'Mullus surmuletus','Squalus acanthias',
                          'Trachurus trachurus', 'Platichthys flesus','Pleuronectes platessa','Limanda limanda',
                          'Lepidorhombus whiffiagoni','Hippoglossus hippoglossus','Hippoglossoides platessoi',
                          'Glyptocephalus cynoglossu','Microstomus kitt','Scophthalmus maximus',
                          'Scophthalmus rhombus','Solea solea', 'Pollachius virens','Pollachius pollachius',
                          'Trisopterus luscus','Trisopterus minutus','Micromesistius poutassou','Molva molva',
                          'Merluccius merluccius','Brosme brosme', 'Clupea harengus','Sprattus sprattus',
                          'Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus' ,'Merlangius merlangus',
                          'Trisopterus esmarkii')),
        !(BySpecRecCode==3 &
          !Species %in% c('Pollachius virens','Pollachius pollachius','Trisopterus luscus','Trisopterus minutus',
                          'Micromesistius poutassou','Molva molva', 'Merluccius merluccius','Brosme brosme',
                          'Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua',
                          'Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii')),
        !(BySpecRecCode==4 &
          !Species %in% c('Platichthys flesus','Pleuronectes platessa','Limanda limanda',
                          'Lepidorhombus whiffiagoni','Hippoglossus hippoglossus','Hippoglossoides platessoi',
                          'Glyptocephalus cynoglossu','Microstomus kitt','Scophthalmus maximus','Scophthalmus rhombus',
                          'Solea solea', 'Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua',
                          'Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii')),
        !(BySpecRecCode==5 &
          !Species %in% c('Ammodytidae','Anarhichas lupus','Argentina silus','Argentina sphyraena',
                          'Chelidonichthys cuculus','Callionymus lyra','Eutrigla gurnardus',
                          'Lumpenus lampretaeformis', 'Mullus surmuletus','Squalus acanthias','Trachurus trachurus',
                          'Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua',
                          'Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii'))
    )
print("done")



##########################################################################################
#### More cleaning
##########################################################################################

## Some data transformation
## ---------
survey <- survey %>%
    mutate(
        abstime = Year+(Month-1)*1/12+(Day-1)/365,
        timeOfYear = (Month-1)*1/12+(Day-1)/365,
        TimeShotHour = as.integer(TimeShot/100) + (TimeShot%%100)/60,
        Depth = replace(Depth, Depth<0, NA)) %>%
    ## two different gear sizes are used in the Baltic -
    ##they cannot converted reliably for sensitives, so they are treated as separate surveys in my analysis.
    ## mutate(Survey = if_else(Survey=='BITS' & Gear=='TVS', 'BITSS', Survey)) %>%
    ## mutate(Survey = if_else(Survey=='BITS' & Gear=='TVL', 'BITSL', Survey)) %>%
    as.data.frame()
survey$Ship <- as.factor(survey$Ship)
survey$Gear <- as.factor(survey$Gear)
survey$ShipG <- factor(paste(survey$Ship, survey$Gear, sep=":"))
survey$Species <- as.character(survey$Species)



## Remove surveys with issues:
survey <- survey %>%
  filter(!(Survey=='NS-IBTS' & Quarter %in% c(2,4)), ## 0
         ## !(Survey=='NS-IBTS' & Quarter==1 & Year<1967), ## 0
         ## !(Survey=='SWC-IBTS' & Quarter %in% c(2,3)), ## 3300
         ## !(Survey=='BITSS' & Quarter %in% c(2,3)),  ## 0
         ## !(Survey %in% c('BITS')), ## 667825  ## BITS and Gear not TVS or TVL ## before: removing BITSL, why?
         ## !(Survey=='BTS' & Year<1987),    ## 7618
         ## !(Survey=='IE-IGFS' & Year<2003),  ## 0
         !(Survey=='NIGFS' & Year<2006)  ## 1 ## IGFS survey only sampled at depths greater than 200m from 2005 to present.
         )


## Double-check key variables
any(is.na(survey$Year))
any(is.na(survey$Lat))
any(is.na(survey$Lon))
any(is.na(survey$timeOfYear))
any(is.na(survey$Depth))
any(is.na(survey$SweptArea))
any(survey$SweptArea == 0)
any(is.na(survey$DayNight))
any(is.na(survey$DepthStratum))
length(unique(survey$HaulID[is.na(survey$DepthStratum)])) ## many NAs (useful?)



##########################################################################################
#### ZERO DATA
##########################################################################################
## Dummy survey with zeros (important so that all Survey * Year * Quarter combis are included (with 0)),
## even if there where no individuals caught
survey$N <- 0
survey0 <- survey %>%
    dplyr::select(-Species, -AphiaID, -SpecCode, -SpecCodeType) %>%
    group_by(HaulID, Survey, Year, Month, Day, Quarter, StatRec, Lat,
             Lon, HaulDur, Ship, Gear, Depth, DayNight,
             SweptArea, DepthStratum, ShipG, GearCat, Ecoregion, Area_27,
             BySpecRecCode, TimeShotHour, abstime, timeOfYear) %>%
    summarise_at(.vars = c("N"),
                 .funs = function(x) sum(x, na.rm = TRUE)) %>%
    dplyr::rename(haul.id = HaulID) %>%
    dplyr::select(haul.id, Survey, Year, Month, Day, Quarter, StatRec, Lat,
                  Lon, HaulDur, Ship, Gear, Depth, DayNight,
                  SweptArea, DepthStratum, ShipG, GearCat, Ecoregion, Area_27,
                  BySpecRecCode, TimeShotHour, abstime, timeOfYear)
survey0$N <- 0
survey0 <- as.data.frame(survey0)

tmp <- survey0 %>%
    dplyr::select(Survey, Quarter, Year, N) %>%
    group_by(Survey, Quarter, Year) %>%
        summarise_at(.vars = c("N"),
                     .funs = function(x) sum(x, na.rm = TRUE)) %>%
    as.data.frame()
tmp <- aggregate(list(Years=tmp$Year), by = list(Survey = tmp$Survey, Quarter = tmp$Quarter), range)
tmp <- tmp[order(tmp$Survey),]
tmp[,3] <- apply(tmp[,3],1,paste,collapse="-")

write.xlsx(tmp, file = "results/input_data.xlsx")

capture.output(
    print(
        xtable(tmp[1:15,],
               align = c("l","l","c","c")),
        include.rownames = FALSE,
        include.colnames = TRUE,
        hline.after = c(-1,0,2,5,6,8,9,10,11,13,15),
        sanitize.text.function = identity,
        caption.placement = "top"),
    file="results/input_data1.tex",
    append=FALSE)

capture.output(
    print(
        xtable(tmp[16:nrow(tmp),],
               align = c("l","l","c","c")
               ),
        include.rownames = FALSE,
        include.colnames = TRUE,
        hline.after = c(-1,0,1,2,4,6,8,10,12),
        sanitize.text.function = identity,
        floating = TRUE,
        caption.placement = "top"),
    file="results/input_data2.tex",
    append=FALSE)



##########################################################################################
#### SUBSET DATA, SUM UP COUNTS
##########################################################################################

## AphiaID required by WKABSENS
## remove species with AphiaID == NA
aphia_req <- wkabsens[!is.na(wkabsens$AphiaID),]

## Species in WKABSENS list that could not be matched:
tmp <- cbind(Species = wkabsens$Species[which(is.na(wkabsens$AphiaID))],
             AphiaID = NA,
             Comment = "AphiaID is NA")
tmp <- rbind(tmp, cbind(Species = aphia_req[which(!aphia_req$AphiaID %in% survey$AphiaID),"Species"],
                        AphiaID = aphia_req[which(!aphia_req$AphiaID %in% survey$AphiaID),"AphiaID"],
                        Comment = "AphiaID not in DATRAS"))

write.xlsx(tmp, file = "results/species_not_matched.xlsx")



## Subset based on WKABSENS list
## ---------
survey <- subset(survey, survey$AphiaID %in% aphia_req$AphiaID)


## Day and Night
## ---------
unique(survey$DayNight)
## meaningful


## HaulDur
## ---------
unique(survey$HaulDur)
range(survey$HaulDur)
## meaningful


## Swept Area
## ---------
range(survey$SweptArea)
## meaningful


## SpecVal
## ---------
## SpecVal (5,6) only useful for presence-absence
unique(survey$SpecVal)
ind <- which(survey$SpecVal %in% c(5,6))
tmp <- survey[ind,]

writeLines(paste0("Species with SpecVal %in% c(5,6): "))
tmp <- as.data.frame(table(tmp$Species))
colnames(tmp) <- c("Species", "Frequency")
print(tmp)
write.xlsx(tmp, file = "results/species_with_specVal_5_or_6.xlsx")
writeLines(paste0("Removing these for now."))
survey <- survey[-ind,]


## HLNoAtLngt
## -------------
ind <- which(is.na(survey$HLNoAtLngt))
writeLines(paste0("Entries with HLNoAtLngt == NA: ",length(ind)))
survey$LngtClass[ind] ## None of them has LngtClass info
survey$TotalNo[ind]  ## Most of them have still TotalNo info!
survey$SubFactor[ind]

tmp <- cbind(paste0(survey$HaulID[ind],"-",survey$AphiaID[ind]),
             survey$CatIdentifier[ind], survey$TotalNo[ind],
             survey$NoMeas[ind], survey$SubFactor[ind])

## DECISION: Using TotalNo when HLNoAtLngt == NA and setting subFactor to 1.
## only two duplicated but Sex different: => summing up okay
survey[which(paste0(survey$HaulID, "-", survey$AphiaID) %in% tmp[which(duplicated(tmp[,1]))][1]),]
survey[which(paste0(survey$HaulID, "-", survey$AphiaID) %in% tmp[which(duplicated(tmp[,1]))][2]),]

survey$HLNoAtLngt[ind] <- survey$TotalNo[ind]
survey$SubFactor[ind] <- 1


## SubFactor
## -------------
ind <- which(is.na(survey$SubFactor))
writeLines(paste0("Entries with SubFactor == NA: ",length(ind)))
ind <- which(survey$SubFactor == 0)
writeLines(paste0("Entries with SubFactor == 0: ",length(ind)))
range(survey$SubFactor)  ## high values ... realistic? keeping them for now

unique(survey$Species[survey$SubFactor > 20])


## Using HLNoAgeLngt for now
## -------------------
## account for datatype
## https://www.ices.dk/data/Documents/DATRAS/DATRAS_FAQs.pdf:
## DataType R,S: TotalNo –report the total number of fish of one species, sex, and category in the given haul
## DataType C: TotalNo –report the total number of fish of one species and sex in the given haul, raised to 1 hour hauling;
survey$multiplier <- ifelse(survey$DataType=="C", survey$HaulDur/60, survey$SubFactor)  ## not using SubFactor if DataType == "C"
survey$N <- survey$HLNoAtLngt * survey$multiplier
## sum up counts
survey <- survey %>%
    group_by(HaulID, Survey, Year, Month, Day, Quarter, StatRec, Lat,
             Lon, HaulDur, Ship, Gear, Depth, Species, AphiaID,
             DayNight, SweptArea, DepthStratum, ShipG, GearCat, Ecoregion, Area_27,
             BySpecRecCode, TimeShotHour, abstime, timeOfYear) %>%
    summarise_at(.vars = c("N"),
                 .funs = function(x) sum(x, na.rm = TRUE)) %>%
    mutate(N = round(N)) %>%
    dplyr::rename(haul.id = HaulID)
survey <- as.data.frame(survey)



##################
if(download){
    ## creating taxonomy tables for each species
    my_sp_taxo <- wm_record_(id = aphia_list)
    save(my_sp_taxo, file = "data/my_sp_taxo.RData", version=2)
}else{
    load("data/my_sp_taxo.RData")
}



worms_df <- data.frame(do.call(rbind, my_sp_taxo))[,c("AphiaID","class")]
#check if all AphiaID from survey identified
nrow(subset(worms_df, is.na(worms_df$valid_name))) # ok
colnames(worms_df) <- c("AphiaID", "class")
any(duplicated(worms_df$AphiaID))
survey <- left_join(survey, worms_df, by='AphiaID')
## unique(survey$Species[which(survey$class == "Elasmobranchii")])



##########################################################################################
#### SAVE DATA
##########################################################################################
save(survey, survey0, file='data/DATRAS_26_06_2021.RData', version=2)
