#############################################################
## Abundance index for rare species - GAMs
## Tobias Mildenberger, June 2021
#############################################################
setwd("../")
getwd()


##
saveFig <- FALSE
source("code/extra.functions.R")
ncores <- 6
last.year <- 2020


##########################################################################################
#### Libraries
##########################################################################################

## Former way
## ------------------
library(dplyr)
library(ggplot2)
library(egg)
library(RColorBrewer)
library(rgdal)
library(tidyr)
## library(tidyverse)
## instead:
library(dplyr)
require(tibble)
## library(sjstats)
library(msir)
## packages "maps", "rgeos" need to be installed
library(lubridate)
library(mgcv)
library(maptools)
library(mapdata)
library(openxlsx)
library(xtable)
library(statmod)
library(viridis) ## scale_color_viridis()
library(showtext)
library(unix)
library(pryr)

## increase memory on linux
rlimit_as(1e12, 1e12)

## something wrong with my plotting
showtext_auto()

## GAMs
## ------------------
## remotes::install_github("DTUAqua/DATRAS/DATRAS")
library(DATRAS)
## remotes::install_github("casperwberg/surveyIndex/surveyIndex")
library(surveyIndex)
library(parallel)
library(MASS) ## surveyIndex should require MASS
library(itsadug)


##########################################################################################
#### Load data
##########################################################################################
#### Load species abundances across surveys
load('data/DATRAS_26_06_2021.RData')  ## CHECK: using newest file

#### Load ICES rectangles shapefiles
rect <- readOGR(dsn = "data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp",
                layer="ICES_Statistical_Rectangles_Eco")
## subset shapefile for ICES rectangles sampled by the surveys
surveyed.rect <- subset(rect, ICESNAME %in% unique(survey0$StatRec))


## any duplicated HaulID?
any(duplicated(survey$HaulID))

wkabsens <- read.csv("data/wkabsens_species_list.csv")
length(unique(wkabsens$Species))  ## but still some duplication? Squalus vs. Squalus acanthias

## to start with use most abundant species (HaulIDs)
haulsPerSpecies <- aggregate(list(Hauls = survey$haul.id),
                             by=list(Species=survey$Species,
                                     class = survey$class),
                             function(x) length(unique(x)))
haulsPerSpecies <- haulsPerSpecies[order(haulsPerSpecies$Hauls),]
nrow(haulsPerSpecies[haulsPerSpecies$Hauls < 100,])
nrow(haulsPerSpecies)
nrow(haulsPerSpecies[haulsPerSpecies$Hauls >= 100,])

cbind(sort(haulsPerSpecies$Species[haulsPerSpecies$Hauls >= 100]))



## Priority species lists:
## --------------------------
## 100 - 500 hauls
speciesSel_low <- haulsPerSpecies$Species[haulsPerSpecies$Hauls >= 100 &
                                          haulsPerSpecies$Hauls < 500]
## 500 - 1000 hauls
speciesSel_mid <- haulsPerSpecies$Species[haulsPerSpecies$Hauls >= 500 &
                                          haulsPerSpecies$Hauls < 1000]
## > 1000 hauls
speciesSel_high <- haulsPerSpecies$Species[haulsPerSpecies$Hauls >= 1000]

## all of them
speciesSel <- haulsPerSpecies$Species[haulsPerSpecies$Hauls >= 100]




## Missing years (NA in stratMean)
## ----------------
tmp <- aggregate(list(Years = survey0$Year),
                 by=list(Survey=survey0$Survey,Quarter=survey0$Quarter),
                 unique)
tmp <- tmp[order(tmp$Survey),]
tmp2 <- cbind(tmp[,1:2],
              should = sapply(tmp$Years,max) - sapply(tmp$Years,min) + 1,
              is = sapply(tmp$Years,length))
as.vector(apply(tmp2[which(tmp2[,3] != tmp2[,4]),1:2],1,paste,collapse="-"))
## Problem with:
## [1] "DYFS-3"     "DYFS-4"     "NIGFS-4"    "PT-IBTS-4"  "ROCKALL-3"
## [6] "SNS-3"      "SNS-4"      "SP-ARSA-1"  "SP-ARSA-4"  "SP-NORTH-3"
## [11] "SP-NORTH-4" "SP-PORC-3"  "SWC-IBTS-4"

## Ship == NA -> REMOVE
ind <- is.na(survey$Ship)
length(ind)
unique(survey$Year[ind])
unique(survey$Quarter[ind])
unique(survey$Gear[ind])
unique(survey$Area_27[ind])
unique(survey$Survey[ind])
survey <- survey[-ind,]




##########################################################################################
#### Build abundance indices for selected species
##########################################################################################
Survey <- sort(unique(survey$Survey))
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(length(Survey))
cols <- data.frame(cbind(Survey, mycolors))


## Stocks with several populations or other specific considerations
multPop <- c("Anarhichas lupus",
             "Chelidonichthys lucerna",
             "Cyclopterus lumpus",
             "Leucoraja naevus",
             "Scophthalmus rhombus",
             "Scyliorhinus canicula")


## Create folder for results
specDirAll <- paste0("results/species_",format(Sys.Date(),"%Y_%m_%d"),"/")
dir.create(file.path(specDirAll), showWarnings = FALSE)


speciesSel1 <- 1:length(speciesSel)

## loop through all species
## ---------------------
for (i in speciesSel1){

    ## species
    species <- speciesSel[i]
    specLab <- gsub(" ", "_",species)

    writeLines(paste0("\nSpecies ",i,": ",species,"\n"))


    ## Account for spatial assessment units
    stockLab <- "OnePop"
    nstocks <- ifelse(species %in% multPop, 2, 1)


    ## Loop through stocks
    ## ---------------------
    for(stock in 1:nstocks){


        ## 1. Subset species
        ## --------------------------------------
        survey.spp <- survey %>%
            filter(Species==species)
        survey.spp0 <- survey0


        ## 2. Subset stocks (if applicable)
        ## --------------------------------------
        if(species == "Anarhichas lupus"){
            ## One population in the North Sea/Skagerrak/Kattegat and a separate
            ## (part of a) population at Rockall (west of -10oE).
            if(stock == 1){
                stockLab <- "NorthSea"
                survey.spp <- survey.spp %>%
                    filter(Lon >= -10)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lon >= -10)
            }else if(stock == 2){
                stockLab <- "Rockall"
                survey.spp <- survey.spp %>%
                    filter(Lon < -10)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lon < -10)
            }
        }else if(species == "Chelidonichthys lucerna"){
            ## One population off the Spanish/Portuguese coast and another in the
            ## northern Celtic Seas. The two were divided at the northern border of
            ## the Bay of Biscay (48oN).
            if(stock == 1){
                stockLab <- "North48"
                survey.spp <- survey.spp %>%
                    filter(Lat > 48)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lat > 48)
            }else if(stock == 2){
                stockLab <- "South48"
                survey.spp <- survey.spp %>%
                    filter(Lat <= 48)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lat <= 48)
            }
        }else if(species == "Cyclopterus lumpus"){
            ## Genetic evidence suggests two separate populations in the central and
            ## western Baltic/Kattegat/Skagerrak (8-13.5oE and east of 13.5 oE).
            ## Sporadic elsewhere.
            if(stock == 1){
                stockLab <- "WesternBaltic"
                survey.spp <- survey.spp %>%
                    filter(Lon >= 8 & Lon < 13.5)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lon >= 8 & Lon < 13.5)
            }else if(stock == 2){
                stockLab <- "CentralBaltic"
                survey.spp <- survey.spp %>%
                    filter(Lon >= 13.5)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lon >= 13.5)
            }
        }else if(species == "Leucoraja naevus"){
            ## Distribution seems to reflect one southern population around southern
            ## Portugal and Spain, a mid-latitude population in the Bay of Biscay and
            ## southern Celtic Sea and a northern population in the North Sea and
            ## west of the UK and Ireland. The current ICES advice assumes one
            ## population from the Bay of Biscay and north. Therefore, the
            ## populations were divided north and south of 42oN. However, this
            ## decisions should be revisited in the future as more knowledge becomes
            ## available. HERE: CHECK: TOBIAS: SP-NORTH/SPARSA catches are not apparent?
            if(stock == 1){
                stockLab <- "North42"
                survey.spp <- survey.spp %>%
                    filter(Lat > 42)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lat > 42)
            }else if(stock == 2){
                stockLab <- "South42"
                survey.spp <- survey.spp %>%
                    filter(Lat <= 42)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lat <= 42)
            }
        }else if(species == "Scophthalmus rhombus"){
            ## Assessed species in Baltic Sea and North Sea/Channel. Separate west of
            ## UK index will be produced by WKABSENS (surveys other than the North
            ## Sea and Baltic Sea surveys).
            if(stock == 1){
                stockLab <- "NorthSea"
                survey.spp <- survey.spp %>%
                    filter(Ecoregion != "Bay of Biscay and the Iberian Coast" &
                           Ecoregion != "Celtic Seas")
                survey.spp0 <- survey.spp0 %>%
                    filter(Ecoregion != "Bay of Biscay and the Iberian Coast" &
                           Ecoregion != "Celtic Seas")
            }else if(stock == 2){
                stockLab <- "CelticSeas"
                survey.spp <- survey.spp %>%
                    filter(Ecoregion != "Bay of Biscay and the Iberian Coast" &
                           Ecoregion == "Celtic Seas")
                survey.spp0 <- survey.spp0 %>%
                    filter(Ecoregion != "Bay of Biscay and the Iberian Coast" &
                           Ecoregion == "Celtic Seas")
            }
        }else if(species == "Scyliorhinus canicula"){
            ## This species is likely to have several substocks but knowledge of this
            ## is too limited within the group to use the information in this
            ## analysis. WKABSENS continued with sepreate evaluations north and south
            ## of 48oN.
            if(stock == 1){
                stockLab <- "North48"
                survey.spp <- survey.spp %>%
                    filter(Lat > 48)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lat > 48)
            }else if(stock == 2){
                stockLab <- "South48"
                survey.spp <- survey.spp %>%
                    filter(Lat <= 48)
                survey.spp0 <- survey.spp0 %>%
                    filter(Lat <= 48)
            }
        }


        ## 2. Gears and surveys
        ## --------------------------------------
        ## Use Gear category rather than many individual gears
        survey.spp$Gear <- as.factor(survey.spp$GearCat)
        survey.spp0$Gear <- as.factor(survey.spp0$GearCat)
        survey.spp$ShipG <- factor(paste0(survey.spp$Ship,":",survey.spp$GearCat))
        survey.spp0$ShipG <- factor(paste0(survey.spp0$Ship,":",survey.spp0$GearCat))
        ## More gears in zero data set!
        length(unique(survey.spp$Gear))
        length(unique(survey.spp0$Gear))
        ## Only keep gears with more or equal 5 individual hauls
        tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                         by = list(Gear = survey.spp$Gear[survey.spp$N > 0]),
                         function(x) length(unique(x)))
        ## print(tmp)
        gears.keep <- as.character(tmp$Gear[tmp$haul.id >= 1])  ## HERE:

        ## More surveys in zero data set!
        length(unique(survey.spp$Survey))
        length(unique(survey.spp0$Survey))
        ## Only keep surveys with more or equal 5 individual hauls
        tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                         by = list(Survey = survey.spp$Survey[survey.spp$N > 0]),
                         function(x) length(unique(x)))
        ## print(tmp)
        surveys.keep <- as.character(tmp$Survey[tmp$haul.id >= 1])

        ## Check Ship-Gear
        tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                         by = list(ShipG = survey.spp$ShipG[survey.spp$N > 0]),
                         function(x) length(unique(x)))
        ## print(tmp)


        ## Species-specific
        if(species == "Galeus spp."){
            ## Keep SP-PORC not matter what
            surveys.keep <- unique(c(surveys.keep,"SP-PORC"))
        }


        ## 2. Realized habitat of species
        ## --------------------------------------
        realized.habitat <- survey.spp %>%
            group_by(StatRec) %>%
            summarize(N=1)
        ## only keep ices squares where species is present (COMMENT: kind of
        ## unnessary as only those StatRec are present in data anyways)
        ices.keep <- sort(unique(realized.habitat$StatRec))


        ## Apply selections to both data sets
        ## --------------------------------------
        survey.spp <- survey.spp %>%
            filter(StatRec %in% ices.keep,
                   Gear %in% gears.keep,
                   Survey %in% surveys.keep)

        survey.spp0 <- survey.spp0 %>%
            filter(StatRec %in% ices.keep,
                   Gear %in% gears.keep,
                   Survey %in% surveys.keep)


        ## 3. Remove years before first occurrence
        ## --------------------------------------
        first_occurence <- min(survey.spp$Year)
        survey.spp0 <- survey.spp0 %>%
            filter(Year >= first_occurence)


        ## 4. Missing / fragmented years
        ## --------------------------------------
        cont <- TRUE
        while(cont){
            obs <- unique(survey.spp$Year)
            all <- seq(min(survey.spp$Year),
                       max(survey.spp$Year), 1)
            mis <- all[!all %in% obs]
            print(paste0("First year: ",min(survey.spp$Year)))
            print("Missing years:")
            print(mis)
            tmp <- merge(data.frame(Year=all,
                                    all = 0),
                         data.frame(Year=obs,
                                    obs = 1),
                         by = "Year", all.x = TRUE)
            tmp$obs[is.na(tmp$obs)] <- tmp$all[is.na(tmp$obs)]
            tmp2 <- rle(tmp$obs)
            if(tmp2$lengths[1] == 1 && tmp2$lengths[2] > 1){
                print(paste0("First year (",min(tmp$Year),
                             ") has observations than at least two years with no observations. Removing these years and checking again!"))
                keepYears <- tmp$Year[min(which(tmp$obs == 1)[-1])]
                survey.spp <- survey.spp %>%
                    filter(Year >= keepYears)
                survey.spp0 <- survey.spp0 %>%
                    filter(Year >= keepYears)
            }else{
                cont <- FALSE
            }
        }


        ## 5. Re-check gears and rectangles
        ## --------------------------------------
        ## Only keep gears with more or equal 5 individual hauls
        tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                         by = list(Gear = survey.spp$Gear[survey.spp$N > 0]),
                         function(x) length(unique(x)))
        ## print(tmp)
        gears.keep <- as.character(tmp$Gear[tmp$haul.id >= 1]) ## HERE:
        ## habitat
        realized.habitat <- survey.spp %>%
            group_by(StatRec) %>%
            summarize(N=1)
        ices.keep <- sort(unique(realized.habitat$StatRec))
        ## Apply selections to both data sets
        survey.spp <- survey.spp %>%
            filter(StatRec %in% ices.keep,
                   Gear %in% gears.keep,
                   Survey %in% surveys.keep)
        survey.spp0 <- survey.spp0 %>%
            filter(StatRec %in% ices.keep,
                   Gear %in% gears.keep,
                   Survey %in% surveys.keep)


        ## 6. Combine surveys with observations and zeros
        ## --------------------------------------
        survey.spp2 <- merge(survey.spp0, survey.spp,
                             by=c('haul.id',"Survey","StatRec","Year","Month",
                                  "Day","Quarter","Lat","Lon",
                                  "HaulDur","Ship","Gear", "ShipG", "GearCat",
                                  "Depth", "SweptArea","DepthStratum","DayNight",
                                  "BySpecRecCode","TimeShotHour",
                                  "Ecoregion","Area_27",
                                  "abstime","timeOfYear"),
                             all.x = TRUE)
        ##
        survey.spp2$Species <- unique(survey.spp$Species)
        ##
        survey.spp3 <- survey.spp2 %>%
            dplyr::select(-N.x, -N.y)
        ##
        vec <- c("N")
        df <- vector("list",length(vec))
        for(nu in 1:length(vec)){
            ind <- c(grep(paste0(vec[nu],".x"),colnames(survey.spp2)),
                     grep(paste0(vec[nu],".y"),colnames(survey.spp2)))
            df[[nu]] <- rowSums(survey.spp2[,ind], na.rm=TRUE)
        }
        df <- do.call(cbind, df)
        colnames(df) <- vec
        ##
        survey.spp <- cbind(survey.spp3, df)
        ##
        rm(survey.spp0, survey.spp3, survey.spp2)


        ## 7. Species specific considerations
        ## --------------------------------------
        if(species == "Alosa spp"){
            ## A separate population in the Baltic and Kattegat (east of 7oE).
            ## Sporadic in other areas and likely too sparse to allow indices to be
            ## reliably estimated outside the Baltic Sea/Kattegat.
            survey.spp <- survey.spp %>%
                filter(Lon >= 7)
        }else if(species == "Amblyraja radiata"){
            ## One population with the distribution centred in the North Sea.
            ## According to WGEF, individuals recorded west of UK and in the Channel
            ## are likely to be misidentified R. clavata and surveys in these areas
            ## should not be included.
            survey.spp <- survey.spp %>%
                filter(!(Lon < -6),
                       !(Lon < -4 & Lat < 60),
                       !(Lon < -3 & Lat < 55)
                       )
        }



        ## 6. Some checks
        ## --------------------------------------
        ## - No NA in Lat/Lon?
        ind <- which(is.na(survey.spp$Lat))
        if(length(ind) > 0){
            writeLines(paste0(length(ind)," hauls have Lat == NA. Omitting them."))
            survey.spp <- survey.spp[-ind,]
        }
        ind <- which(is.na(survey.spp$Lon))
        if(length(ind) > 0){
            writeLines(paste0(length(ind)," hauls have Lon == NA. Omitting them."))
            survey.spp <- survey.spp[-ind,]
        }
        ## - Duplicates in data?
        ind <- which(duplicated(survey.spp$haul.id))
        if(length(ind) > 0){
            writeLines(paste0("Duplicated haul ID in data set. Omitting all duplicates."))
            survey.spp <- survey.spp[-ind,]
        }
        ## - N all integers?
        tmp <- unique(survey.spp$N)
        ind <- which(tmp %% 1 != 0)
        if(length(ind) > 0){
            writeLines(paste0("Following non-integers found in total numbers per haul: ",
                              paste(tmp[ind],collapse=", "),
                              ". Rounding to integers!"))
            survey.spp$N <- round(survey.spp$N)
        }
        ## - Realistic maximum number by haul?
        writeLines(paste0("Maximum total number is: ",max(survey.spp$N, na.rm = TRUE),
                          ", and allocated to following haul:\n"))
        print(survey.spp[which(survey.spp$N == max(survey.spp$N, na.rm = TRUE)),])
        ## - Remove all Total numbers above 3000?
        ## ind <- which(survey.spp$N > 3000)
        ## if(length(ind) > 0) survey.spp <- survey.spp[-ind,]
        ## - Print remaining hauls
        nHaulsPos <- length(unique(survey.spp$haul.id[which(survey.spp$N > 0)]))
        writeLines(paste0("Number of hauls with N>0: ", nHaulsPos))
        nHauls0 <- length(unique(survey.spp$haul.id[which(survey.spp$N == 0)]))
        writeLines(paste0("Number of hauls with N==0: ", nHauls0))
        nHaulsTot <- length(unique(survey.spp$haul.id))
        writeLines(paste0("Number of hauls total: ", nHaulsTot))


        ## Check that substocks still have enough observations (>=100)
        ## --------------------------------------
        ind <- length(unique(survey.spp$haul.id[which(survey.spp$N > 0)]))
        print(ind)
        if(ind < 100){
            print(paste0("Substock ", species," - ",stockLab," has less than 100 observations. Skipping."))
            next()
            ## Only Anarhichas lupus - Rockall so far.
        }else{
            print("More than 100 observations. Moving on")
        }



        ## years
        ## --------------------------------------
        survey.spp$ctime <- as.numeric(as.character(survey.spp$Year)) +
            round(survey.spp$timeOfYear,1)

        ## Numbers
        ## --------------------------------------
        Nage <- as.matrix(survey.spp$N, na.rm=TRUE)
        colnames(Nage) <- "1"
        survey.spp$Nage <- Nage
        ages <- 1
        ## withweight (download CA)  d<-addWeightByHaul(d,to1min=FALSE) but not clear if enough info
        ## maybe future project



        ## 6. Stratified mean
        ## --------------------------------------
        stratMean <- try(
            getSurveyIdxStratMean_df(survey.spp, 1),
            silent = TRUE
        )




        ## 7. Presence-absence model
        ## --------------------------------------
        if(length(unique(survey.spp$Gear)) > 1 && length(unique(survey.spp$ShipG)) > 1){

            suc <- aggregate(list(successes=survey.spp$haul.id[survey.spp$N > 0]),
                             by = list(ctime = survey.spp$ctime[survey.spp$N > 0],
                                       Gear = survey.spp$Gear[survey.spp$N > 0],
                                       ShipG = survey.spp$ShipG[survey.spp$N > 0]),
                             function(x) length(unique(x)))
            trials <- aggregate(list(trials=survey.spp$haul.id),
                                by = list(ctime = survey.spp$ctime,
                                          Gear = survey.spp$Gear,
                                          ShipG = survey.spp$ShipG),
                                function(x) length(unique(x)))
            dat <- merge(trials, suc, by = c("ctime","Gear","ShipG"), all=TRUE)
            dat$successes[is.na(dat$successes)] <- 0

            bino <- try(
                gam(successes/trials ~ s(ctime, bs=c('ds'), k=c(12)) + Gear + s(ShipG, bs='re'),
                    data = dat,
                    family = binomial("logit"),
                    weights = trials),
                silent = TRUE
            )

        }else if(length(unique(survey.spp$ShipG)) > 1){

            writeLines(paste0("Not using Gear as variable - only one gear used."))

            suc <- aggregate(list(successes=survey.spp$haul.id[survey.spp$N > 0]),
                             by = list(ctime = survey.spp$ctime[survey.spp$N > 0],
                                       ShipG = survey.spp$ShipG[survey.spp$N > 0]),
                             function(x) length(unique(x)))
            trials <- aggregate(list(trials=survey.spp$haul.id),
                                by = list(ctime = survey.spp$ctime,
                                          ShipG = survey.spp$ShipG),
                                function(x) length(unique(x)))
            dat <- merge(trials, suc, by = c("ctime","ShipG"), all=TRUE)
            dat$successes[is.na(dat$successes)] <- 0

            bino <- try(
                gam(successes/trials ~ s(ctime, bs=c('ds'), k=c(12)) + s(ShipG, bs='re'),
                    data = dat,
                    family = binomial("logit"),
                    weights = trials),
                silent = TRUE
            )

        }else{

            writeLines(paste0("Not using Gear nor ShipG as variable - only one gear and ShipG used."))

            suc <- aggregate(list(successes=survey.spp$haul.id[survey.spp$N > 0]),
                             by = list(ctime = survey.spp$ctime[survey.spp$N > 0]),
                             function(x) length(unique(x)))
            trials <- aggregate(list(trials=survey.spp$haul.id),
                                by = list(ctime = survey.spp$ctime),
                                function(x) length(unique(x)))
            dat <- merge(trials, suc, by = c("ctime"), all=TRUE)
            dat$successes[is.na(dat$successes)] <- 0

            bino <- try(
                gam(successes/trials ~ s(ctime, bs=c('ds'), k=c(12)),
                    data = dat,
                    family = binomial("logit"),
                    weights = trials),
                silent = TRUE
            )

        }

        ## Create dir
        ## -------------
        ## species dir
        dir.create(file.path(paste0(specDirAll,specLab,"/")), showWarnings = FALSE)
        ## stock dir
        specDir <- paste0(specDirAll,specLab,"/",stockLab,"/")
        dir.create(file.path(specDir), showWarnings = FALSE)

        save(survey.spp,
             file = paste0(specDir,"surveyspp_",specLab,"_",stockLab,".RData"),
             version = 2)


        ## Binomial
        if(inherits(bino,"try-error")){

            print(paste0("Error in binomial GAM with species: ", species))

            print(bino)

            plotBino <- NULL

        }else{

            save(bino,
                 file = paste0(specDir,"bino_",specLab,"_",stockLab,".RData"),
                 version = 2)

            if(saveFig){
                pdf(file=paste0(specDir,'bino_index_',specLab,"_",stockLab,'.pdf'))
                par(mfrow = c(1,1), mar = c(5,4,4,2), oma = c(0,0,0,0))
                plotBino <- plot_smooth(bino, view = "ctime", main = "",
                                        transform = plogis,
                                        print.summary = FALSE)
                dev.off()
            }

        }

        ## strat mean
        if(inherits(stratMean,"try-error")){

            print(paste0("Error in stratMean with species: ", species))

        }else{

            save(stratMean,
                 file = paste0(specDir,"stratMean_",specLab, "_",stockLab,".RData"),
                 version = 2)

        }

        ## Realised habitat
        ## ---------------------------
        ## N = 1
        map_data_fortified <- fortify(surveyed.rect, region = "ICESNAME")
        map_data <- map_data_fortified %>% left_join(realized.habitat, by = c("id" = "StatRec"))
        world <- map_data("world")
        habitat <- ggplot(data=map_data, aes(long, lat, group = group, fill = N)) +
            geom_polygon(colour = 'white',  size = 0.3) +
            scale_fill_gradient(
                low = "#2F53E1",
                high = "#2F53E1",
                na.value = "grey80") +
            geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='grey30', color="grey40") +
            coord_quickmap(xlim=c(-16,24), ylim=c(36,62)) +
            theme_bw() +
            ggtitle(species) + theme(legend.position = 'none') +
            xlab('') + ylab('')

        if(saveFig){
            png(file=paste0(specDir,'realised_habitat_',specLab, "_",stockLab,'.png'),
                width = 800, height = 800, res=120)
            print(habitat)
            dev.off()
        }

        ## with N
        realized.habitat2 <- survey.spp %>%
            group_by(StatRec) %>%
            summarise_at(.vars = c("N"),
                         .funs = function(x) sum(x, na.rm = TRUE))
        map_data <- map_data_fortified %>% left_join(realized.habitat2, by = c("id" = "StatRec"))
        world <- map_data("world")
        habitat <- ggplot(data=map_data, aes(long, lat, group = group, fill = N)) +
            geom_polygon(colour = 'white',  size = 0.3) +
            scale_fill_gradient(
                low = "#EEEE12",
                high = "#CE1500",
                na.value = "grey80") +
            geom_polygon(data=world, aes(x=long, y=lat, group=group),
                         fill='grey30', color="grey40") +
            coord_quickmap(xlim=c(-16,24), ylim=c(36,62)) +
            theme_bw() +
            ggtitle(species) +
            xlab('') + ylab('')

        if(saveFig){
            png(file=paste0(specDir,'realised_habitat2_',specLab, "_",stockLab,'.png'),
                width = 800, height = 800, res=120)
            print(habitat)
            dev.off()
        }



        ## 5. Fit GAMs
        ## --------------------------------------
        ## Get grid (doesn't matter too much for this approach)
        grid <- getGrid_df(survey.spp, opt = TRUE, gridSize = 20)

        ## set max basis dim for spatial smooths, P=positive and Z=zero/absence. (as high as possible, but computation speed)
        kvP <- 100

        modType <- 1

        ## models
        if(length(unique(survey.spp$Gear)) > 1 && length(unique(survey.spp$ShipG)) > 1){

            varInfo <- 1

            mps <- list(
                "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + Gear + s(ShipG, bs='re') + offset(log(SweptArea))",
                ##
                "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))"
            )

        }else if(length(unique(survey.spp$ShipG)) > 1){

            varInfo <- 2

            writeLines(paste0("Not using Gear as variable - only one gear used."))

            mps <- list(
                "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) +  s(ShipG, bs='re') + offset(log(SweptArea))",
                ##
                "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))"
            )

        }else{

            varInfo <- 3

            writeLines(paste0("Not using Gear nor ShipG as variable - only one gear and ShipG used."))

            mps <- list(
                "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))",
                ##
                "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))"
            )

        }

        ## prediction
        ## --------------------
        ## Median time of year
        unique(survey.spp$Quarter)
        flag <- any(survey.spp$Quarter=="1")
        print(paste0("Includes Q1: ", flag))
        if(flag){
            toyPred <- median(survey.spp$timeOfYear[survey.spp$Quarter=="1"])
        }else{
            toyPred <- median(survey.spp$timeOfYear[survey.spp$Quarter=="4"]) ## for Argyrosomus regius,
        }
        ## Median SweptArea
        sweptAreaPred <- median(survey.spp$SweptArea)
        ## Median depth
        depthPred <- median(survey.spp$Depth)

        ## knots (don't use)
        ## ---------------------
        ## knots=list(timeOfYear=seq(0,1,length=6))


        for(j in 1:length(mps)){

            ## run gams
            surveyIdx <- try(
                getSurveyIdx_df(
                    survey.spp,
                    ages = ages,
                    myids = grid[[3]],
                    cutOff = 0.01,
                    kvecP = kvP,
                    modelP = mps[[j]],
                    fam = "negbin",
                    nBoot = 1000,
                    CIlevel = 0.95,
                    mc.cores = ncores,
                    predfix = list(timeOfYear = toyPred,
                                   SweptArea = sweptAreaPred,
                                   Depth = depthPred),
                    control = list(trace=FALSE,
                                   maxit=10)
                ),
                silent = TRUE
            )



            ## 8. Create figures and output
            ## --------------------------------------

            ## GAM
            if(inherits(surveyIdx,"try-error")){

                print(paste0("Error in GAM with species: ", species, ". Removing timeOfYear and re-trying."))

                print(surveyIdx)

                ## Try again without timeOfYear which caused errors for at least
                ## 2 species: "Argyrosomus regius", "Molva macrophthalma"
                ##--------------------------------------------------------------
                modType <- 2

                ## models
                if(length(unique(survey.spp$Gear)) > 1 && length(unique(survey.spp$ShipG)) > 1){

                    varInfo <- 1

                    mps <- list(
                        "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + Gear + s(ShipG, bs='re') + offset(log(SweptArea))",
                        ##
                        "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))"
                    )

                }else if(length(unique(survey.spp$ShipG)) > 1){

                    writeLines(paste0("Not using Gear as variable - only one gear used."))

                    varInfo <- 2

                    mps <- list(
                        "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) +  s(ShipG, bs='re') + offset(log(SweptArea))",
                        ##
                        "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))"
                    )

                }else{

                    writeLines(paste0("Not using Gear nor ShipG as variable - only one gear and ShipG used."))

                    varInfo <- 3

                    mps <- list(
                        "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))",
                        ##
                        "s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))"
                    )

                }

                ## run gams again without timeOfYear
                surveyIdx <- try(
                    getSurveyIdx_df(
                        survey.spp,
                        ages = ages,
                        myids = grid[[3]],
                        cutOff = 0.01,
                        kvecP = kvP,
                        modelP = mps[[j]],
                        fam = "negbin",
                        nBoot = 1000,
                        CIlevel = 0.95,
                        mc.cores = ncores,
                        predfix = list(SweptArea = sweptAreaPred,
                                       Depth = depthPred),
                        control = list(trace=FALSE,
                                       maxit=10)
                    ),
                    silent = TRUE
                )


                if(inherits(surveyIdx,"try-error")){

                    print(paste0("Still error even without timeOfYear with species: ", species))

                    print(surveyIdx)

                }else{

                    specDir <- paste0(specDirAll,specLab,"/",stockLab,"/mod",j,"/")
                    dir.create(file.path(specDir), showWarnings = FALSE)

                    ## Save info
                    modInfo <- data.frame(varInfo, modType,
                                          min(survey.spp$Year),
                                          max(survey.spp$Year),
                                          length(unique(survey.spp$haul.id)),
                                          length(unique(survey.spp$haul.id[which(survey.spp$N > 0)])),
                                          length(unique(survey.spp$haul.id[which(survey.spp$N == 0)])))
                    colnames(modInfo) = c("varInfo","modType",
                                          "minYear","maxYear",
                                          "nHaulsTot","nHaulsPos","nHauls0")
                    rownames(modInfo) <- species
                    write.csv(modInfo, paste0(specDir,"modInfo_",specLab, "_",stockLab,"_mod",j,".csv"),
                              row.names = TRUE)


                    ## Save data
                    ## -------------
                    save(surveyIdx,
                         file = paste0(specDir,"surveyIdx_",specLab, "_",stockLab,"_mod",j,".RData"),
                         version = 2)
                    save(grid,
                         file = paste0(specDir,"grid_",specLab, "_",stockLab,"_mod",j,".RData"),
                         version = 2)

                    ##load(paste0(specDir,"surveyIdx_",specLab, "_",stockLab,".RData"))

                    ## Estimates
                    ## -------------
                    res <- data.frame(surveyIdx$idx, surveyIdx$lo, surveyIdx$up,
                                      surveyIdx$sd, surveyIdx$sd/surveyIdx$idx,
                                      stratMean$stratMean)
                    colnames(res) = c("gam","gam_lo","gam_up","gam_sd","gam_cv","stratMean")
                    write.csv(res, paste0(specDir,"index_",specLab, "_",stockLab,"_mod",j,".csv"),
                              row.names = TRUE)


                    ## Summary
                    ## -------------
                    sumi <- summary(surveyIdx$pModels[[1]])

                    ## whole summary
                    capture.output(sumi,
                                   file = paste0(specDir,"summary_",specLab,
                                                 "_",stockLab,"_mod",j,".txt"),
                                   append = FALSE)


                }

            }else{

                specDir <- paste0(specDirAll,specLab,"/",stockLab,"/mod",j,"/")
                dir.create(file.path(specDir), showWarnings = FALSE)

                ## Save data
                ## -------------
                save(surveyIdx,
                     file = paste0(specDir,"surveyIdx_",specLab, "_",stockLab,"_mod",j,".RData"),
                     version = 2)
                save(grid,
                     file = paste0(specDir,"grid_",specLab, "_",stockLab,"_mod",j,".RData"),
                     version = 2)

                ##load(paste0(specDir,"surveyIdx_",specLab, "_",stockLab,".RData"))

                ## Save info
                modInfo <- data.frame(varInfo, modType,
                                      min(survey.spp$Year),
                                      max(survey.spp$Year),
                                      length(unique(survey.spp$haul.id)),
                                      length(unique(survey.spp$haul.id[which(survey.spp$N > 0)])),
                                      length(unique(survey.spp$haul.id[which(survey.spp$N == 0)])))
                colnames(modInfo) = c("varInfo","modType",
                                      "minYear","maxYear",
                                      "nHaulsTot","nHaulsPos","nHauls0")
                rownames(modInfo) <- species
                write.csv(modInfo, paste0(specDir,"modInfo_",specLab, "_",stockLab,"_mod",j,".csv"),
                          row.names = TRUE)

                ## Estimates
                ## -------------
                res <- data.frame(surveyIdx$idx, surveyIdx$lo, surveyIdx$up,
                                  surveyIdx$sd, surveyIdx$sd/surveyIdx$idx,
                                  stratMean$stratMean)
                colnames(res) = c("gam","gam_lo","gam_up","gam_sd","gam_cv","stratMean")
                write.csv(res, paste0(specDir,"index_",specLab, "_",stockLab,"_mod",j,".csv"),
                          row.names = TRUE)


                ## Summary
                ## -------------
                sumi <- summary(surveyIdx$pModels[[1]])

                ## whole summary
                capture.output(sumi,
                               file = paste0(specDir,"summary_",specLab, "_",stockLab,"_mod",j,".txt"),
                               append = FALSE)

                ## nHauls
                ## nHaulsTot
                ## nZeroObs
                ## nNonZeroObs
                ## Deviance explained
                ## round(sumi$dev.expl * 100,2)
                ## sumi$s.table

                ## Figures
                ## -------------
                if(saveFig){

                    ## Index plot
                    ## -----------------------------
                    pdf(file=paste0(specDir,'index_',specLab, "_",stockLab,"_mod",j,'.pdf'))
                    par(mfrow = c(1,1), mar = c(5,4,4,2), oma = c(0,0,0,0))
                    if(!inherits(stratMean,"try-error")){
                        alt.idx <- stratMean[,2]
                    }else{
                        alt.idx <- NULL
                    }
                    ys = range(as.numeric(levels(as.factor(survey.spp$Year))))
                    ys = ys[1]:ys[2]
                    yl = range(c(surveyIdx$idx, 0, surveyIdx$lo,
                                 surveyIdx$up)/mean(surveyIdx$idx, na.rm = TRUE), na.rm = TRUE)
                    if (!is.null(plotBino)) {
                        yl = range(c(plotBino$fv$fit/mean(plotBino$fv$fit, na.rm = TRUE),
                                     yl), na.rm = TRUE) * 1.1
                    }
                    if (!is.null(alt.idx)) {
                        yl = range(c(alt.idx/mean(alt.idx, na.rm = TRUE), yl), na.rm = TRUE) * 1.1
                        plot(ys, alt.idx/mean(alt.idx, na.rm = TRUE),
                             ylim = yl, col = 2, ylab = "Index", xlab = "Year",
                             main = "")
                    } else {
                        plot(ys, rep(NA, length(ys)), ylim = yl, col = 2,
                             ylab = "Index", xlab = "Year", main = "")
                    }
                    idx = surveyIdx$idx
                    lo = surveyIdx$lo
                    up = surveyIdx$up
                    if(all(lo == 0, na.rm=TRUE)) lo  <- matrix(NA, nrow=length(ys), ncol=1)
                    if(all(up == 0, na.rm=TRUE)) up  <- matrix(NA, nrow=length(ys), ncol=1)
                    idx[surveyIdx$idx <= 0] = NA
                    lo[surveyIdx$idx <= 0] = NA
                    up[surveyIdx$idx <= 0] = NA
                    if (!is.null(plotBino)) {
                        lines(plotBino$fv$Year, plotBino$fv$fit/mean(plotBino$fv$fit, na.rm = TRUE),
                              lwd = 2, col=4)
                    }
                    lines(ys, idx/mean(idx, na.rm = TRUE), lwd = 2)
                    lines(ys, lo/mean(idx, na.rm = TRUE), lwd = 2, lty = 2)
                    lines(ys, up/mean(idx, na.rm = TRUE), lwd = 2, lty = 2)
                    if (!is.null(alt.idx))
                        legend("topright", pch = c(1, NA,NA),
                               lty = c(NA,1,1),
                               lwd = c(1,2, 1),
                               col = c(2, 1, 4),
                               legend = c("Strat. mean", "GAM", "Binomial"))
                    dev.off()


                    ## index plot no CI
                    pdf(file=paste0(specDir,'index_noCI_',specLab, "_",stockLab,"_mod",j,'.pdf'))
                    par(mfrow = c(1,1), mar = c(5,4,4,2), oma = c(0,0,0,0))
                    if(!inherits(stratMean,"try-error")){
                        alt.idx <- stratMean[,2]
                    }else{
                        alt.idx <- NULL
                    }
                    ys = range(as.numeric(levels(as.factor(survey.spp$Year))))
                    ys = ys[1]:ys[2]
                    idx = surveyIdx$idx
                    lo = surveyIdx$lo
                    up = surveyIdx$up
                    idx[idx <= 0] = NA
                    lo <- rep(NA, length(ys))
                    up <- rep(NA, length(ys))
                    yl = range(c(idx, 0, lo, up)/mean(idx,na.rm=TRUE), na.rm = TRUE)
                    if (!is.null(plotBino)) {
                        yl = range(c(plotBino$fv$fit/mean(plotBino$fv$fit, na.rm = TRUE),
                                     yl), na.rm = TRUE) * 1.1
                    }
                    if (!is.null(alt.idx)) {
                        yl = range(c(alt.idx/mean(alt.idx), yl), na.rm = TRUE) * 1.1
                        plot(ys, alt.idx/mean(alt.idx, na.rm = TRUE),
                             ylim = yl, col = 2, ylab = "Index", xlab = "Year",
                             main = "")
                    } else {
                        plot(ys, rep(NA, length(ys)), ylim = yl, col = 2,
                             ylab = "Index", xlab = "Year", main = "")
                    }
                    if (!is.null(plotBino)) {
                        lines(plotBino$fv$Year, plotBino$fv$fit/mean(plotBino$fv$fit, na.rm = TRUE),
                              lwd = 2, col=4)
                    }
                    lines(ys, idx/mean(idx, na.rm = TRUE),
                          lwd = 2)
                    lines(ys, lo/mean(idx, na.rm = TRUE), lwd = 2,
                          lty = 2)
                    lines(ys, up/mean(idx, na.rm = TRUE), lwd = 2,
                          lty = 2)
                    if (!is.null(alt.idx))
                        legend("topright", pch = c(1, NA,NA),
                               lty = c(NA,1,1),
                               lwd = c(1,2, 1),
                               col = c(2, 1, 4),
                               legend = c("Strat. mean", "GAM", "Binomial"))
                    dev.off()


                    ## Overall map
                    ## --------------------
                    png(file=paste0(specDir,'map_',specLab, "_",stockLab,"_mod",j,'.png'),
                        width = 800, height = 900, res=120)
                    surveyIdxPlots_df(surveyIdx, survey.spp,
                                      cols=ages, alt.idx=NULL, grid[[3]],
                                      par=list(mfrow=c(1,1)),legend=FALSE,
                                      map.cex = 1.5, main = "All years",
                                      colors=rev(heat.colors(8)),
                                      select="map",plotByAge=FALSE)
                    dev.off()



                    ## yearly maps
                    ## -------------------------
                    map.cex <- 1
                    png(file=paste0(specDir,'yearmaps_',specLab, "_",stockLab,"_mod",j,'.png'),
                        width = 900, height = 500, res=120)
                    ## Spatial yearly maps
                    years0 <- sort(unique(surveyIdx$yearNum))
                    years <- c(max(min(years0),1991), floor(mean(years0)), 2019)
                    ny <- length(years)
                    lt <- layout(matrix(c(1:3,rep(4,3)),2,3,byrow = TRUE), heights = c(1,0.2))
                    par(mar = c(1,1,2,1), oma = c(4,3,2,1))
                    for(j in 1:ny){
                        year <- years[j]
                        if(is.na(year)) break()
                        predD <- NULL
                        myids <- grid[[3]]
                        colors=rev(heat.colors(8))
                        a <- 1
                        xlims = range(survey.spp$Lon, na.rm = TRUE)
                        ylims = range(survey.spp$Lat, na.rm = TRUE)
                        if (is.null(predD)) {
                            tmp = subset(survey.spp, haul.id %in% myids)
                        }else {
                            tmp = predD
                        }
                        ally = data.frame(val = surveyIdx$gPreds2[[a]][[1]],
                                          year = as.character(levels(as.factor(survey.spp$Year))[1]))
                        cc = 0
                        for (y in names(surveyIdx$gPreds2[[a]])) {
                            cc = cc + 1
                            ally = rbind(ally, data.frame(val = surveyIdx$gPreds2[[a]][[cc]],
                                                          year = as.character(levels(as.factor(survey.spp$Year))[cc])))
                        }
                        ally$conc = surveyIndex:::concTransform(log(ally$val))
                        ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))
                        for (yy in year) {
                            plot(tmp$Lon, y = tmp$Lat, col = 1, pch = 1,
                                 cex = map.cex, xlab = "", ylab = "",
                                 axes = FALSE)
                            title(yy, line = 1)
                            sel = which(ally$year == yy)
                            points(tmp$Lon, y = tmp$Lat, col = colors[as.numeric(ally$zFac[sel])],
                                   pch = 16, cex = map.cex)
                            maps::map("worldHires", xlim = xlims, ylim = ylims,
                                      fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
                            box(lwd=1.5)
                        }
                    }
                    plot.new()
                    maxcuts = aggregate(val ~ zFac, data = ally,
                                        FUN = max)
                    mincuts = aggregate(val ~ zFac, data = ally,
                                        FUN = min)
                    mm = mean(ally$val)
                    ml = signif(mincuts[, 2]/mm, 3)
                    ml[1] = 0
                    leg = paste0("[", ml, ",", signif(maxcuts[,2]/mm, 3), "]")
                    legend("center",
                           ncol = length(leg),
                           legend = leg, pch = 16,
                           col = colors, bg = "white")
                    dev.off()


                    ## Effects (smooth terms
                    ## spatial
                    png(file=paste0(specDir,'effect_spatial_',specLab, "_",stockLab,"_mod",j,'.png'),
                        width = 800, height = 800, res=120)
                    plot.gam(surveyIdx$pModels[[1]], select = 1, main = "Spatial effect")
                    dev.off()
                    ## timeOfYear
                    png(file=paste0(specDir,'effect_toy_',specLab, "_",stockLab,"_mod",j,'.png'),
                        width = 800, height = 800, res=120)
                    plot.gam(surveyIdx$pModels[[1]], select = 2, main = "Seasonal effect")
                    dev.off()
                    ## ctime
                    png(file=paste0(specDir,'effect_year_',specLab, "_",stockLab,"_mod",j,'.png'),
                        width = 800, height = 800, res=120)
                    plot.gam(surveyIdx$pModels[[1]], select = 3, main = "Year effect")
                    dev.off()
                    ## Depth
                    png(file=paste0(specDir,'effect_depth_',specLab, "_",stockLab,"_mod",j,'.png'),
                        width = 800, height = 800, res=120)
                    plot.gam(surveyIdx$pModels[[1]], select = 4, main = "Depth effect")
                    dev.off()
                    ## ShipGear (not for random effects?)
                    ## plot.gam(surveyIdx$pModels[[1]], select = 5, main = "Ship x Gear")


                    ## Diagnostics
                    ## --------------
                    ## Histogram
                    pdf(file=paste0(specDir,'resid_dist_',specLab, "_",stockLab,"_mod",j,'.pdf'))
                    par(mfrow=c(2,1), mar = c(5,4,4,2), oma = c(0,0,0,0))
                    hist(surveyIdx$residuals[[1]], main = "Residuals",
                         xlab = "Residuals",ylab = "Frequency")
                    ## Boxplot over time
                    pdat <- by(surveyIdx$residuals[[1]], surveyIdx$yearNum, list)
                    boxplot(pdat, ylab = "Residuals", xlab = "Year")
                    abline(h=0)
                    dev.off()

                    ## Fitted vs. residuals
                    pdf(file=paste0(specDir,'resid_fitted_',specLab, "_",stockLab,"_mod",j,'.pdf'))
                    plot(log(fitted(surveyIdx$pModels[[1]])),
                         surveyIdx$residuals[[1]],
                         xlab = "Fitted", ylab = "Residuals")
                    dev.off()


                    pdf(file=paste0(specDir,'resid_daynight_',specLab, "_",stockLab,"_mod",j,'.pdf'))
                    plot(as.factor(survey.spp$DayNight),
                         surveyIdx$residuals[[1]],
                         xlab = "DayNight", ylab = "Residuals")
                    abline(h=0)
                    dev.off()

                    pdf(file=paste0(specDir,'resid_ShipG_',specLab, "_",stockLab,"_mod",j,'.pdf'))
                    plot(as.factor(survey.spp$ShipG),
                         surveyIdx$residuals[[1]],
                         xlab = "ShipGear", ylab = "Residuals")
                    abline(h=0)
                    dev.off()

                    ## QQplot
                    if(all(is.finite(surveyIdx$residuals[[1]]))){
                        pdf(file=paste0(specDir,'resid_qqplot_',specLab, "_",stockLab,"_mod",j,'.pdf'))
                        qqnorm(surveyIdx$residuals[[1]])
                        qqline(surveyIdx$residuals[[1]], lty = 3, col = "gray50")
                        dev.off()
                    }

                    ## Spatial residuals
                    png(file=paste0(specDir,'resid_spatial_',specLab, "_",stockLab,"_mod",j,'.png'),
                        width = 900, height = 500, res=120)
                    years0 <- sort(unique(surveyIdx$yearNum))
                    years <- c(max(min(years0),1991), floor(mean(years0)), 2019)
                    ny <- length(years)
                    lt <- layout(matrix(c(1:3),1,3,byrow = TRUE))
                    par(mar = c(1,1,2,1), oma = c(4,3,2,1))
                    for(j in 1:ny){
                        year <- as.character(years[j])
                        if(is.na(year)) break()
                        scale <- 1
                        sel <- which(survey.spp$Year == as.character(year))
                        xaxt <- ifelse(j %in% 1:3, "s", "n")
                        yaxt <- ifelse(j %in% c(1,5,9,13,17), "s", "n")
                        plot(survey.spp$Lon, survey.spp$Lat,
                             type = "n", xlab = "",
                             xaxt= xaxt, yaxt = yaxt,
                             ylab = "", main = year)
                        maps::map("worldHires", fill = TRUE, plot = TRUE,
                                  add = TRUE, col = grey(0.5))
                        resi <- surveyIdx$residuals[[1]]
                        positive = surveyIdx$residuals[[1]][sel] > 0
                        points(survey.spp$Lon[sel][positive], survey.spp$Lat[sel][positive],
                               pch = 1, cex = scale * sqrt(resi[sel][positive]),
                               col = "blue")
                        points(survey.spp$Lon[sel][!positive], survey.spp$Lat[sel][!positive],
                               pch = 1, cex = scale * sqrt(-resi[sel][!positive]),
                               col = "red")
                    }
                    dev.off()


                    ## Spatial yearly maps
                    map.cex <- 1
                    png(file=paste0(specDir,'yearmaps_all_',specLab, "_",stockLab,"_mod",j,"_page1",'.png'),
                        width = 800, height = 800, res=120)
                    years <- sort(unique(surveyIdx$yearNum))
                    ny <- length(years)
                    par(mfrow=c(5,4), mar = c(1,1,2,1), oma = c(4,3,2,1))
                    for(j in 1:20){
                        year <- years[j]
                        if(is.na(year)) break()
                        predD <- NULL
                        myids <- grid[[3]]
                        colors=rev(heat.colors(8))
                        a <- 1
                        xlims = range(survey.spp$Lon, na.rm = TRUE)
                        ylims = range(survey.spp$Lat, na.rm = TRUE)
                        if (is.null(predD)) {
                            tmp = subset(survey.spp, haul.id %in% myids)
                        }else {
                            tmp = predD
                        }
                        ally = data.frame(val = surveyIdx$gPreds2[[a]][[1]],
                                          year = as.character(levels(as.factor(survey.spp$Year))[1]))
                        cc = 0
                        for (y in names(surveyIdx$gPreds2[[a]])) {
                            cc = cc + 1
                            ally = rbind(ally, data.frame(val = surveyIdx$gPreds2[[a]][[cc]],
                                                          year = as.character(levels(as.factor(survey.spp$Year))[cc])))
                        }
                        ally$conc = surveyIndex:::concTransform(log(ally$val))
                        ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))
                        for (yy in year) {
                            plot(tmp$Lon, y = tmp$Lat, col = 1, pch = 1,
                                 cex = map.cex, xlab = "Longitude", ylab = "Latitude",
                                 axes = FALSE)
                            box()
                            title(yy, line = 1)
                            sel = which(ally$year == yy)
                            points(tmp$Lon, y = tmp$Lat, col = colors[as.numeric(ally$zFac[sel])],
                                   pch = 16, cex = map.cex)
                            maps::map("worldHires", xlim = xlims, ylim = ylims,
                                      fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
                        }
                    }
                    dev.off()
                    png(file=paste0(specDir,'yearmaps_all_',specLab, "_",stockLab,"_mod",j,"_page2",'.png'),
                        width = 800, height = 800, res=120)
                    par(mfrow=c(5,4), mar = c(1,1,2,1), oma = c(4,3,2,1))
                    if(ny > 20){
                        for(j in 1:20){
                            year <- years[20 + j]
                            if(is.na(year)) break()
                            predD <- NULL
                            myids <- grid[[3]]
                            colors=rev(heat.colors(8))
                            a <- 1
                            xlims = range(survey.spp$Lon, na.rm = TRUE)
                            ylims = range(survey.spp$Lat, na.rm = TRUE)
                            if (is.null(predD)) {
                                tmp = subset(survey.spp, haul.id %in% myids)
                            }else {
                                tmp = predD
                            }
                            ally = data.frame(val = surveyIdx$gPreds2[[a]][[1]],
                                              year = as.character(levels(as.factor(survey.spp$Year))[1]))
                            cc = 0
                            for (y in names(surveyIdx$gPreds2[[a]])) {
                                cc = cc + 1
                                ally = rbind(ally, data.frame(val = surveyIdx$gPreds2[[a]][[cc]],
                                                              year = as.character(levels(as.factor(survey.spp$Year))[cc])))
                            }
                            ally$conc = surveyIndex:::concTransform(log(ally$val))
                            ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))
                            for (yy in year) {
                                plot(tmp$Lon, y = tmp$Lat, col = 1, pch = 1,
                                     cex = map.cex, xlab = "Longitude", ylab = "Latitude",
                                     axes = FALSE)
                                box()
                                title(yy, line = 1)
                                sel = which(ally$year == yy)
                                points(tmp$Lon, y = tmp$Lat, col = colors[as.numeric(ally$zFac[sel])],
                                       pch = 16, cex = map.cex)
                                maps::map("worldHires", xlim = xlims, ylim = ylims,
                                          fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
                            }
                        }
                    }
                    dev.off()
                    png(file=paste0(specDir,'yearmaps_all_',specLab, "_",stockLab,"_mod",j,"_page3",'.png'),
                        width = 800, height = 800, res=120)
                    par(mfrow=c(5,4), mar = c(1,1,2,1), oma = c(4,3,2,1))
                    if(ny > 40){
                        for(j in 1:20){
                            year <- years[40 + j]
                            if(is.na(year)) break()
                            predD <- NULL
                            myids <- grid[[3]]
                            colors=rev(heat.colors(8))
                            a <- 1
                            xlims = range(survey.spp$Lon, na.rm = TRUE)
                            ylims = range(survey.spp$Lat, na.rm = TRUE)
                            if (is.null(predD)) {
                                tmp = subset(survey.spp, haul.id %in% myids)
                            }else {
                                tmp = predD
                            }
                            ally = data.frame(val = surveyIdx$gPreds2[[a]][[1]],
                                              year = as.character(levels(as.factor(survey.spp$Year))[1]))
                            cc = 0
                            for (y in names(surveyIdx$gPreds2[[a]])) {
                                cc = cc + 1
                                ally = rbind(ally, data.frame(val = surveyIdx$gPreds2[[a]][[cc]],
                                                              year = as.character(levels(as.factor(survey.spp$Year))[cc])))
                            }
                            ally$conc = surveyIndex:::concTransform(log(ally$val))
                            ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))
                            for (yy in year) {
                                plot(tmp$Lon, y = tmp$Lat, col = 1, pch = 1,
                                     cex = map.cex, xlab = "Longitude", ylab = "Latitude",
                                     axes = FALSE)
                                box()
                                title(yy, line = 1)
                                sel = which(ally$year == yy)
                                points(tmp$Lon, y = tmp$Lat, col = colors[as.numeric(ally$zFac[sel])],
                                       pch = 16, cex = map.cex)
                                maps::map("worldHires", xlim = xlims, ylim = ylims,
                                          fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
                            }
                        }
                    }
                    dev.off()

                    ## Spatial residuals (all years)
                    scale <- 0.7
                    png(file=paste0(specDir,'resid_spatial_all_',specLab, "_",stockLab,"_mod",j,'_page1.png'),
                        width = 800, height = 800, res=120)
                    years <- sort(unique(surveyIdx$yearNum))
                    ny <- length(years)
                    par(mfrow=c(5,4), mar = c(1,1,2,1), oma = c(4,3,2,1))
                    for(j in 1:20){
                        year <- as.character(years[j])
                        if(is.na(year)) break()
                        sel <- which(survey.spp$Year == as.character(year))
                        xaxt <- ifelse(j %in% 17:20, "s", "n")
                        yaxt <- ifelse(j %in% c(1,5,9,13,17), "s", "n")
                        plot(survey.spp$Lon, survey.spp$Lat,
                             type = "n", xlab = "",
                             xaxt= xaxt, yaxt = yaxt,
                             ylab = "", main = year)
                        maps::map("worldHires", fill = TRUE, plot = TRUE,
                                  add = TRUE, col = grey(0.5))
                        resi <- surveyIdx$residuals[[1]]
                        positive = surveyIdx$residuals[[1]][sel] > 0
                        points(survey.spp$Lon[sel][positive], survey.spp$Lat[sel][positive],
                               pch = 1, cex = scale * sqrt(resi[sel][positive]),
                               col = "blue")
                        points(survey.spp$Lon[sel][!positive], survey.spp$Lat[sel][!positive],
                               pch = 1, cex = scale * sqrt(-resi[sel][!positive]),
                               col = "red")
                    }
                    dev.off()
                    png(file=paste0(specDir,'resid_spatial_all_',specLab, "_",stockLab,"_mod",j,'_page2.png'),
                        width = 800, height = 800, res=120)
                    par(mfrow=c(5,4), mar = c(1,1,2,1), oma = c(4,3,2,1))
                    if(ny > 20){
                        par(mfrow=c(5,4), mar = c(1,1,2,1), oma = c(4,3,2,1))
                        for(j in 1:20){
                            year <- as.character(years[20 + j])
                            if(is.na(year)) break()
                            sel <- which(survey.spp$Year == as.character(year))
                            xaxt <- ifelse(j %in% 17:20, "s", "n")
                            yaxt <- ifelse(j %in% c(1,5,9,13,17), "s", "n")
                            plot(survey.spp$Lon, survey.spp$Lat,
                                 type = "n", xlab = "",
                                 xaxt= xaxt, yaxt = yaxt,
                                 ylab = "", main = year)
                            maps::map("worldHires", fill = TRUE, plot = TRUE,
                                      add = TRUE, col = grey(0.5))
                            resi <- surveyIdx$residuals[[1]]
                            positive = surveyIdx$residuals[[1]][sel] > 0
                            points(survey.spp$Lon[sel][positive], survey.spp$Lat[sel][positive],
                                   pch = 1, cex = scale * sqrt(resi[sel][positive]),
                                   col = "blue")
                            points(survey.spp$Lon[sel][!positive], survey.spp$Lat[sel][!positive],
                                   pch = 1, cex = scale * sqrt(-resi[sel][!positive]),
                                   col = "red")
                        }
                    }
                    dev.off()
                    png(file=paste0(specDir,'resid_spatial_all_',specLab, "_",stockLab,"_mod",j,'_page3.png'),
                        width = 800, height = 800, res=120)
                    if(ny > 40){
                        par(mfrow=c(5,4), mar = c(1,1,2,1), oma = c(4,3,2,1))
                        for(j in 1:20){
                            year <- as.character(years[40 + j])
                            if(is.na(year)) break()
                            sel <- which(survey.spp$Year == as.character(year))
                            xaxt <- ifelse(j %in% 17:20, "s", "n")
                            yaxt <- ifelse(j %in% c(1,5,9,13,17), "s", "n")
                            plot(survey.spp$Lon, survey.spp$Lat,
                                 type = "n", xlab = "",
                                 xaxt= xaxt, yaxt = yaxt,
                                 ylab = "", main = year)
                            maps::map("worldHires", fill = TRUE, plot = TRUE,
                                      add = TRUE, col = grey(0.5))
                            resi <- surveyIdx$residuals[[1]]
                            positive = surveyIdx$residuals[[1]][sel] > 0
                            points(survey.spp$Lon[sel][positive], survey.spp$Lat[sel][positive],
                                   pch = 1, cex = scale * sqrt(resi[sel][positive]),
                                   col = "blue")
                            points(survey.spp$Lon[sel][!positive], survey.spp$Lat[sel][!positive],
                                   pch = 1, cex = scale * sqrt(-resi[sel][!positive]),
                                   col = "red")
                        }
                    }
                    dev.off()

                }
            }
        }
    }
}
