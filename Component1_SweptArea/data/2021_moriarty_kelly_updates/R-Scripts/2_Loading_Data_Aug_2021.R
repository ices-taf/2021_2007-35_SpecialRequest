###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# This is Script 2 of X 
# The purpose of this script is to load the raw data into R from the various folders.
# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: Nicola Walker (Cefas) Jonas Hentati Sundberg (SLU)
# Editted for 2021 rerun by Ruth Kelly (AFBI) - R version 4.0.4 (2021-02-15) -- "Lost Library Book"
#############
# Load data #
#############
# If FALSE, mostly suppress CI computation
need.CI <- FALSE
# Number of bootstrap replicates
if(need.CI){
  nb <- 1000
}else{
  nb <- 3 ## just to make code run through
}
#
# Confidence interval range
CV <- .95
# Set seed for reproducable results
setSeed <- set.seed(627)

###################
# Load Saved Data #
###################

# As this product must be totally reproducable I have saved a copy of the 
# DATRAS files on a given date (08-09/06/2021)- scripts used in the download of these files
# are given in script 0_get_datras_data.r

# For reproducibility the files are archived in the ICES sharepoint for the WK_SAE meeting of 2021, as they are 
# too large to store on github.


# Read in haul header data

## scottish files are now split into two on Datras (RK)
HH_SWC1<-read.csv("./Raw_Data/DATRAS_08_06_2021/SWC-IBTS/HH_data_SWC_IBTS1_09_06_2021.csv")
HH_SWC2<-read.csv("./Raw_Data/DATRAS_08_06_2021/SWC-IBTS/HH_data_SWC_IBTS2_09_06_2021.csv")
HH_SWC <- rbind(HH_SWC1,HH_SWC2)
rm(HH_SWC1)
rm(HH_SWC2)
## Rockall (Scottish)
HH_ROCK1 <-read.csv("Raw_Data/DATRAS_08_06_2021/ROCKALL/HH_data_ROC1_09_06_2021.csv")
HH_ROCK2 <-read.csv("Raw_Data/DATRAS_08_06_2021/ROCKALL/HH_data_ROC2_09_06_2021.csv")
HH_ROCK <- rbind(HH_ROCK1, HH_ROCK2)
rm(HH_ROCK1, HH_ROCK2)

## Portugal 
HH_PT<-read.csv("Raw_Data/DATRAS_08_06_2021/PT-IBTS/HH_data_PTIBTS_09_06_2021.csv")

## Irish Sea (NI)
HH_NIGFS<-read.csv("Raw_Data/DATRAS_08_06_2021/NIGFS/HH_data_NIGFS_08_06_2021.csv")

## Irish Ground Fish
HH_IGFS<-read.csv("Raw_Data/DATRAS_08_06_2021/IE-IGFS/HH_data_IEIGFS_09_06_2021.csv")


## France - Channel Groundfish
HH_FRCGFS<-read.csv("Raw_Data/DATRAS_08_06_2021/FR-CGFS/HH_data_FRCGFS_08_06_2021.csv")

## France EVHOE
HH_EVHOE<-read.csv("Raw_Data/DATRAS_08_06_2021/EVHOE/HH_data_EVHOE_08_06_2021.csv")

## Beam trawl
HH_BTS<-read.csv("Raw_Data/DATRAS_08_06_2021/BTS/HH_data_BTS_09_06_2021.csv")

## HH_BTS7a<-read.csv("./Raw_Data/DATRAS_30-03-2017/BTS-VIIa/Exchange Data_2017-03-30 15_45_09.csv") 
## this is now incuded in the above BTS file I think RK

## North Sea 
HH_NSIBTS<-read.csv("Raw_Data/DATRAS_08_06_2021/NS-IBTS/HH_NSIBTS_09_06_2021.csv",row.names = "X")

## Spain

HH_SP_PORC<-read.csv("Raw_Data/DATRAS_08_06_2021/SPAIN/HH_data_SP_PORC_09_06_2021.csv")
HH_SP_ARSA<-read.csv("Raw_Data/DATRAS_08_06_2021/SPAIN/HH_SP_ARSA_09_06_2021.csv", row.names = "X")
HH_SP_NORTH<-read.csv("Raw_Data/DATRAS_08_06_2021/SPAIN/HH_SP_NORTH_09_06_2021.csv", row.names = "X")

names(HH_NSIBTS)

# Read biological data (Datras HL files)

HL_SWC1<-read.csv("Raw_Data/DATRAS_08_06_2021/SWC-IBTS/HL_data_SWC_IBTS1_09_06_2021.csv")
HL_SWC2<-read.csv("Raw_Data/DATRAS_08_06_2021/SWC-IBTS/HL_data_SWC_IBTS2_09_06_2021.csv")

setdiff(names(HL_SWC1), names(HL_SWC2))
setdiff(names(HL_SWC2), names(HL_SWC1))

names(HL_SWC1)

HL_SWC1 <- HL_SWC1[,-29]  ## remove scientific name col not present in other datasets

HL_SWC1$Valid_Aphia <- HL_SWC1$ValidAphiaID  ## move and rename Aphia Id col to match SWC2

HL_SWC1 <- HL_SWC1[,-28] ## remove old apha ID column which is now moved. 

names(HL_SWC1) == names(HL_SWC2) ### now match and can be rbound

HL_SWC <- rbind(HL_SWC1, HL_SWC2)

rm(HL_SWC1, HL_SWC2)


HL_ROCK1<-read.csv("Raw_Data/DATRAS_08_06_2021/ROCKALL/HL_data_ROC1_09_06_2021.csv")
HL_ROCK2<-read.csv("Raw_Data/DATRAS_08_06_2021/ROCKALL/HL_data_ROC2_09_06_2021.csv")
HL_ROCK <- rbind(HL_ROCK1,HL_ROCK2)
rm(HL_ROCK1,HL_ROCK2)

###  
HL_PT<-read.csv("Raw_Data/DATRAS_08_06_2021/PT-IBTS/HL_data_PTIBTS_09_06_2021.csv")
HL_NIGFS<-read.csv("Raw_Data/DATRAS_08_06_2021/NIGFS/HL_data_NIGFS_08_06_2021.csv")
HL_IGFS<-read.csv("Raw_Data/DATRAS_08_06_2021/IE-IGFS/HL_data_IEIGFS_09_06_2021.csv")
HL_EVHOE<-read.csv("Raw_Data/DATRAS_08_06_2021/EVHOE/HL_data_EVHOE_08_06_2021.csv")
HL_BTS<-read.csv("Raw_Data/DATRAS_08_06_2021/BTS/HL_data_BTS_09_06_2021.csv")
HL_FRCGFS<-read.csv("Raw_Data/DATRAS_08_06_2021/FR-CGFS/HL_data_FRCGRS_08_06_2021.csv")
#HL_BTS7a<-read.csv("./Raw_Data/DATRAS_30-03-2017/BTS-VIIa/Exchange Data_2017-03-30 15_44_22.csv")
HL_NSIBTS<-read.csv("Raw_Data/DATRAS_08_06_2021/NS-IBTS/HL_NSIBTS_09_06_2021.csv", row.names = "X")

###############################
# Add national submitted data##
###############################
##############
# Danish Data#
##############
# Add corrections of data from National Data providers
# Denmark earliest years of survey were missing species
NS_DEN_sp_1983<-read.csv("./Raw_Data/Corrections/DNK_IBTS1_1983_GOV.CSV", header=F)
NS_DEN_sp_1984<-read.csv("./Raw_Data/Corrections/DNK_IBTS1_1984_GOV.CSV", header=F)
NS_DEN_sp_1985<-read.csv("./Raw_Data/Corrections/DNK_IBTS1_1985_GOV.CSV", header=F)
NS_DEN_sp_1986<-read.csv("./Raw_Data/Corrections/DNK_IBTS1_1986_GOV.CSV", header=F)
#######################
# Northern Irish Data #
#######################
# Northern Ireland early data not available on Datras
NI_extra<-read.csv("./Raw_Data/National Submissions/Datras_MSFD_NI/Datras_MSFD1.csv", header=F)


### Spain

HL_SP_PORC <- read.csv("Raw_Data/DATRAS_08_06_2021/SPAIN/HL_data_SP_PORC_09_06_2021.csv")
HL_SP_ARSA  <- read.csv("Raw_Data/DATRAS_08_06_2021/SPAIN/HL_SP_ARSA_09_06_2021.csv", row.names = "X")
HL_SP_NORTH  <- read.csv("Raw_Data/DATRAS_08_06_2021/SPAIN/HL_SP_NORTH_09_06_2021.csv", row.names = "X")

### reformat SP PORC cols to match others.. .
names(HL_SP_PORC)
names(HL_SP_ARSA)


HL_SP_PORC <- HL_SP_PORC[,-29]  ## remove scientific name col not present in other datasets

HL_SP_PORC$Valid_Aphia <- HL_SP_PORC$ValidAphiaID  ## move and rename Aphia Id col to match SWC2

HL_SP_PORC <- HL_SP_PORC[,-28] ## remove old apha ID column which is now moved. 

which(names(HL_SP_PORC) != names(HL_SWC)) ### now match and can be rbound
