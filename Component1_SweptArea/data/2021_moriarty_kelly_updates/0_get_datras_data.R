##### obtaining data for rerun of OSPAR scripts ###

### This script was run separately from the main modelling process
## such that data files obtained on this date (08/09 of June 2021)
## can be stored for reproducibility purposes. 

### script to download necessary HH and HL files from Datras
# including all available years from surveys included in Moriarty
# et al.  

# The Spanish surveys are not currently included in the data product, 
# and were not included in Moriarty et al 2017. These would require additional
# work and testing which is beyond the scope of this rerun. 

# Rockall and Scottish surveys are now split into two data files, based on changes
# to survey in 2010/2011 respectively

# BTS 7a is now included in BTS survey data folder. 




#install.packages("icesDatras")
library(icesDatras)


### 08_06_2021 ###

### NI ground fish
HH_NIGFS <- getDATRAS(record = "HH", survey = "NIGFS",
                      years = 2008:2020, quarters = c(1,2,3,4))
HL_NIGFS <- getDATRAS(record = "HL", survey = "NIGFS",
                      years = 2008:2020, quarters = c(1,2,3,4))


write.csv(HH_NIGFS, "Raw_Data/DATRAS_08_06_2021/NIGFS/HH_data_NIGFS_08_06_2021.csv", row.names = FALSE )
write.csv(HL_NIGFS, "Raw_Data/DATRAS_08_06_2021/NIGFS/HL_data_NIGFS_08_06_2021.csv", row.names = FALSE )

### French EVHOE

HH_EVHOE <- getDATRAS(record = "HH", survey = "EVHOE",
                      years = 1997:2020, quarters = c(1,2,3,4))
HL_EVHOE <- getDATRAS(record = "HL", survey = "EVHOE",
                      years = 1997:2020, quarters = c(1,2,3,4))

write.csv(HH_EVHOE, "Raw_Data/DATRAS_08_06_2021/EVHOE/HH_data_EVHOE_08_06_2021.csv", row.names = FALSE )
write.csv(HL_EVHOE, "Raw_Data/DATRAS_08_06_2021/EVHOE/HL_data_EVHOE_08_06_2021.csv", row.names = FALSE )

### France Channel Ground Fish
HH_FR <- getDATRAS(record = "HH", survey = "FR-CGFS",
                   years = 1988:2020, quarters = c(1,2,3,4))
HL_FR <- getDATRAS(record = "HL", survey = "FR-CGFS",
                   years = 1988:2020, quarters = c(1,2,3,4))

write.csv(HH_FR, "Raw_Data/DATRAS_08_06_2021/FR-CGFS/HH_data_FRCGFS_08_06_2021.csv", row.names = FALSE )
write.csv(HL_FR, "Raw_Data/DATRAS_08_06_2021/FR-CGFS/HL_data_FRCGRS_08_06_2021.csv", row.names = FALSE )

### Irish Ground Fish
HH_IE <- getDATRAS(record = "HH", survey = "IE-IGFS",
                   years = 2003:2020, quarters = c(1,2,3,4))
HL_IE <- getDATRAS(record = "HL", survey = "IE-IGFS",
                   years = 2003:2020, quarters = c(1,2,3,4))

write.csv(HH_IE, "Raw_Data/DATRAS_08_06_2021/IE-IGFS/HH_data_IEIGFS_09_06_2021.csv", row.names = FALSE )
write.csv(HL_IE, "Raw_Data/DATRAS_08_06_2021/IE-IGFS/HL_data_IEIGFS_09_06_2021.csv", row.names = FALSE )


### Scottish IBTS - now split into 2 files to reflect change in survey in 2011
HH_SWC1 <- getDATRAS(record = "HH", survey = "SWC-IBTS",
                     years = 1985:2010, quarters = c(1,2,3,4))
# HL_SWC1 <- getDATRAS(record = "HL", survey = "SWC-IBTS",
#                      years = 1985:2010, quarters = c(1,2,3,4))

## there was an error with the R download of this file so I downloaded it directly from the Datras webpage and 
## saved it in the folder below

write.csv(HH_SWC1, "Raw_Data/DATRAS_08_06_2021/SWC-IBTS/HH_data_SWC_IBTS1_09_06_2021.csv", row.names = FALSE )
#write.csv(HL_SWC1, "Raw_Data/DATRAS_08_06_2021/SWC-IBTS/HL_data_SWC_IBTS1_09_06_2021.csv", row.names = FALSE )

HH_SWC2 <- getDATRAS(record = "HH", survey = "SCOWCGFS",
                     years = 2011:2021, quarters = c(1,2,3,4))
HL_SWC2 <- getDATRAS(record = "HL", survey = "SCOWCGFS",
                     years = 2011:2021, quarters = c(1,2,3,4))

write.csv(HH_SWC2, "Raw_Data/DATRAS_08_06_2021/SWC-IBTS/HH_data_SWC_IBTS2_09_06_2021.csv", row.names = FALSE )
write.csv(HL_SWC2, "Raw_Data/DATRAS_08_06_2021/SWC-IBTS/HL_data_SWC_IBTS2_09_06_2021.csv", row.names = FALSE )

### Portuguese GroundFish
HH_PT <- getDATRAS(record = "HH", survey = "PT-IBTS",
                   years = 2002:2021, quarters = c(1,2,3,4))
HL_PT <- getDATRAS(record = "HL", survey = "PT-IBTS",
                   years = 2002:2021, quarters = c(1,2,3,4))

write.csv(HH_PT, "Raw_Data/DATRAS_08_06_2021/PT-IBTS/HH_data_PTIBTS_09_06_2021.csv", row.names = FALSE )
write.csv(HL_PT, "Raw_Data/DATRAS_08_06_2021/PT-IBTS/HL_data_PTIBTS_09_06_2021.csv", row.names = FALSE )

### Rockall - split into 2 files to reflect changes to survey in 2011

HH_ROC1 <-  getDATRAS(record = "HH", survey = "ROCKALL",
          years = 1999:2009, quarters = c(1,2,3,4))

HL_ROC1 <-  getDATRAS(record = "HL", survey = "ROCKALL",
                      years = 1999:2009, quarters = c(1,2,3,4))


write.csv(HH_ROC1, "Raw_Data/DATRAS_08_06_2021/ROCKALL/HH_data_ROC1_09_06_2021.csv", row.names = FALSE )
write.csv(HL_ROC1, "Raw_Data/DATRAS_08_06_2021/ROCKALL/HL_data_ROC1_09_06_2021.csv", row.names = FALSE )

HH_ROC2 <-  getDATRAS(record = "HH", survey = "SCOROC",
                      years = 2011:2021, quarters = c(1,2,3,4))

HL_ROC2 <-  getDATRAS(record = "HL", survey = "SCOROC",
                      years = 2011:2021, quarters = c(1,2,3,4))


write.csv(HH_ROC2, "Raw_Data/DATRAS_08_06_2021/ROCKALL/HH_data_ROC2_09_06_2021.csv", row.names = FALSE )
write.csv(HL_ROC2, "Raw_Data/DATRAS_08_06_2021/ROCKALL/HL_data_ROC2_09_06_2021.csv", row.names = FALSE )

#### Beam Trawl


HH_BTS <-  getDATRAS(record = "HH", survey = "BTS",
                      years = 1985:2020, quarters = c(1,2,3,4))

HL_BTS <-  getDATRAS(record = "HL", survey = "BTS",
                     years = 1985:2020, quarters = c(1,2,3,4))

write.csv(HH_BTS, "Raw_Data/DATRAS_08_06_2021/BTS/HH_data_BTS_09_06_2021.csv", row.names = FALSE )
write.csv(HL_BTS, "Raw_Data/DATRAS_08_06_2021/BTS/HL_data_BTS_09_06_2021.csv", row.names = FALSE )




# Error in download of this file, so I directly downloaded it from the 
# Datras web download site
# HH_SP <- getDATRAS(record = "HH", survey = "SP-PORC",
#                    years = 2011:2020, quarters = c(1,2,3,4))
# HL_SP <- getDATRAS(record = "HL", survey = "SP-PORC",
#                    years = 2002:2020, quarters = c(1,2,3,4))

#write.csv(HH_SP, "HH_data_SP_PORC_08_06_2021.csv", row.names = FALSE )
#write.csv(HL_SP, "HL_data_SP_PORC_08_06_2021.csv", row.names = FALSE )






HH_NSIBTS <- getDATRAS(record = "HH", survey = "NS-IBTS",
                       years = 1965:2021, quarters = c(1,2,3,4))

HL_NSIBTS_80 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                       years = 1965:1980, quarters = c(1,2,3,4))


HL_NSIBTS_90 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                          years = 1981:1990, quarters = c(1,2,3,4))


HL_NSIBTS_00 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                          years = 1991:2000, quarters = c(1,2,3,4))


HL_NSIBTS_10 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                          years = 2001:2010, quarters = c(1,2,3,4))

HL_NSIBTS_21 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                          years = 2011:2021, quarters = c(1,2,3,4))

HL_NSIBTS <- rbind(HL_NSIBTS_00,HL_NSIBTS_10,
                   HL_NSIBTS_21, HL_NSIBTS_80,
                   HL_NSIBTS_90)

head(HL_NSIBTS)
write.csv(HH_NSIBTS, "Raw_Data/DATRAS_08_06_2021/NS-IBTS/HH_NSIBTS_09_06_2021.csv")

write.csv(HL_NSIBTS, "Raw_Data/DATRAS_08_06_2021/NS-IBTS/HL_NSIBTS_09_06_2021.csv")

### other beam trawls included in the Datras WGBeam product for possible inclusion. 

# Sole Net surveys

HH_SNS <- getDATRAS(record = "HH", survey = "SNS",
                          years = 1985:2021, quarters = c(1,2,3,4))

HLSNS <- getDATRAS(record = "HL", survey = "SNS",
                          years = 1985:2021, quarters = c(1,2,3,4))

write.csv(HH_SNS, "Raw_Data/DATRAS_08_06_2021/Beam_oth/HH_SNS_09_06_2021.csv")
write.csv(HLSNS, "Raw_Data/DATRAS_08_06_2021/Beam_oth/HL_SNS_09_06_2021.csv")

# BTS area VIII - France

HH_BT8 <- getDATRAS(record = "HH", survey = "BTS-VIII",
                    years = 2011:2020, quarters = c(1,2,3,4))

HL_BT8<- getDATRAS(record = "HL", survey = "BTS-VIII",
                   years = 2011:2020, quarters = c(1,2,3,4))

write.csv(HH_BT8, "Raw_Data/DATRAS_08_06_2021/Beam_oth/HH_BTS8_09_06_2021.csv")
write.csv(HL_BT8, "Raw_Data/DATRAS_08_06_2021/Beam_oth/HL_BTS8_09_06_2021.csv")


# DYFS - Inshore Beam Trawl (young fish survey)


HH_DYFS <- getDATRAS(record = "HH", survey = "DYFS",
                    years = 2002:2020, quarters = c(1,2,3,4))

HL_DYFS<- getDATRAS(record = "HL", survey = "DYFS",
                   years = 2002:2020, quarters = c(1,2,3,4))

write.csv(HH_DYFS, "Raw_Data/DATRAS_08_06_2021/Beam_oth/HH_DYFS_09_06_2021.csv")
write.csv(HL_DYFS, "Raw_Data/DATRAS_08_06_2021/Beam_oth/HL_DYFS_09_06_2021.csv")

### Spanish Bottom Trawl surveys for reference. 

# Spanish Porcupine survey

HH_SPP <- getDATRAS(record = "HH", survey = "SP-PORC",
                     years = 2001:2020, quarters = c(1,2,3,4))

HL_SPP<- getDATRAS(record = "HL", survey = "SP-PORC",
                    years = 2001:2020, quarters = c(1,2,3,4))

write.csv(HH_SPP, "Raw_Data/DATRAS_08_06_2021/Spain/HH_SP_PORC_09_06_2021.csv")
write.csv(HL_SPP, "Raw_Data/DATRAS_08_06_2021/Spain/HL_SP_PORC_09_06_2021.csv")

# Spanish North Coast

HH_SPN <- getDATRAS(record = "HH", survey = "SP-NORTH",
                    years = 1990:2020, quarters = c(1,2,3,4))

HL_SPN<- getDATRAS(record = "HL", survey = "SP-NORTH",
                   years = 1990:2020, quarters = c(1,2,3,4))

write.csv(HH_SPN, "Raw_Data/DATRAS_08_06_2021/Spain/HH_SP_NORTH_09_06_2021.csv")
write.csv(HL_SPN, "Raw_Data/DATRAS_08_06_2021/Spain/HL_SP_NORTH_09_06_2021.csv")

# Spanish Gulf of Cadiz

HH_SPA <- getDATRAS(record = "HH", survey = "SP-ARSA",
                    years = 1996:2020, quarters = c(1,2,3,4))

HL_SPA<- getDATRAS(record = "HL", survey = "SP-ARSA",
                   years = 1996:2020, quarters = c(1,2,3,4))

write.csv(HH_SPA, "Raw_Data/DATRAS_08_06_2021/Spain/HH_SP_ARSA_09_06_2021.csv")
write.csv(HL_SPA, "Raw_Data/DATRAS_08_06_2021/Spain/HL_SP_ARSA_09_06_2021.csv")