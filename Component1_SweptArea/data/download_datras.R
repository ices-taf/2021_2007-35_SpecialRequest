

## Haul info from Datras
    hh.ns <- getDATRAS(record='HH', survey='NS-IBTS', years=c(1967:last.year), quarters=c(1,3))
    hh.baltic <- getDATRAS(record='HH', survey='BITS', years=c(1991:last.year), quarters=c(1,4))
    hh.evhoe <- getDATRAS(record='HH', survey='EVHOE', years=c(1997:last.year), quarters=4)
    hh.cgfs <- getDATRAS(record='HH', survey='FR-CGFS', years=c(1998:last.year), quarters=4)
    hh.igfs <- getDATRAS(record='HH', survey='IE-IGFS', years=c(2003:last.year), quarters=4)
    hh.nigfs <- getDATRAS(record='HH', survey='NIGFS', years=c(2005:last.year), quarters=c(1:4))
    hh.pt <- getDATRAS(record='HH', survey='PT-IBTS', years=c(2002:last.year), quarters=c(3:4))
    hh.rock <- getDATRAS(record='HH', survey='ROCKALL', years=c(1999:2009), quarters=3)
    hh.scorock <- getDATRAS(record='HH', survey='SCOROC', years=c(2011:last.year), quarters=3)
    hh.spa <- getDATRAS(record='HH', survey='SP-ARSA', years=c(1996:last.year), quarters=c(1:4))
    hh.spn <- getDATRAS_rob(record='HH', survey='SP-NORTH', years=c(1990:last.year), quarters=c(3:4))
    hh.spp <- getDATRAS_rob(record='HH', survey='SP-PORC',
                        years=c(2001:last.year), quarters=c(3:4)) ## CHECK: problem with 2016
    hh.sns <- getDATRAS(record='HH', survey='SNS', years=c(2002:last.year), quarters=c(3:4))
    hh.swc <- getDATRAS_rob(record='HH', survey='SWC-IBTS',
                            years=c(1985:2010), quarters=c(1:4)) ## CHECK: problem with 1987
    hh.scowcgfs <- getDATRAS(record='HH', survey='SCOWCGFS', years=c(2011:last.year), quarters=c(1:4))
    hh.bts <- getDATRAS(record='HH', survey='BTS', years=c(1985:last.year), quarters=c(1:4))
    hh.bts8 <- getDATRAS(record='HH', survey='BTS-VIII', years=c(2011:last.year), quarters=4)
    hh.dyfs <- getDATRAS(record='HH', survey='DYFS', years=c(2002:last.year), quarters=c(3,4))

    hh <- rbind(hh.ns, hh.baltic, hh.evhoe, hh.cgfs, hh.igfs, hh.nigfs, hh.pt, hh.rock, hh.scorock, hh.spa,
                hh.spn, hh.spp, hh.sns, hh.swc, hh.scowcgfs, hh.bts, hh.bts8, hh.dyfs)

    ## Length info from DATRAS
    hl.ns <- getDATRAS(record='HL', survey='NS-IBTS', years=c(1967:last.year), quarters=c(1,3))
    hl.baltic <- getDATRAS(record='HL', survey='BITS', years=c(1991:last.year), quarters=c(1,4))
    hl.evhoe <- getDATRAS(record='HL', survey='EVHOE', years=c(1997:last.year), quarters=4)
    hl.cgfs <- getDATRAS(record='HL', survey='FR-CGFS', years=c(1998:last.year), quarters=4)
    hl.igfs <- getDATRAS(record='HL', survey='IE-IGFS', years=c(2003:last.year), quarters=4)
    hl.nigfs <- getDATRAS(record='HL', survey='NIGFS', years=c(2005:last.year), quarters=c(1:4))
    hl.pt <- getDATRAS(record='HL', survey='PT-IBTS', years=c(2002:last.year), quarters=c(3:4))
    hl.rock <- getDATRAS(record='HL', survey='ROCKALL', years=c(1999:2009), quarters=3)
    hl.scorock <- getDATRAS(record='HL', survey='SCOROC', years=c(2011:last.year), quarters=3)
    hl.spa <- getDATRAS(record='HL', survey='SP-ARSA', years=c(1996:last.year), quarters=c(1:4))
    hl.spn <- getDATRAS_rob(record='HL', survey='SP-NORTH', years=c(1990:last.year), quarters=c(3:4))
    ##
    ## hl.spn <- getDATRAS(record='HL', survey='SP-NORTH', years=c(1990:2012), quarters=c(3:4))
    ## hl.spn2 <- getDATRAS(record='HL', survey='SP-NORTH', years=c(2014:last.year), quarters=c(3:4))
    ##                                     # there is a problem with SPNorth for year 2013, so did not load the data for that year
    ## hl.spn <- rbind(hl.spn, hl.spn2)
    hl.spp <- getDATRAS_rob(record='HL', survey='SP-PORC', years=c(2001:last.year), quarters=c(3:4)) ## CHECK: problem with 2016
    hl.sns <- getDATRAS(record='HL', survey='SNS', years=c(2002:last.year), quarters=c(3:4))
    hl.swc <- getDATRAS_rob(record='HL', survey='SWC-IBTS',
                            years=c(1985:2010), quarters=c(1:4)) ## CHECK: problem with 1987
    hl.scowcgfs <- getDATRAS(record='HL', survey='SCOWCGFS', years=c(2011:last.year), quarters=c(1:4))
    hl.bts <- getDATRAS(record='HL', survey='BTS', years=c(1985:last.year), quarters=c(1:4))
    hl.bts8 <- getDATRAS(record='HL', survey='BTS-VIII', years=c(2011:last.year), quarters=4)
    hl.dyfs <- getDATRAS(record='HL', survey='DYFS', years=c(2002:last.year), quarters=c(3,4))

    hl <- rbind(hl.ns, hl.baltic, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.pt, hl.rock, hl.scorock, hl.spa,
                hl.spn, hl.spp, hl.sns, hl.swc, hl.scowcgfs, hl.bts, hl.bts8, hl.dyfs)

    rm(hl.ns, hl.baltic, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.pt, hl.rock, hl.scorock, hl.spa,
       hl.spn, hl.spp, hl.sns, hl.swc, hl.scowcgfs, hl.bts, hl.bts8, hl.dyfs,
       hh.ns, hh.baltic, hh.evhoe, hh.cgfs, hh.igfs, hh.nigfs, hh.pt, hh.rock, hh.scorock, hh.spa,
       hh.spn, hh.spp, hh.sns, hh.swc, hh.scowcgfs, hh.bts, hh.bts8, hh.dyfs)

    save(hh, hl, file = "data/hh_hl.RData", version=2)
