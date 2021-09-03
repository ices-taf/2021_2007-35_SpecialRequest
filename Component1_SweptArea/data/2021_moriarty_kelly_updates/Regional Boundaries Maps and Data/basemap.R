# New BaseMap
require(rgdal)

dev.new()
#load in basemap files for all maps
#set directory to working folder and insure subfolders are availible

europe<-readOGR("./shapes//europe.dbf","europe")
contour_1000m<-readOGR(".//shapes//1000m.dbf","1000m")
NWW_boundary<-readOGR(".//shapes//NWW_boundary.dbf","NWW_boundary")
contour_200m<-readOGR(".///shapes//200m.dbf","200m")
ospar<-readOGR(".//ospar_boundaries//OSPAR_inner_Boundary.dbf", "OSPAR_inner_Boundary")
spain<-readOGR(".//Sp-NGFS.WGS84//Sp_North.WGS84.dbf", "Sp_North.WGS84")
par(mai=c(0,0,0,0),bg='white' )
sco1<-readOGR(".//SWCQ1.WGS84//SWC_Q1.dbf", "SWC_Q1")
sco3<-readOGR(".//SWCQ4.WGS84//SWC_Q4.dbf", "SWC_Q4")
ices<-readOGR(".//ICES_rectangles_statistics/ICES_rectangles_statistics.dbf", "ICES_rectangles_statistics")
ire<-readOGR(".//IGFS.WGS84//IGFS.WGS84.dbf", "IGFS.WGS84")
ni<-readOGR(".//NI-IBTS.WGS84//NI_IBTS.WGS84.dbf", "NI_IBTS.WGS84")
evhoe<-readOGR(".//Fr-EVHOE.WGS84//EVHOE.WGS84.dbf", "EVHOE.WGS84")
rock<-readOGR(".//SWC-RockQ3.WGS84//SWC_Q3.dbf", "SWC_Q3")
summary(h$ShootLat)
summary(h$ShootLong)
plot(NWW_boundary, border="white", xlim = c(-16,13), ylim = c(36, 62))
plot(europe,add=TRUE, xlim = c(-16, 13), ylim = c(36, 62), asp = 1, col=c('gray81'), 
     border='gray3')
#plot(contour_200m, add=TRUE, col="lightblue3")
#plot(contour_1000m, add=TRUE, col="lightblue4")
plot(ices, add=TRUE)
text(ices$Centr_X, ices$Centr_Y, ices$ICESNAME)
plot(ospar, col="red", add=T, lwd=4)

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


