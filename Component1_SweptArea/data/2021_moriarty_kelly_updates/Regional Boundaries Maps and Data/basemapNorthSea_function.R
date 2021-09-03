setwd("~/Datras Project/Regional Boundaries Maps and Data")


require(rgdal)
#load in basemap files for all maps
#set directory to working folder and insure subfolders are availible
europe<-readOGR("./shapes//europe.dbf","europe")
contour_1000m<-readOGR("./shapes//1000m.dbf","1000m")
contour_200m<-readOGR("./shapes//200m.dbf","200m")
innerbound<-readOGR("./shapes/ospar_boundaries/OSPAR_Inner_Boundary.dbf", "OSPAR_Inner_Boundary")

bmnorthsea<- function(Title="Insert title") {
  # set parameters for background
  par(mai=c(0.2,0.2,0.2,0.2), bg="white")
  #add the coastline
  plot(europe, xlim = c(-3, 5), ylim = c(50, 62), asp = 1, col=c('gray79') ,border='gray9')
  #add some bathymetry lines
  plot(contour_200m, add=TRUE, col="grey47")
  plot(contour_1000m, add=TRUE, col="grey18")
  plot(innerbound, add=T, col="red", lwd=2.5)
}


plot(bmnorthsea)

