setwd("/Users/Maxwell/Documents/MRT/")
library(rts)
#setNASAauth()
modisProducts()
setMRTpath("/Users/Maxwell/MRT/bin/", update=T)
x="MOD13A3"
end<-paste0(2001:2018, rep(".05.31",18))
end
paste((start), ", ", paste(end))
for(i in start[1]){
  for(j in end[1]){
ModisDownload("MOD13A3",h=c(8, 9),v=c(4,4),dates=c(start,end),bands_subset="0 1 0 0 0 0 0 0 0 0 0", proj_type="UTM",proj_params="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",utm_zone=c(10,11),datum="WGS84",pixel_size=1000, mosaic = T, proj=T, UL =c(-130446, 5150545), LR=c(200000, 4649751), version='006')
  }
}

ModisDownload("MOD13A3",h=c(8, 9),v=c(4,4),dates=c('2017.04.01','2017.04.31'),bands_subset="0 1 0 0 0 0 0 0 0 0 0", proj_type="UTM",proj_params="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",utm_zone=c(10,11),datum="WGS84",pixel_size=1000, mosaic = T, proj=T, UL =c(-130446, 5150545), LR=c(200000, 4649751), version='006')

library(raster)
library(rgeos)
listfiles <- list.files(pattern='.tif$')
listfiles
r<-NULL
evistack<-NULL
years<-2001:2017
for(i in listfiles){
 r<-raster(i)/10000
 evistack<-stack(r, evistack)
}
names(evistack)<-paste0(rep("EVI",17), 2017:2001)
levelplot(evistack, pretty=T)
EVIdiff<-EVI2010-EVI2001
EVIdiff[EVIdiff == 0] <- NA
EVIdiff<-scale(EVIdiff)
plot(EVIdiff, col = rainbow(n=1000, v=1, start=2.5/6, end=4/6))
library(rgdal)
oregon<-readOGR(dsn="/Users/Maxwell/Documents/geospatial/orcnty2015/", layer = "orcntyline")
plot(oregon)
crs(oregon)
orutm<-spTransform(oregon, crs(EVI2001))
orutm<-crop(orutm, extent(EVI2001))
orEVI<-mask(EVI2001, orutm)
plot(orEVI)
plot(orutm)
plot(EVIdiff)
plot(orutm, add=T)

evistack<-stack(EVI2001/10000, EVI2002/10000, EVI2010/10000)
library(rasterVis)
levelplot(evistack)
EVI2001[EVI2001 == 0] <- NA
mask(EVI2001, )
levelplot(evistack, pretty=T)
