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
for(i in listfiles){
 r<-raster(i)/10000
 evistack<-stack(r, evistack)
}
names(evistack)<-paste0(rep("EVI",17),"April,", 2017:2001)
diffstack<-evistack-evistack[[17]]
library(rasterVis)library(rgdal)
oregon<-readOGR(dsn="/Users/Maxwell/Documents/geospatial/orcnty2015/", layer = "orcntyline")
plot(oregon)
crs(oregon)
orutm<-spTransform(oregon, crs(evistack))
plot(orutm)
orutm<-crop(orutm, extent(evistack))
#orEVI<-mask(EVI2001, orutm)
plot(orutm)
plot(diffstack)
plot(orutm, add=T)

library(rasterVis)
myTheme=rasterTheme(region=brewer.pal('Greens', n=9))
cols <- rasterTheme(region=brewer.pal(n=9,"YlGn"))
levelplot(scale(diffstack[[16]]), pretty=T, par.settings=cols)

colortest<-diffstack[[1]]
colortest[colortest<0.05&colortest>-0.05]<-NA
plot(colortest)
levelplot(diffstack[[1]], pretty=T)
plot(diffstack)

library(animation)
saveGIF(animate(evistack), movie.name="EVI.gif")
