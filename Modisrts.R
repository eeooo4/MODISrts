setwd("/Users/Maxwell/Documents/MRT/")
library(rts)
#need to set nasa authorization (username and password), but only need to do once with the function below
#setNASAauth()
modisProducts() #check out possible products (Beware, the version of Modis matters for extracting with ModisDownload later)
setMRTpath("/Users/Maxwell/MRT/bin/", update=T) #set the path of the modis reprojection tool for the functions to find

#make a date sequence to extract
start<-NULL
end<-NULL
start<-paste0(rep(2001:2017,1) , rep(".05.01",17))
end<-paste0(rep(2001:2017,1),rep(".05.31",17))
start
end
#combine into one statement for each desired date so it is easy to loop
dateseq<-NULL
dateseq<-paste((start), noquote("\",\""), paste(end), sep = "") 
dateseq
#extract dateseq for all of oregon, mosaic, and reproject all in one using ModisDownload
  ##note the "MOD13A3" is the modis product
    ###h and v are the modis tiles (can look up online which oncs youll need)
      ####bands - each modis product has a bunch of bands, so here can specify which ones you want, need to put 0 or 1 for all or else it may do somethin weird
        #####proj_params Something weird, look up in the package to get a better idea of what to do here.
          ######utm_zone only needed if using UTM
            ######Datum: projection, pixel size: set what you what, mosaic: default is FALSE, need to set to true to mosaic. If you want there is a separate funciton to do it manually. Same                with proj, which reprojects the mosaicked file to a tif. Versopm: important, seems like 006 has everything I wanted, 005 didnt have much.
for(i in dateseq){
ModisDownload("MOD13Q1",h=c(8, 9),v=c(4,4),dates=c(i),bands_subset="0 1 0 0 0 0 0 0 0 0 0", proj_type="UTM",proj_params="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",utm_zone=c(10,11),datum="WGS84",pixel_size=250, mosaic = T, proj=T, version='006')
}

#Cascade Swath, just a different code using UL and LR to specifiy upper left and lower right bounds in the reprojected coordinate system for the final product. Can act weird...
ModisDownload("MOD13Q1",h=c(8, 9),v=c(4,4),dates=c('2001.05.01','2001.05.31'),bands_subset="0 1 0 0 0 0 0 0 0 0 0", proj_type="UTM",proj_params="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",utm_zone=c(10,11),datum="WGS84",pixel_size=250, mosaic = T, proj=T, version='006')

ModisDownload("MOD13Q1",h=c(8, 9),v=c(4,4),dates=c('2017.05.01','2017.05.31'),bands_subset="0 1 0 0 0 0 0 0 0 0 0", proj_type="UTM",proj_params="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",utm_zone=c(10,11),datum="WGS84",pixel_size=1000, mosaic = T, proj=T, UL =c(-130446, 5150545), LR=c(-200000, 4649751), version='006')


library(raster)
library(rgeos)
#Show files of some pattern in WD
listfiles <- list.files(pattern='05-01')
listfiles
listfiles<-listfiles[1:18]
listfiles
#Downloaded this separately to crop by
oregon<-readOGR(dsn="/Users/Maxwell/Documents/geospatial/orcnty2015/", layer = "orcntypoly")
orutm<-spTransform(oregon, crs(raster(listfiles[1])))
plot(orutm)
#make a loop to read in all the tif files and crop them to oregon size, then put them all into a rasterstack
r<-NULL
evimay<-NULL
for(i in listfiles){
 r<-raster(i)/10000 #divide by 10000 to get into correct units.
 r<-crop(r, orutm)
 r<-mask(r, orutm)
 evimay<-stack(r, evimay)
}
#changes names to useful ones
names(evimay)<-paste0(rep("EVI",18),"May", 2017:2000)
cellStats(evimay[[1]]<(-0.2), stat='sum')
evimay[evimay<(-0.2)]<-NA
plot(evimay)
diffstackOR<-eviOR-eviOR[[19]]
par(mfrow=c(2,1))
plot(evimay[[1]], colNA="lightblue", main = "EVI March, 2018")
plot(orutm, add=T)
plot(diffstackOR[[1]], colNA="lightblue", main = "EVI March, 2018-March 2000")
plot(orutm, add=T)
library(rasterVis)
library(rgdal)
library(sp)
cols=rasterTheme(region=brewer.pal('Greens', n=9))
cols2=rasterTheme(region=brewer.pal(n=11,"BrBG"))
levelplot(scale(diffstackOR[[1]]), pretty=T, par.settings=cols2)
levelplot(scale(eviOR[[1]]), pretty=T, par.settings=cols2)
levelplot(eviOR, pretty=T, par.settings = cols2)


#Find coefficients between rasterstack raster cells over time
lm_fun = function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }} # slope
lm_fun_int = function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[1] }} #intercept
lm_fun_p = function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }} #R2
#check the math on sample set
r <- raster(ncol=10, nrow=10)
set.seed(99)
s <- stack(sapply(1:4, function(i) setValues(r, rnorm(100,0,1)*6)))
time<-1:4
stest<-calc(s, lm_fun)
plot(stest)
as.data.frame(s)
as.data.frame(stest)
#first cell
stest[1]
#calculate the slope through that cell manually
y<-as.vector(as.data.frame(s[1,1]))
y<-as.numeric(y)
class(y)
x<-1:4
class(x)
summary(lm(y~x))$coefficients[2]

#actual data calculation
time<-18:1

#March
eviintercept= calc(eviOR, lm_fun_int)
evip= calc(eviOR, lm_fun_p)
evip.sig<-evip
evip.sig<-evip.sig[evip.sig>0.05]<-NA
plot(evip.sig)

evislope= calc(eviOR, lm_fun)
evislopeadj<-evislope
evislopeadj[evislopeadj>0.1]<-NA
levelplot(evislopeadj, par.settings=cols2)

#May
eviintmay= calc(evimay, lm_fun_int)
evipmay= calc(evimay, lm_fun_p)
evip.sig.may<-evip
evip.sig.may<-evip.sig.may[evip.sig.may>0.05]<-NA
plot(evip.sig.may)

evislopemay= calc(evimay, lm_fun)
evislopeadjmay<-evislopemay
plot(evislopeadjmay)
plot(evislopeadjmay[evislopeadjmay>0])
evislopeadjmay[evislopeadjmay<(-0.02)]<-NA
evislopeadjmay[evislopeadjmay>0.02]<-NA
plot(evislopeadjmay)
levelplot(scale(evislopeadjmay), par.settings=cols2)


levelplot(evitrend, contours=T, pretty=T)
extent(evislopeadj)
map <- openmap (c( 45.587077, -122.551729), c( 41.989833, -121.104984),type='esri-topo')#provides the map boundary using upper-left and lower-right corners
plot(map)
map_utm <- openproj(map, projection =crs(evislopeadj))
crs(map_utm)
plot(map_utm)

plot(map_utm)
plot(evitrend, add=T, alpha=0.75)
plot(map_utm, add=T, alpha=0.75)
evitrend[evitrend>0.02]<-NA
evitrend[evitrend<(-0.02)]<-NA
levelplot(evitrend, par.settings= cols)

plot(eviOR[[1]], colNA="lightblue", main = "EVI March, 2018")
plot(orutm, add=T)
plot(diffstackOR[[1]], colNA="lightblue", main = "EVI March, 2018-March 2000")
plotstack<-stack(scale(evimay[[1]]), scale(evislopeadjmay))
names(plotstack)<-c("EVI May 2017", "EVI Trend\n ∆EVI/Year")
levelplot(plotstack, par.settings=cols2, pretty=T, layout=c(2,1))
par(mfrow =c(1,3))
plot(eviOR[[1]])
plot(diffstackOR[[1]])


raster::plot(map_utm)
plot(eviOR[[1]], add=T, alpha=0.7)
writeRaster(evislopeadj, "evislopeadj.tif")

par(mfrow=c(2,1)
plot(evislopeadj)
plot(evislopeadjmay)
