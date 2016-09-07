# Format Solitary Tinomou data
# Data was given as a single data frame plus (later) a polygon for the range map.
library(devtools)
library(spatstat)
library(splancs)

# load data
load("data-raw/SolTinamou.RData")
load("data-raw/SolTinBoundary.RData") # range map

CovsToUse <- c( "For", "NPP", "Alt")
# Split Data into different data types

# ebird
SolTin_ebird <- Data[Data$ebird_pres>0, c("X", "Y")]

# gbif
SolTin_gbif <- Data[Data$gbif_pres>0, c("X", "Y")]

# Parks
Data$IsPark=as.logical(apply(Data[,c("ParkPres","ParkAbs")],1,sum))
parks.mask=as.owin(cbind(Data[,c("X","Y")],In=Data$IsPark))
parks.mask$m[is.na(parks.mask$m)] <- FALSE
parks.poly=simplify.owin(as.polygonal(parks.mask), dmin=0.5)

# lapply over parks.poly[[4]] to get mean of covariates in the polygon, plus whether the species was observed in it
Parks.lst=lapply(parks.poly[[4]], function(poly, data) {
  IN=inpip(data[,c("X","Y")], poly)
  c(apply(data[IN,],2,mean), area=poly$area)
  #}, data=Data[,c("X","Y", gsub("RangeF", "range",CovsToUse), "ParkPres")]) # use this line if range should be a factor
}, data=Data[,c("X","Y", CovsToUse, "ParkPres")])

SolTin_parks <- as.data.frame(
  sapply(c("X","Y", CovsToUse, "ParkPres","area"), function(wh, lst) {
  unlist(lapply(lst, function(list, WH) list[[WH]], WH=wh))
}, lst=Parks.lst, simplify=TRUE))
SolTin_parks$Present <- SolTin_parks$ParkPres>0
SolTin_parks <- SolTin_parks[,c("X", "Y", "area", "Present")]

SolTin_covariates <- Data[,c("ID", "For", "NPP", "Alt", "X", "Y")]

# Write data
use_data(SolTin_ebird, overwrite = TRUE) #, pkg=PointedSDMs)
use_data(SolTin_gbif, overwrite = TRUE) #, pkg=PointedSDMs)
use_data(SolTin_parks, overwrite = TRUE) #, pkg=PointedSDMs)
use_data(SolTin_covariates, overwrite = TRUE) #, pkg=PointedSDMs)

