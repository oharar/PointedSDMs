# Format Solitary Tinomou data
# Data was given as a single data frame plus (later) a polygon for the range map.
library(devtools)
library(spatstat)
library(splancs)
library(rgbif)

# GBIF
Names <- name_suggest("tinamus solitorius", rank="species")
SolTin.key <- Names$key[grep("solitarius", Names$canonicalName)]
SolTinGBIF.all <- occ_search(taxonKey = SolTin.key, hasCoordinate = TRUE) #, eventDate = 2010)
SolTinGBIF <- SolTinGBIF.all$data[!grepl("EBIRD", SolTinGBIF.all$data$collectionCode),
                                  c("decimalLatitude", "decimalLongitude", "basisOfRecord", "stateProvince",
                                    "eventDate", "geodeticDatum", "locality", "collectionCode")]

SolTin_gbif <- data.frame(X = SolTinGBIF$decimalLongitude, Y = SolTinGBIF$decimalLatitude)
SolTin_gbif <-SolTin_gbif[SolTin_gbif$X<0,]

# eBird (from GBIF. Not a lot of data)
SolTineBird <- SolTinGBIF.all$data[grepl("EBIRD", SolTinGBIF.all$data$collectionCode),
                                  c("decimalLatitude", "decimalLongitude", "basisOfRecord", "stateProvince",
                                    "eventDate", "geodeticDatum", "locality", "collectionCode")]
SolTin_ebird <- data.frame(X = SolTineBird$decimalLongitude, Y = SolTineBird$decimalLatitude)
SolTin_ebird <-SolTin_ebird[SolTin_ebird$X<0,]

###########
# load other data
load("data-raw/SolTinamou.RData")

CovsToUse <- c( "For", "NPP", "Alt")
# Split Data into different data types

# Range map
range.mask=as.owin(cbind(Data[,c("Xorig","Yorig")],In=Data$range), step=c(0.25, 0.3))
range.mask$m[is.na(range.mask$m)] <- FALSE
range.poly=simplify.owin(as.polygonal(range.mask), dmin=0.5)
plot(range.poly)

SolTin_range <- data.frame(X=range.poly$bdry[[1]]$x, Y=range.poly$bdry[[1]]$y)

# ebird
#  SolTin_ebird <- Data[Data$ebird_pres>0, c("X", "Y")]
# gbif
#  SolTin_gbif <- Data[Data$gbif_pres>0, c("X", "Y")]

# Parks
Data$IsPark=as.logical(apply(Data[,c("ParkPres","ParkAbs")],1,sum))
parks.mask=as.owin(cbind(Data[,c("Xorig","Yorig")],In=Data$IsPark), step=c(0.25, 0.3))
parks.mask$m[is.na(parks.mask$m)] <- FALSE
parks.poly=simplify.owin(as.polygonal(parks.mask), dmin=0.5)
plot(parks.poly)

# lapply over parks.poly[[4]] to get mean of covariates in the polygon, plus whether the species was observed in it
Parks.lst=lapply(parks.poly[[4]], function(poly, data) {
  IN=inpip(data[,c("Xorig","Yorig")], poly)
  c(apply(data[IN,],2,mean), area=poly$area)
  #}, data=Data[,c("X","Y", gsub("RangeF", "range",CovsToUse), "ParkPres")]) # use this line if range should be a factor
}, data=Data[,c("Xorig","Yorig", CovsToUse, "ParkPres")])

SolTin_parks <- as.data.frame(
  sapply(c("Xorig","Yorig", CovsToUse, "ParkPres","area"), function(wh, lst) {
  unlist(lapply(lst, function(list, WH) list[[WH]], WH=wh))
}, lst=Parks.lst, simplify=TRUE))
SolTin_parks$Present <- SolTin_parks$ParkPres>0
SolTin_parks <- SolTin_parks[,c("Xorig", "Yorig", "area", "Present")]
names(SolTin_parks) <- gsub("orig","", names(SolTin_parks))

# Environmenal covariates
SolTin_covariates <- Data[,c("Xorig", "Yorig", "For", "NPP", "Alt")]
names(SolTin_covariates) <- c("X", "Y", "Forest", "NPP", "Altitude")

# Write data
use_data(SolTin_ebird, overwrite = TRUE) #, pkg=PointedSDMs)
use_data(SolTin_gbif, overwrite = TRUE) #, pkg=PointedSDMs)
use_data(SolTin_parks, overwrite = TRUE) #, pkg=PointedSDMs)
use_data(SolTin_covariates, overwrite = TRUE) #, pkg=PointedSDMs)
use_data(SolTin_covariates, overwrite = TRUE) #, pkg=PointedSDMs)
use_data(SolTin_range, overwrite = TRUE) #, pkg=PointedSDMs)
