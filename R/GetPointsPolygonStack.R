#' Function to create stack for small presence/absence polygons
#'
#' @param polys Spatial polygon data frame.
#' @param data Data frame with columns for coordinates, and others are covariates.
#' @param coords Names for columns with coordinares in data.
#' @param polydatanames Names of covariate data in polygon object.
#' @param respName Name for response (e.g. presence/absence). Might be possible to use >1, but haven't tried.
#' @param tol Tolerance for converting points to spatial pixels.
#' @param tag Name for tag for the stack (defaults to "points").
#' @param mesh INLA mesh.
#' @param InclCoords Boolean, shoiuld coordinates be included in data (defaults to FALSE).
#'
#' @return An INLA stack, which will be treated as points in the analysis.
#'
#' @export
#' @import sp
#' @import INLA

GetPointsPolygonStack=function(polys, data, coords=c("X","Y"), mesh, polydatanames,
                               respName="Present", tol=1e-1, tag="polygon", InclCoords=FALSE) {
  if(!all(coordnames%in%names(data))) stop("Coordinates not in the data")
  sp.points.df <- SpatialPointsDataFrame(data[,coords], data=data, proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.pixels <- SpatialPixels(sp.points.df, tolerance = tol)
  data.spdf <- SpatialPixelsDataFrame(sp.pixels, data, tolerance = tol)  # this might also leave the Lon and Lat columns in your SpatialPixelsDataFrame which you can manually remove
  data.raster <- raster::brick(data.spdf)

  dat=as.data.frame(raster::extract(data.raster, polys, small=TRUE, fun=mean))
  dat[,polydatanames] <- polys@data[,polydatanames]
  dat$X=coordinates(polys)[,1]
  dat$Y=coordinates(polys)[,2]

  Area=sapply(polys@polygons, function(x) x@area) # area of each polygon
  if(InclCoords) {
    CovEffects <- dat
  }  else  {
    CovEffects <- dat[,!names(dat)%in%coords]
  }
  #  CovEffects$Intercept=1

  projmat <- inla.spde.make.A(mesh, as.matrix(dat[,c("X","Y")])) # from mesh to presence/absence

  stk <- inla.stack(list(y=cbind(NA, dat[,respName]), e=Area),
                    A=list(1,projmat), tag=tag, Ntrials=rep(1, nrow(dat)),
                    effects=list(CovEffects, list(i=1:mesh$n)))
  stk
}
