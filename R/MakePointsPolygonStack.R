#' Function to create stack for small presence/absence polygons
#'
#' @param polys Spatial polygon data frame.
#' @param data Data frame with columns for coordinates, and others are covariates.
#' @param coords Names for columns with coordinates in data.
#' @param mesh INLA mesh.
#' @param polydatanames Names of covariate data in polygon object. Defaults to NULL, which means no covariate data at polygon level.
#' @param respName Name for response (e.g. presence/absence). Might be possible to use >1, but haven't tried.
#' @param tol Tolerance for converting points to spatial pixels.
#' @param tag Name for tag for the stack (defaults to "points").
#' @param InclCoords Boolean, should coordinates be included in data (defaults to FALSE).
#'
#' @return An INLA stack, which will be treated as points in the analysis.
#'
#' @export
#' @import sp
#' @import INLA

MakePointsPolygonStack <- function(polys, data, coords=c("X","Y"), mesh, polydatanames=NULL,
                                   respName="Present", tol=1e-1, tag="polygon", InclCoords=FALSE) {
  if(!all(coords%in%names(data))) stop("Coordinates not in the data")
  sp.points.df <- SpatialPointsDataFrame(data[,coords], data=data, proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.pixels <- SpatialPixels(sp.points.df, tolerance = tol)
  data.spdf <- SpatialPixelsDataFrame(sp.pixels, data, tolerance = tol)  # this might also leave the Lon and Lat columns in your SpatialPixelsDataFrame which you can manually remove
  data.raster <- raster::brick(data.spdf)

  dat <- as.data.frame(raster::extract(data.raster, polys, small=TRUE, fun=mean))
  dat[,respName] <- as.numeric(polys@data[,respName])
  if(!is.null(polydatanames)) dat[,polydatanames] <- polys@data[,polydatanames]
  dat$X <- coordinates(polys)[,1]
  dat$Y <- coordinates(polys)[,2]

  Area <- sapply(polys@polygons, function(x) x@area) # area of each polygon
  if(InclCoords) {
    CovEffects <- dat
  }  else  {
    CovEffects <- dat[,!names(dat)%in%coords]
  }
  #  CovEffects$Intercept=1

  projmat <- inla.spde.make.A(mesh, as.matrix(dat[,c("X","Y")])) # from mesh to presence/absence
  stk <- inla.stack(data=list(resp=cbind(NA, dat[,respName]), Ntrials=rep(1, nrow(dat)), e=Area),
                    A=list(1,projmat), tag=tag, effects=list(CovEffects, list(i=1:mesh$n)))
  stk
}
