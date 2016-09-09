#' Function to add distance to a polygon to a spatial points object
#' @param data SpatialPoints* object
#' @param polynoms A spatialpolygonsdataframe.
#' @param scale Should the distance be scaled by dividing by the mean of the non-zero distances? (defaults to FALSE, either logical or numeric).
#'
#' @return A spatial points data frame with distances to the polygon added.
#'
#' @export
#' @import sp
#' @import INLA
AddDistToRangeToSpatialPoints <- function(data, polynoms, scale=FALSE) {
  if(!grepl("^SpatialPoints", class(data))) stop("data should be a SpatialPoints* object")
  if(!is.logical(scale) & !is.numeric(scale)) stop("scale should be numeric or logial")
  if(is.numeric(scale) & !(scale>0)) stop("scale should be positive")

  # calculate distances
  DistToPolys <- sapply(seq_along(polynoms), function(wh, Polys, dat) {
    NPl <- over(x=dat, y=Polys[wh,])
    if(!is.vector(NPl)) NPl <- NPl[,1] # hopefully this works!
    NotInPoly <- is.na(NPl) # NA if points no in a polygon
    Dist <- as.numeric(NotInPoly)
    if(any(NotInPoly==1)) {
      Dist[NotInPoly] <- geosphere::dist2Line(p=dat[NotInPoly,], line=Polys[wh,])[,"distance"]
    }
    Dist
  }, Polys=polynoms, dat=data)

  # scale distances
  if(is.logical(scale)) DistToPolys <- sweep(DistToPolys, 2, apply(DistToPolys, 2, function(x) mean(x[x>0])), "/")
  if(is.numeric(scale)) DistToPolys <- DistToPolys/scale
  if(!is.null(names(polynoms@polygons))) {
    colnames(DistToPolys) <- names(polynoms@polygons)
  } else {
    colnames(DistToPolys) <- paste0("DistToPoly", seq_along(polynoms@polygons))
  }
  # Add distance to stack
  if(class(data)!="SpatialPointsDataFrame") {
    data <- SpatialPointsDataFrame(data, data=as.data.frame(DistToPolys))
  } else {
    data@data[,colnames(DistToPolys)] <- DistToPolys
  }
  data
}
