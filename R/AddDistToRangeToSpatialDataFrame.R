#' Function to add distance to a polygon to spatial points data frame
#' @param data Data points frame (might work for other spatial data frames too).
#' @param polys A spatialpolygonsdataframe.
#' @param scale Should the distance be scaled by dividing by the mean of the non-zero distances? (defaults to FALSE, either logical or numeric).
#'
#' @return The spatial points data frame with distances to the polygon added
#'
#' @export
#' @import sp
#' @import INLA
AddDistToRangeToSpatialDataFrame <- function(data, polys, scale=FALSE) {
  if(!is.logical(scale) & !is.numeric(scale)) stop("scale should be numeric or logial")
  if(is.numeric(scale) & !(scale>0)) stop("scale should be positive")

  # calculate distances
  DistToPolys <- sapply(seq_along(polys), function(wh, Polys, Data) {
    NotInPoly <- is.na(over(x=Data, y=Polys[wh,])$crtdb_d)
    Dist <- as.numeric(NotInPoly)
    Dist[NotInPoly] <- geosphere::dist2Line(p=Data[NotInPoly,], line=Polys[wh,])[,"distance"]
    Dist
  }, Polys=polys, Data=data)

  # scale distances
  if(is.logical(scale)) DistToPolys <- sweep(DistToPolys, 2, apply(DistToPolys, 2, function(x) mean(x[x>0])), "/")
  if(is.numeric(scale)) DistToPolys <- DistToPolys/scale
  if(!is.null(names(polys@polygons))) {
    colnames(DistToPolys) <- names(polys@polygons)
  } else {
    colnames(DistToPolys) <- paste0("DistToPoly", seq_along(polys@polygons))
  }

  # Add distance to stack
  data@data[,colnames(DistToPolys)] <- DistToPolys
  data
}
