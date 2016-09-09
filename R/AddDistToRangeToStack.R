#' Function to add distance to a list of polygons (e.g. range map) to a stack
#'
#' @param in.stk INLA data stack.
#' @param coords Names of geographic coordinates used in in.stk.
#' @param polynoms List of polygons (a spatialpolygonsdataframe will work).
#' @param scale Should the distance be scaled by dividing by the mean of the non-zero distances? (defaults to FALSE, eitehr logical or numeric).
#'
#' @return An INLA stack, with the distance to the polygon added
#'
#' @export
#' @import sp
AddDistToRangeToStack <- function(in.stk, coords, polynoms, scale=FALSE) {
  if(!is.logical(scale) & !is.numeric(scale)) stop("scale should be numeric or logial")
  if(is.numeric(scale) & !(scale>0)) stop("scale should be positive")
  if(!all(coords%in%names(in.stk$effects$ncol))) stop("Coordinates not in effects")

  # calculate distances
  UsePts <- !is.na(in.stk$effects$data[,coords[1]])
  Locations <- SpatialPoints(in.stk$effects$data[UsePts,coords], proj4string = CRS(proj4string(polynoms)))

  Dists <- AddDistToRangeToSpatialPoints(data = Locations, polynoms = polynoms, scale=scale)

  # Add distance to stack
  in.stk$effects$data[,names(Dists@data)] <- NA
  in.stk$effects$data[UsePts,names(Dists@data)] <- Dists@data
  in.stk$effects$ncol <- rep(1,ncol(in.stk$effects$data))
  attr(in.stk$effects$ncol, "names") <- names(in.stk$effects$data)
  in.stk$effects$names[names(Dists@data)] <- names(Dists@data)

  in.stk
}
