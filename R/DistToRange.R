#' Function to calculate  distance to a list of polygons (e.g. range map) from a matrix of points
#'
#' @param polys List of polygons (a spatialpolygonsdataframe will work).
#' @param points Names of coorinates.
#'
#' @return A vector of distances
#'
#' @details This should probably be deprecated or re-written to use rgeos::gDistance()
#'
#' @export
#' @import sp
DistToRange <- function(polys, points) {
  #  points=Points.df[1:50,]; polys=PolyCoords[[1]]
  InPolys <- data.frame(lapply(polys, function(ply, pts)
    point.in.polygon(pts[,1], pts[,2], ply[,1], ply[,2]), pts=points))
  InPoly <- apply(InPolys,1, function(vec) any(vec>0))
  Dist <- as.numeric(!InPoly)
  Dist[!InPoly] <- apply(data.frame(lapply(polys, function(ply, pts)
    geosphere::dist2Line(pts, ply)[,"distance"], pts=points[!InPoly,])),1,min) # replace with gDistance?
  Dist
}
