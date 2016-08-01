#' Function to add distance to a list of polygons (e.g. range map) to a stack
#'
#' @param in.stk INLA data stack.
#' @param coords Names of coorinates.
#' @param polys List of polygons (a spatialpolygonsdataframe will work).
#' @param name Name to be given to distance (defaults to "dist").
#' @param scale Should the distance be scaled by dividing by the mean of the non-zero distances? (defaults to FALSE, eitehr logical or numeric).
#'
#' @return An INLA stack, with the distance to the polygon added
#'
#' @export
#' @import sp
AddDistToRangeToStack <- function(in.stk, coords, polys, name="dist", scale=FALSE) {
  if(!is.logical(scale) & !is.numeric(scale)) stop("scale should be numeric or logial")
  if(is.numeric(scale) & !(scale>0)) stop("scale should be positive")
  if(!all(coords%in%names(in.stk$effects$ncol))) stop("Coordinates not in effects")

  # calculate distances
  Locs <- in.stk$effects$data[,coords]
  Use <- !is.na(Locs[,1])
  Dist <- as.numeric(Use)
  if("package:parallel" %in% search()) {
    InPolys <- data.frame(parallel::mclapply(polys, function(ply, pts)
      point.in.polygon(pts[,1], pts[,2], ply[,1], ply[,2]), pts=Locs[Use,]))
  } else {
    InPolys <- data.frame(lapply(polys, function(ply, pts)
      point.in.polygon(pts[,1], pts[,2], ply[,1], ply[,2]), pts=Locs[Use,]))
  }
  InPoly <- apply(InPolys,1, function(vec) any(vec>0))
  Dist[Use] <- as.numeric(!InPoly)
  if("package:parallel" %in% search()) {
    dists <- parallel::mclapply(polys, function(ply, pts) geosphere::dist2Line(pts, ply)[,"distance"], pts=Locs[Dist==1,])
  } else {
    dists <- lapply(polys, function(ply, pts) geosphere::dist2Line(pts, ply)[,"distance"], pts=Locs[Dist==1,])
  }
  Dist[Dist==1] <- apply(as.data.frame(dists),1,min)

  # scale distances
  if(is.logical(scale)) Dist <- Dist/mean(Dist[Dist>0])
  if(is.numeric(scale)) Dist <- Dist/scale

  # Add distance to stack
  in.stk$effects$data[,name] <- Dist
  in.stk$effects$ncol <- rep(1,ncol(in.stk$effects$data))
  attr(in.stk$effects$ncol, "names") <- names(in.stk$effects$data)
  in.stk$effects$names[name] <- name

  in.stk
}
