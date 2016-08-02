#' Function to add distance to a list of polygons (e.g. range map) to a data frame
#' @param df Data frame.
#' @param coords Names of coordinates.
#' @param polys List of polygons (a spatialpolygonsdataframe will work).
#' @param name Name to be given to distance (defaults to "dist").
#' @param scale Should the distance be scaled by dividing by the mean of the non-zero distances? (defaults to FALSE, either logical or numeric).
#'
#' @return The df data frame with distances to the polygon added
#'
#' @export
#' @import sp
#' @import INLA
AddDistToRangeToDataFrame <- function(df, coords, polys, name="dist", scale=FALSE) {
  if(!is.logical(scale) & !is.numeric(scale)) stop("scale should be numeric or logial")
  if(is.numeric(scale) & !(scale>0)) stop("scale should be positive")
  #  in.stk <- stk.all; coords <- c("long", "lat"); polys <- PolyCoords$`iucn_birds-14034`; name <- "iucnbirds14034"
  if(!all(coords%in%names(df))) stop("Coordinates not in effects")

  # calculate distances
  Locs <- df[,coords]
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

  if(sum(Dist==1)>0) {
    if("package:parallel" %in% search()) {
      dists <- parallel::mclapply(polys, function(ply, pts) geosphere::dist2Line(pts, ply)[,"distance"],
                                  pts=Locs[Dist==1,])
    } else {
      dists <- lapply(polys, function(ply, pts) geosphere::dist2Line(pts, ply)[,"distance"], pts=Locs[Dist==1,])
    }
    Dist[Dist==1] <- apply(as.data.frame(dists),1,min)
  }
  # scale distances
  if(is.logical(scale)) Dist <- Dist/mean(Dist[Dist>0])
  if(is.numeric(scale)) Dist <- Dist/scale

  # Add distance to stack
  df <- cbind(df, Dist)
  #  names(df) <- gsub("^Dist$", name, names(df))
  names(df)[names(df)=="Dist"] <- name
  df
}
