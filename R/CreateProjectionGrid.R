#' Function to create stack for predictions
#' @param nxy Number of points in x and y directions.
#' @param mesh INLA mesh.
#' @param data Data frame with columns for coordinates, and others are covariates.
#' @param tag Name for tag for the stack (defaults to "points").
#' @param coordnames Names of coorinates (defaults to x and y)
#' @param boundary Boundary of region to project onto. Defaults to NULL, when the boundary of the mesh will be used. Either of class SpatialPolygons or two columns with the coorinates of the polygon
#'
#' @return An INLA stack onto which new data can be projected
#'
#' @export
#' @import INLA

CreateProjectionGrid  <- function(nxy, mesh, data, tag='pred', coordnames = c("x", "y"), boundary=NULL) {
  if("Y"%in%coordnames) stop("Y cannot be a coordinate name")
  if("e"%in%coordnames) stop("e cannot be a coordinate name")
  if(is.null(boundary)) boundary <- mesh$loc[mesh$segm$int$idx[,2],]
  if(class(boundary)=="SpatialPolygons") {
    projgrid <- inla.mesh.projector(mesh, xlim=boundary@bbox["x",], ylim=boundary@bbox["y",], dims=nxy)
    # get the points on the grid within the boundary
    xy.in <- !is.na(over(SpatialPoints(projgrid$lattice$loc, proj4string=boundary@proj4string), boundary))
  } else {
    if(ncol(boundary)<2) stop("Boundary should have at least 2 columns")
    projgrid <- inla.mesh.projector(mesh, xlim=range(boundary[,1]), ylim=range(boundary[,2]), dims=nxy)
    xy.in <- splancs::inout(projgrid$lattice$loc, boundary)
  }

  # get the points on the grid within the boundary
  #  xy.in <- splancs::inout(projgrid$lattice$loc, boundary[,1:2])
  predcoords <- projgrid$lattice$loc[which(xy.in),]
  colnames(predcoords) <- coordnames
  Apred <- projgrid$proj$A[which(xy.in), ]

  # Extract covariates for points
  NearestCovs=GetNearestCovariate(points=predcoords, covs=data)
  NearestCovs$Intercept=1

  # stack the predicted data
  stk <- inla.stack(list(Y=cbind(NA, rep(NA, nrow(NearestCovs))), e=rep(0, nrow(NearestCovs))),
                    A=list(1,Apred), tag=tag, effects=list(NearestCovs@data, list(i=1:mesh$n)))
  pred=list(stk=stk, xy.in=xy.in, predcoords=predcoords)
  pred
}

