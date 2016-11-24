#' Function to create stack for presence only points
#'
#' @param data \code{SpatialPointsDataFrame} of covariates.
#' @param presences \code{SpatialPoints} object of presences.
#' @param tag Name for this stack (defaults to "points").
#' @param intercept should an intercept be added? Defaults to \code{TRUE}.
#' @param mesh INLA mesh.
#' @param coordnames Names of coordinates.
#' @param InclCoords should coordinates be included in data? (defaults to \code{FALSE})
#' @param polynoms If not \code{NULL}, a \code{SpatialPolygons} object, with (for example) range maps.
#' @param scale Should the distance be scaled by dividing by the mean of the non-zero distances? Defaults to \code{FALSE}, either logical or numeric. Ignored if \code{polynoms} is \code{NULL}.
#'
#' @return An INLA stack with points
#'
#' @export
#' @import INLA
MakePointsStack <- function(data, presences, tag="points", intercept=TRUE, mesh,
                         coordnames=NULL, InclCoords=FALSE, polynoms = NULL,
                         scale = FALSE) {
  if(is.null(coordnames)) coordnames <- colnames(data@coords)
  if(!is.null(polynoms)) {
    if(class(polynoms) != "SpatialPolygonsDataFrame" & class(polynoms) != "SpatialPolygons")
      stop("polynoms should be a spatial polygon")
  }

  NearestCovs <- GetNearestCovariate(points=presences, covs=data)
  if(InclCoords) {    data@data[,coordnames] <- data@coords  }
  if(intercept) NearestCovs@data[,paste("int",tag,sep=".")] <- 1 # add intercept
  if(!is.null(polynoms)) {
    NearestCovs <- AddDistToRangeToSpatialPoints(data = NearestCovs, polynoms = polynoms, scale=scale)
  }

  # Projector matrix from mesh to data.
  projmat <- inla.spde.make.A(mesh, as.matrix(NearestCovs@coords)) # from mesh to point observations

  stk.pp <- inla.stack(data=list(resp=cbind(rep(1,nrow(NearestCovs)), NA),
                                 e=rep(0, nrow(NearestCovs))),
                       A=list(1,projmat), tag=tag,
                       effects=list(NearestCovs@data, list(i=1:mesh$n)))

  return(stk.pp)
}
