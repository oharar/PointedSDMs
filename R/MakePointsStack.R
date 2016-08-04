#' Function to create stack for presence only points
#'
#' @param data SpatialPointsDataFrame of covariates.
#' @param presences SpatialPoints object of presences.
#' @param tag Name for tag for the stack (defaults to "points").
#' @param intercept Boolean, should an intercept be added? Defaults to TRUE.
#' @param mesh INLA mesh.
#' @param coordnames Names of coorninates
#' @param InclCoords Boolean, shoiuld coordinates be included in data (defaults to FALSE).
#'
#' @return An INLA stack with points
#'
#' @export
#' @import INLA

MakePointsStack=function(data, presences, tag="points", intercept=TRUE, mesh,
                         coordnames=NULL, InclCoords=FALSE) {
  if(is.null(coordnames)) coordnames <- colnames(data@coords)
  if(InclCoords) {
    data@data[,coordnames] <- data@coords
  }
  NearestCovs=GetNearestCovariate(points=presences, covs=data)

  # Projector matrix from mesh to data.
  NearestCovs@data[,paste("int",tag,sep=".")] <- 1
  projmat <- inla.spde.make.A(mesh, as.matrix(NearestCovs@coords)) # from mesh to point observations

  stk.pp <- inla.stack(data=list(y=cbind(rep(1,nrow(NearestCovs)), NA),
                                 e=rep(0, nrow(NearestCovs))), A=list(1,projmat), tag=tag,
                       effects=list(NearestCovs@data, list(i=1:mesh$n)))
  stk.pp
}

