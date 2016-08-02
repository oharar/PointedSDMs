#' Function to create stack for presence only points
#'
#' @param data Data frame with columns X and Y, and others are covariates.
#' @param presences Data frame with columns X and Y for coordinates of points.
#' @param tag Name for tag for the stack (defaults to "points").
#' @param intercept Boolean, should an intercept be added? Defaults to TRUE.
#' @param mesh INLA mesh.
#' @param coordnames Names for columns with coordinates in data.
#' @param InclCoords Boolean, shoiuld coordinates be included in data (defaults to FALSE).
#'
#' @return An INLA stack with points
#'
#' @export
#' @import INLA

MakePointsStack=function(data, presences, tag="points", intercept=TRUE, mesh,
                        coordnames=c("X","Y"), InclCoords=FALSE) {
  if(!all(coordnames%in%names(data))) stop("Coordinates not in the data")
  NearestCovs=GetNearestCovariate(points=presences[,coordnames], covs=data)

  # Projector matrix from mesh to data.
  projmat <- inla.spde.make.A(mesh, as.matrix(presences)) # from mesh to point observations
  if(InclCoords) {
    CovEffects <- NearestCovs
  }  else  {
    CovEffects <- NearestCovs[,!names(NearestCovs)%in%coordnames]
  }

  if(intercept)  CovEffects[,paste("int",tag,sep=".")]=rep(1,nrow(NearestCovs))

  stk.pp <- inla.stack(data=list(y=cbind(rep(1,nrow(NearestCovs)), NA),
                                 e=rep(0, nrow(NearestCovs))), A=list(1,projmat), tag=tag,
                       effects=list(CovEffects, list(i=1:mesh$n)))
  stk.pp
}

