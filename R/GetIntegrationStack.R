#' Function to create stack for integration points
#'
#' @param mesh INLA mesh.
#' @param data Data frame with columns X and Y, and others covariates.
#' @param area Area around each integration point.
#' @param tag Tag to name stack.
#' @param coordnames Names for coordinates in data.
#' @param InclCoords Boolean, should coordinates be included in data (defaults to FALSE).
#' @return An INLA stack for integration
#'
#' @export
#' @import INLA
GetIntegrationStack=function(mesh, data, area, tag='mesh', coordnames=c("X","Y"), InclCoords=FALSE) {
  if(!all(coordnames%in%names(data))) stop("Coordinates not in the data")
  if(InclCoords) {
    Names  <- names(data)
  }  else  {
    Names  <- names(data)[!names(data)%in%coordnames]
  }
  NearestCovs=GetNearestCovariate(points=cbind(c(mesh$loc[,1]),
                                               c(mesh$loc[,2])),
                                  covs=data[,c(coordnames, names(data)[!names(data)%in%coordnames])])
  names(NearestCovs) <- names(data[,c(coordnames, names(data)[!names(data)%in%coordnames])])
  NearestCovs$Intercept <- rep(1,nrow(NearestCovs))

  # Projector matrix for integration points.
  projmat.ip <- Matrix::Diagonal(mesh$n, rep(1, mesh$n))  # from mesh to integration points

  stk.ip <- inla.stack(data=list(y=cbind(rep(0,mesh$n), NA), e=area), A=list(1,projmat.ip), tag=tag,
                       effects=list(NearestCovs[,c(Names, "Intercept")], list(i=1:mesh$n)))
  stk.ip
}
