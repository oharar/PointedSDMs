#' Function to create stack for integration points
#'
#' @param mesh INLA mesh.
#' @param data Data frame with columns X and Y, and others covariates, or a SpatialPointsDataFrame object.
#' @param area Area around each integration point.
#' @param tag Tag to name stack.
#' @param coordnames Names for coordinates in data (if data is nota spatial object).
#' @param InclCoords Boolean, should coordinates be included in data (defaults to FALSE).
#' @return An INLA stack for integration
#'
#' @export
#' @import INLA
GetIntegrationStack=function(mesh, data, area, tag='mesh', coordnames=c("X","Y"), InclCoords=FALSE) {
  if(class(data)!="SpatialPointsDataFrame" & !all(coordnames%in%names(data))) stop("Coordinates not in the data")

  if(class(data)=="SpatialPointsDataFrame") {
    coordnames <- dimnames(data@coords)[[2]]
    Names <- dimnames(data@data)[[2]]
  } else {
    Names  <- names(data)[!names(data)%in%coordnames]
  }
  if(InclCoords) Names  <- c(coordnames, Names)

  NearestCovs=GetNearestCovariate(points=cbind(c(mesh$loc[,1]),
                                               c(mesh$loc[,2])),
                                  covs=data)
  names(NearestCovs) <- names(data@data)
  NearestCovs$Intercept <- rep(1,nrow(NearestCovs))

  # Projector matrix for integration points.
  projmat.ip <- Matrix::Diagonal(mesh$n, rep(1, mesh$n))  # from mesh to integration points

  stk.ip <- inla.stack(data=list(y=cbind(rep(0,mesh$n), NA), e=area), A=list(1,projmat.ip), tag=tag,
                       effects=list(NearestCovs[,c(Names, "Intercept")], list(i=1:mesh$n)))
  stk.ip
}
