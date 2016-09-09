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
MakeIntegrationStack=function(mesh, data, area, tag='mesh', coordnames=c("X","Y"), InclCoords=FALSE) {
  if(class(data)!="SpatialPointsDataFrame" & !all(coordnames%in%names(data))) stop("Coordinates not in the data")

  if(class(data)=="SpatialPointsDataFrame") {
    coordnames <- colnames(data@coords)
    Names <- colnames(data@data)
  } else {
    Names  <- names(data)[!names(data)%in%coordnames]
  }
  if(InclCoords) Names  <- c(coordnames, Names)

  Points <- cbind(c(mesh$loc[,1]), c(mesh$loc[,2]))
  colnames(Points) <- coordnames
  NearestCovs <- GetNearestCovariate(points=Points, covs=data)
  NearestCovs$Intercept <- rep(1,nrow(NearestCovs))
  if(InclCoords) {
    NearestCovs@data[,colnames(NearestCovs@coords)] <- NearestCovs@coords
  }

  # Projector matrix for integration points.
  projmat.ip <- Matrix::Diagonal(mesh$n, rep(1, mesh$n))  # from mesh to integration points

  stk.ip <- inla.stack(data=list(resp=cbind(rep(0,mesh$n), NA), e=area), A=list(1,projmat.ip), tag=tag,
                       effects=list(NearestCovs@data, list(i=1:mesh$n)))
  stk.ip
}
