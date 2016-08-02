#' Function to get covariate values nearest the data/integration point
# inputs:
#' @param points Points for which we need covariates.
#' @param covs Covariates, with coordinates in first and second columns, or a SpatialPointsDataFrame object
#' @return A matrix with the original points plus two columns with coorinates of nearest point.
#' If there are ties, this will use the first element.
#' NOTE to self:  should probably pass coordinates in covs as names
#'
#' @export
GetNearestCovariate=function(points, covs) {
  if(class(covs)=="SpatialPointsDataFrame") {
    coordnames <- dimnames(covs@coords)[[2]]
    Names <- dimnames(covs@data)[[2]]
    covs <- as.data.frame(cbind(covs@coords, covs@data))
    names(covs) <- c(coordnames, Names)
  }
  Nearest=apply(points,1, function(coord, Covs) {
    distsq = (coord[1]-Covs[,1])^2 + (coord[2]-Covs[,2])^2
    which(distsq==min(distsq, na.rm=TRUE))[1]
  }, Covs=covs)
  cbind(points, covs[Nearest,-c(1,2)])
}
