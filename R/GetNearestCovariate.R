#' Function to get covariate values nearest the data/integration point
# inputs:
#' @param points Points for which we need covariates. Either a SpatialPoints* object or a matrix with coordinates in columns.
#' @param covs Covariates, with coordinates in first and second columns, or a SpatialPointsDataFrame object.
#' @return A SpatialPointsDataFrame with the original points plus the data from the closest point.
#' If there are ties, this will use the first element.
#'
#' @export
GetNearestCovariate <- function(points, covs) {
  if(class(covs)!="SpatialPointsDataFrame") {
    covs <- SpatialPointsDataFrame(coords = covs[,1:2], data = covs[,-(1:2)])
  }
  if(class(points)!="SpatialPointsDataFrame") {
    points <- SpatialPoints(points)
  }

  Nearest <- apply(points@coords, 1, function(pt, pts) {
    dists <- spDistsN1(pts, pt, longlat = TRUE)
    which(dists==min(dists))
  }, pts=covs)
  points.df <- SpatialPointsDataFrame(coords=points@coords, data = covs@data[Nearest,])
  points.df
}
