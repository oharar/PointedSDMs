#' Function to add distance to a list of polygons (e.g. range map) to a data frame
#'
#' @param polynoms List of polygons (a spatialpolygonsdataframe will work).
#' @param covs Data frame of covariates.
#' @param scale Scale argument to be passed to AddDistToDataFrame.
#'
#' @return covs data frame plus columns with distances to each polygon
AddDists <- function(polynoms, covs, scale=FALSE) {
  if("package:parallel" %in% search()) {
    dists <- parallel::mclapply(polynoms, function(pcs, IP) {
      df <- AddDistToRangeToDataFrame(df=IP, coords=c("long", "lat"), polynoms=pcs, name="Name", scale=scale)
      df[,"Name"]
    }, IP=covs)
  } else {
    dists <- lapply(polynoms, function(pcs, IP) {
      df <- AddDistToRangeToDataFrame(df=IP, coords=c("long", "lat"), polynoms=pcs, name="Name", scale=scale)
      df[,"Name"]
    }, IP=covs)
  }
  names(dists) <- gsub('[_-]', "", names(dists))   # give correct names
  cbind(covs, dists)
}


