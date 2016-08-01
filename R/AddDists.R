#' Function to add distance to a list of polygons (e.g. range map) to a data frame
#'
#' @param polys List of polygons (a spatialpolygonsdataframe will work).
#' @param covs Data frame of covariates.
#' @param scale Scale argument to be passed to AddDistToDataFrame.
#'
#' @return covs data frame plus columns with distances to each polygon
AddDists <- function(polys, covs, scale=FALSE) {
  if("package:parallel" %in% search()) {
    dists <- parallel::mclapply(polys, function(pcs, IP) {
      df <- AddDistToRangeToDataFrame(df=IP, coords=c("long", "lat"), polys=pcs, name="Name", scale=scale)
      df[,"Name"]
    }, IP=covs)
  } else {
    dists <- lapply(polys, function(pcs, IP) {
      df <- AddDistToRangeToDataFrame(df=IP, coords=c("long", "lat"), polys=pcs, name="Name", scale=scale)
      df[,"Name"]
    }, IP=covs)
  }
  names(dists) <- gsub('[_-]', "", names(dists))   # give correct names
  cbind(covs, dists)
}


