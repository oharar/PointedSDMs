#' Function to add distance to a list of polygons (e.g. range map) to a points stack
#'
#' @param stk Stack for points.
#' @param polys List of polygons (a spatialpolygonsdataframe will work).
#' @param mesh A mesh.
#' @param scale Scale argument to be passed to AddDistToDataFrame.
#' @param ... Additional variables to pass to GetPointsStack(), e.g. tag.
#'
#' @return An INLA stack with distances to polygons added
#'
#' @export
AddPolyToPointsStack <- function(stk, polys, mesh,scale=FALSE,...) {
  Covs <- stk$effects$data[stk$data$index[[1]],names(stk$effects$data)!="Intercept"]
  if("package:parallel" %in% search()) {
    dists <- parallel::mclapply(polys, function(pcs, covs) {
      df <- AddDistToRangeToDataFrame(df=covs, coords=c("long", "lat"), polys=pcs, name="Name", scale=scale)
      df[,"Name"]
    }, covs=Covs)
  } else {
    dists <- lapply(polys, function(pcs, covs) {
      df <- AddDistToRangeToDataFrame(df=covs, coords=c("long", "lat"), polys=pcs, name="Name", scale=scale)
      df[,"Name"]
    }, covs=Covs)
  }
  names(dists) <- gsub('[_-]', "", names(dists))
  Covs <- cbind(Covs, dists)

  stk=GetPointsStack(presences=Covs[,c("long","lat")], coordnames=c("long","lat"), data=Covs, mesh=mesh,...)
  #    stk=GetPointsStack(mesh=mesh$mesh, data=Covs, ...)
  stk
}
