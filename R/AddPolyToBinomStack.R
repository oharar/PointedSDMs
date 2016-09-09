#' Function to add distance to a list of polygons (e.g. range map) to a binomial stack
#'
#' @param stk Stack for points.
#' @param polys List of polygons (a spatialpolygonsdataframe will work).
#' @param mesh A mesh.
#' @param scale Scale argument to be passed to AddDistToDataFrame().
#' @param ... Additional variables to pass to MakeBinomStack(), e.g. tag.
#' @return A stack.
#'
#' @export
AddPolyToBinomStack <- function(stk, polys, mesh, scale=FALSE, ...) {
  Covs.binom <- stk$effects$data[stk$data$index[[1]],names(stk$effects$data)!="Intercept"]
  dists <- parallel::mclapply(polys, function(pcs, IP) {
    df <- AddDistToRangeToDataFrame(df=IP, coords=c("long", "lat"),
                                    polynoms = pcs, name="Name", scale=scale)
    df[,"Name"]
  }, IP=Covs.binom)
  names(dists) <- gsub('[_-]', "", names(dists))   # give correct names
  Covs.binom <- cbind(Covs.binom, dists)

  pres.binom <- data.frame(long=stk$effects$data$long[stk$data$index$bbs],
                           lat=stk$effects$data$lat[stk$data$index$bbs],
                           NPres=stk$data$data$y.2, Ntrials=stk$data$data$Ntrials)
  stk <- MakeBinomStack(data=Covs.binom, observs=pres.binom, intercept=TRUE,
                       mesh=mesh, coordnames=c("long","lat"), presname=c("NPres"),
                       trialname="Ntrials", ...)
  stk
}
