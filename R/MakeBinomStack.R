#' Function to create stack for presence only points
#'
#' @param data SpatialPointsDataFrame of covariates.
#' @param observs SpatialPoints object of presences and trials.
#' @param tag Name for tag for the stack (defaults to "points").
#' @param intercept Boolean, should an intercept be added? Defaults to TRUE.
#' @param mesh INLA mesh.
#' @param presname Names of persences column in observs. Defaults to "NPres".
#' @param trialname Names of column of number of columns in observs. Defaults to "Ntrials".
#' @param coordnames Names of coordinates.
#' @param InclCoords Boolean, shoiuld coordinates be included in data (defaults to FALSE).
#' @param polynoms If not NULL, a SpatialPolygons object, with (for example) range maps.
#' @param scale Should the distance be scaled by dividing by the mean of the non-zero distances?
#' Defaults to FALSE, either logical or numeric. Ignored if polynoms is NULL.
#'
#' @return An INLA stack with binomial data: include Ntrials, which is the number of trials
#'
#' @export
#' @import INLA

MakeBinomStack=function(data, observs, tag="points", intercept=TRUE, mesh, presname="NPres", trialname="Ntrials",
                        coordnames=NULL, InclCoords=FALSE, polynoms = NULL, scale = FALSE) {

  if(length(presname)>1) stop("more than one name given for presences column")
  if(length(trialname)>1) stop("more than one name given for number of trials column")
  if(!presname%in%names(observs@data)) stop(paste(presname," not in names of presences data frame", sep=""))
  if(!trialname%in%names(observs@data)) stop(paste(trialname," not in names of presences data frame", sep=""))

  if(is.null(coordnames)) coordnames <- colnames(data@coords)
  if(!is.null(polynoms)) {
    if(class(polynoms) != "SpatialPolygonsDataFrame" & class(polynoms) != "SpatialPolygons")
      stop("polynoms should be a spatial polygon")
  }

  NearestCovs <- GetNearestCovariate(points=observs, covs=data)
  if(InclCoords) {    data@data[,coordnames] <- data@coords  }
  if(intercept) NearestCovs@data[,paste("int",tag,sep=".")] <- 1 # add intercept
  if(!is.null(polynoms)) {
    NearestCovs <- AddDistToRangeToSpatialPoints(data = NearestCovs, polys = polynoms, scale=scale)
  }

  # Projector matrix from mesh to data.
  projmat <- inla.spde.make.A(mesh, as.matrix(NearestCovs@coords)) # from mesh to point observations

  stk.binom <- inla.stack(data=list(y=cbind(NA,observs@data[,presname] ), Ntrials=observs@data[,trialname]), A=list(1,projmat), tag=tag,
                          effects=list(NearestCovs@data, list(i=1:mesh$n)))

  stk.binom
}
