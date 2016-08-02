#' Function to create stack for presence only points
#'
#' @param data Data frame with columns X and Y, and others are covariates.
#' @param pres Data frame with columns X and Y for coordinates of points, and number of occurrances & number of trials.
#' @param tag Name for tag for the stack (defaults to "abund").
#' @param intercept Boolean, should an intercept be added? Defaults to TRUE.
#' @param mesh INLA mesh.
#' @param coordnames Names for columns with coordinates in data.
#' @param presname Name of column with presences.
#' @param trialname Name of column with total number of visits.
#' @param InclCoords Boolean, should coordinates be included in data (defaults to FALSE).
#'
#' @return An INLA stack with binomial data: include Ntrials, which is the number of trials
#'
#' @export
#' @import INLA

MakeBinomStack=function(data, pres, tag="abund", intercept=TRUE, mesh, coordnames=c("X","Y"),
                       presname="NPres", trialname="Ntrials", InclCoords=FALSE) {
  if(!all(coordnames%in%names(data))) stop("Coordinates not in the data")
  if(!all(coordnames%in%names(pres))) stop("Coordinates not in the presence data")
  if(length(presname)>1) stop("more than one name given for presences column")
  if(length(trialname)>1) stop("more than one name given for number of trials column")

  if(!presname%in%names(pres)) stop(paste(presname," not in names of presences data frame", sep=""))
  if(!trialname%in%names(pres)) stop(paste(trialname," not in names of presences data frame", sep=""))

  NearestCovs=GetNearestCovariate(points=pres[,coordnames], covs=data)
  names(NearestCovs) <- c(coordnames, names(data)[!names(data)%in%coordnames])

  # Projector matrix from mesh to data.
  projmat <- inla.spde.make.A(mesh, as.matrix(pres[,coordnames]))

  CovEffects=NearestCovs[,!names(NearestCovs)%in%coordnames]
  if(InclCoords) {
    CovEffects <- NearestCovs
  }  else  {
    CovEffects <- NearestCovs[,!names(NearestCovs)%in%coordnames]
  }
  if(intercept)  CovEffects[,paste("int",tag,sep=".")]=rep(1,nrow(NearestCovs))

  stk.binom <- inla.stack(data=list(y=cbind(NA,pres[,presname] ), Ntrials=pres[,trialname]), A=list(1,projmat), tag=tag,
                          effects=list(CovEffects, list(i=1:mesh$n)))
  stk.binom
}
