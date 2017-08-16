#'  Fit a model with INLA
#'
#' @param ... Data stacks.
#' @param formula If not \code{NULL} (the default), the formula to be used in the model fitting.
#' Does not have to contain the spatial term if \code{spat.ind} is specified.
#' @param CovNames Names of covariates to use. If \code{NULL} will use all in stack effects.
#' @param mesh INLA mesh.
#' @param spat.ind Index for spatial mesh. Defaults to \code{i} (which is used elsewhere n this package).
#' Set to \code{NULL} if no spatial term is wanted.
#' @param predictions Boolean: should predictions (on the linear scale) be made? Defaults to \code{FALSE}.
#' @param tag.pred Name of tag for predictions. Defaults to "pred".
#' @param control.fixed List of arguments to be passed to INLA via control.fixed (e.g. parameters of priors of fixed effects). Default is \code{NULL}.
#' @param waic Should wAIC be calculated? Defaults to \code{FALSE}.
#' @param dic Should DIC be calculated? Defaults to \code{FALSE}.
#' @return If predictions is \code{TRUE}, a list with
#'     - model: an object of class INLA, with the results of the analysis in them.
#'     . predictions: Posterior means and standard deviationsfor linear predictor for predictions. Note that
#'     If predictions is FALSE, just the model.
#'
#' @export
#' @import stats
#' @import INLA
FitModel <- function(..., formula=NULL, CovNames=NULL, mesh, spat.ind = "i", predictions=FALSE, tag.pred="pred",
                     control.fixed=NULL, waic=FALSE, dic=FALSE) {

  stck <- inla.stack(...)

  if(is.null(CovNames)) {
    CovNames <- unlist(stck$effects$names)
    CovNames <- CovNames[!CovNames%in%c(spat.ind)]
  } else {
    if(!is.null(formula)) {
      warning("CovNames and formula are both not NULL: CovNames will be ignored")
    }
  }

  mesh <- inla.spde2.matern(mesh)

  if(!is.null(spat.ind)) {
    CovNames <- c(CovNames, paste0("f(", spat.ind, ", model=mesh)"))
  }
# I'm sure there's a nicer way of doing this, but ... won't work
  if(is.null(control.fixed)) {
    control.fixed <- list(mean=0)
  }

  if(is.null(formula)) {
    Formula <- formula(paste(c("resp ~ 0 ", CovNames), collapse="+"))
  } else {
    if(is.null(spat.ind)) {
      Formula <- formula
    } else {
      if(any(grepl(paste0('(', spat.ind, ','), formula, fixed=TRUE))) {
        warning(paste0(spat.ind, " already in formula, so will be ignored"))
      }
      Formula <- update(formula, paste0(" ~ . + f(", spat.ind, ", model=mesh)"))    }
  }

  # Fit model including predictions
  mod <- inla(Formula, family=c('poisson','binomial'),
              control.family = list(list(link = "log"), list(link = "cloglog")),
              data=inla.stack.data(stck), verbose=FALSE,
              control.results=list(return.marginals.random=FALSE,
                                   return.marginals.predictor=FALSE),
              control.predictor=list(A=inla.stack.A(stck), link=NULL, compute=TRUE),
              control.fixed=control.fixed,
              Ntrials=inla.stack.data(stck)$Ntrials, E=inla.stack.data(stck)$e,
              control.compute=list(waic=waic, dic=dic))

  if(predictions) {
    id <- inla.stack.index(stck,tag.pred)$data
    pred <- data.frame(mean=mod$summary.fitted.values$mean[id],
                       stddev=mod$summary.fitted.values$sd[id])
    res <- list(model=mod, predictions=pred)
  } else {
    res <- mod
  }
  res
}
