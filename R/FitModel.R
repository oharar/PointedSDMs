#'  Fit a model with INLA
#'
#' @param ... Data stacks.
#' @param CovNames Names of covariates to use. If NULL will use all in stack effects.
#' @param mesh INLA mesh.
#' @param spat.ind Intext for spatial mesh. Defaults to i.
#' @param predictions Boolean: should predictions be made? Defaults to FALSE
#' @param tag.pred Name of tag for predictions. Defaults to "pred".
#' @return If predictions is TRUE, a list with
#'     - model: an object of class INLA, with the results of the analysis in them.
#'     . predictions: Posterior means and standard deviationsfor linear predictor for predictions. Note that
#'     If predictions is FALSE, just the model.
#'
#' @export
#' @import INLA
FitModel <- function(..., CovNames=NULL, mesh, spat.ind = "i", predictions=FALSE, tag.pred="pred") {
  stck <- inla.stack(...)
  if(is.null(CovNames)) {
    CovNames <- unlist(stck$effects$names)
    CovNames <- CovNames[!CovNames%in%c(spat.ind)]
  }
  mesh <- inla.spde2.matern(mesh)
  Formula <- formula(paste(c("y ~ 0 ", CovNames, paste0("f(", spat.ind, ", model=mesh)")), collapse="+"))

  # Fit model including predictions
  mod <- inla(Formula, family=c('poisson','binomial'),
              control.family = list(list(link = "log"), list(link = "cloglog")),
              data=inla.stack.data(stck), verbose=FALSE,
              control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE),
              control.predictor=list(A=inla.stack.A(stck), compute=TRUE),
              Ntrials=inla.stack.data(stck)$Ntrials, E=inla.stack.data(stck)$e)

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
