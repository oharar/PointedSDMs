#'  Function to set up spatial structure for region
#'
#' @param data Data (as data frame, with points having locations, or as a SpatialPointsDataFrame). Can be NULL, if bdry is not NULL.
#' @param coords Names of columns for coordinates (X & Y) in data. Ignored if data is NULL.
#' @param meshpars List of parameters to be sent to inla.mesh.2d().
#' @param bdry Polygon of boundary for region, of class Polygon. If \code{NULL}, draws a boundary around the points.
#' @param proj Projection to use if data is not a projection. Defaults to utm (hopefully).
#' @return A list with 3 elements:
#'     . mesh: mesh (from inla.mesh.2d)
#'     . spde: spde object for Matern model (from inla.spde2.matern)
#'     . w: weights for each point in mesh
#'
# DLM notes, need to cleanup and include
# @section Mesh parameters:
# The \code{meshpars} argument allows us to set options for mesh creation. Changing these options can have a big impact on the results of the fitted model. \code{cutoff} sets how faithfully the boundary is approximated (setting to 0 gives exactly the boundary supplied). \code{max.edge} is the maximum triangle edge (side) length inside and (optionally) outside the boundary, these are specified in the units of the coordinate system. \code{cutoff} avoids the many triangles being build around locations (this also acts to simplify the boundary), higher values lead to a simpler mesh (which is generated faster).
#
#' @export
#' @import methods
#' @import sp
#' @import INLA
MakeSpatialRegion <- function(data=NULL, coords=c("X","Y"), meshpars, bdry=NULL, proj = CRS("+proj=utm")) {
  # data=Data; coords=c("Xorig", "Yorig"); meshpars=list(cutoff=0.5, max.edge=c(1, 3), offset=c(1,1))
  if(is.null(bdry) & is.null(data)) stop("Either data or a boundary has to be supplied")
  if(is.null(bdry)) {
    if(!class(data)=="SpatialPointsDataFrame") {
      dat.spat <- SpatialPointsDataFrame(data[,coords], data=data, proj4string = proj)
    } else {
      if(!is.projected(data)) data <- spTransform(data, CRSobj = proj)
    }
    bstart <- min(c(diff(sort(unique(dat.spat@coords[,1]))), diff(sort(dat.spat@coords[,2]))))
    poly.tmp <- rgeos::gBuffer(dat.spat, width=bstart, byid=TRUE)
    bdry <-  rgeos::gBuffer(rgeos::gUnaryUnion(poly.tmp), width=bstart)
    bdry <- spTransform(bdry, CRSobj = proj)
    #    bdry <-  gBuffer(gBuffer(gUnaryUnion(poly.tmp), width=bstart), width=0)
  } else {
    if(class(bdry)!="SpatialPolygons") {
      bdry <- SpatialPolygons(Srl=list(Polygons(srl=list(bdry), ID="eek")))
    } else {
      if(!is.projected(bdry)) bdry <- spTransform(bdry, CRSobj = proj)
    }
  }

  bdry <- rgeos::gBuffer(bdry, width=0)
  region.bdry <- inla.sp2segment(bdry)

  # Create mesh and spde object
  mesh <- inla.mesh.2d(boundary=region.bdry, cutoff=meshpars$cutoff,
                       max.edge=meshpars$max.edge, offset=meshpars$offset)
  # region.mesh <- inla.mesh.2d(boundary=region.bdry, cutoff=0.1, max.edge=c(0.1, 3), offset=c(1,1))
  spde <- inla.spde2.matern(mesh=mesh, alpha=2) # create spde object
  #  theta <- c(-log(4*pi*s2x*kappa^2), log(kappa))

  # Get areas for Voronoi tiles around each integration point
  dd <- deldir::deldir(mesh$loc[,1], mesh$loc[,2]) #, suppressMsge=TRUE)
  tiles <- deldir::tile.list(dd)
  #  poly.gpc <- as(bdry@coords,'gpc.poly')
  poly.gpc <- as(bdry@polygons[[1]]@Polygons[[1]]@coords,'gpc.poly')
  w <- sapply(tiles, function(p) rgeos::area.poly(rgeos::intersect(as(cbind(p$x, p$y), 'gpc.poly'), poly.gpc)))
  return(list(mesh=mesh, spde=spde, w=w))
}
