#'  Function to set up spatial structure for region
#'
#' @param data Data (as data frame, with points having locations, or as a SpatialPointsDataFrame). Can be NULL, if bdry is not NULL.
#' @param coords Names of columns for coordinates (X & Y) in data. Ignored if data is NULL.
#' @param meshpars List of parameters to be sent to inla.mesh.2d().
#' @param bdry Polygon of boundary for region, of class Polygon. If NULL, draws a boundary around the points.
#' @return A list with 3 elements:
#'     . mesh: mesh (from inla.mesh.2d)
#'     . spde: spde object for Matern model (from inla.spde2.matern)
#'     . w: weights for each point in mesh
#'
#' @export
#' @import sp
#' @import INLA
MakeSpatialRegion=function(data=NULL, coords=c("X","Y"), meshpars, bdry=NULL) {
  # data=Data; coords=c("Xorig", "Yorig"); meshpars=list(cutoff=0.5, max.edge=c(1, 3), offset=c(1,1))
  if(is.null(bdry) & is.null(data)) stop("Either data or a boundary has to be supplied")
  if(is.null(bdry)) {
    if(!class(data)=="SpatialPointsDataFrame") {
      dat.spat <- SpatialPointsDataFrame(data[,coords], data=data,
                                         proj4string = CRS("+proj=longlat +datum=WGS84"))
    }
    bstart <- min(c(diff(sort(unique(dat.spat@coords[,1]))), diff(sort(dat.spat@coords[,2]))))
    poly.tmp <- rgeos::gBuffer(dat.spat, width=bstart, byid=TRUE)
    bdry <-  rgeos::gBuffer(rgeos::gUnaryUnion(poly.tmp), width=bstart)
    #    bdry <-  gBuffer(gBuffer(gUnaryUnion(poly.tmp), width=bstart), width=0)
  }
  bdry <- rgeos::gBuffer(SpatialPolygons(Srl=list(Polygons(srl=list(bdry), ID="eek"))), width=0)

  region.bdry <- inla.sp2segment(bdry)

  # Create mesh and spde object
  mesh <- inla.mesh.2d(boundary=region.bdry, cutoff=meshpars$cutoff, max.edge=meshpars$max.edge, offset=meshpars$offset)
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
