#' Covariate information for teh region around therange of the solitary tinomou
#'
#' @format A data frame with the coordinates of centroids of national parks plus
#' the area and whether the Solitary Tinomou was recorded as present:
#' \describe{
#' \item{\code{X}}{Longitude}
#' \item{\code{Y}}{Latitude}
#' \item{\code{Forest}}{Percentage of area covered by forest}
#' \item{\code{NPP}}{Net Primary Productivity}
#' \item{\code{Altitude}}{Altitude}
#' }
#'
"SolTin_covariates"

#' Recorded Observations of the Solitary Tinomou, from eBird
#'
#' @format A data frame with the coordinates of observations as two variables:
#' \describe{
#' \item{\code{X}}{Longitude}
#' \item{\code{Y}}{Latitude}
#' }
#'
#' @source \url{http://ebird.org/}
"SolTin_ebird"

#' Recorded Observations of the Solitary Tinomou, from GBIF
#'
#' @format A data frame with the coordinates of observations as two variables:
#' \describe{
#' \item{\code{X}}{Longitude}
#' \item{\code{Y}}{Latitude}
#' }
#'
#' @source \url{http://www.gbif.org/}
"SolTin_gbif"

#' Recorded Observations of the Solitary Tinomou, from parks
#'
#' @format A data frame with the coordinates of centroids of national parks plus
#' the area and whether the Solitary Tinomou was recorded as present:
#' \describe{
#' \item{\code{X}}{Longitude}
#' \item{\code{Y}}{Latitude}
#' \item{\code{area}}{Area of the park, }
#' \item{\code{Present}}{Logical, whether the species is recorded as present in the park.}
#' }
#'
#' @source \url{http://www.gbif.org/}
"SolTin_parks"

#' Points of polygon for region of study for solitary timanou
#'
#' @format A data frame with the coordinates of vertives of the polygon of the region
#' where the covariates are recorded for the Solitary Tinomou.
#' \describe{
#' \item{\code{X}}{Longitude}
#' \item{\code{Y}}{Latitude}
#' }
#'
"SolTin_polygon"

#' Points of polygon for solitary timanou range map
#'
#' @format A data frame with the coordinates of vertives of the polygon of the expert
#' range map for the Solitary Tinomou.
#' \describe{
#' \item{\code{X}}{Longitude}
#' \item{\code{Y}}{Latitude}
#' }
#'
"SolTin_range"

