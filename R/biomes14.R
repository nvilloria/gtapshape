#' World Wildlife Fund (WWF) Subnational Regions
#'
#' A spatial dataset containing 14 subnational regions defined by the World Wildlife Fund (WWF).
#'
#' @format A simple feature collection with 14 features and 2 fields:
#' \describe{
#'   \item{subnat_name}{character. The name of the subnational region.}
#'   \item{subnat_num}{numeric. The numeric identifier for the subnational region.}
#'   \item{geometry}{sfc_MULTIPOLYGON. The geometric representation of the region.}
#' }
#' @details
#' This dataset is a simple feature collection with the following properties:
#' \itemize{
#'   \item Geometry type: MULTIPOLYGON
#'   \item Dimension: XY
#'   \item Bounding box: xmin: -180, ymin: -87.26008, xmax: 180, ymax: 83.62313
#'   \item Geodetic CRS: WGS 84
#' }
#' @source World Wildlife Fund (WWF)
#' @examples
#' data(wwf_subnational)
#' plot(st_geometry(wwf_subnational))
#' summary(wwf_subnational)
"biomes14"
