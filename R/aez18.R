#' Agro-Ecological Zones (AEZ18) Dataset
#'
#' A spatial dataset containing 18 Agro-Ecological Zones (AEZ) defined
#' by the Global Agro-Ecological Zones (GAEZ) methodology--[[Micah add
#' more or less explanations if you see fit---and perhaps mention vignette]].
#'
#' @format A simple feature collection with 18 features and 2 fields:
#' \describe{
#'   \item{subnat_name}{character. The name of the Agro-Ecological Zone.}
#'   \item{subnat_num}{numeric. The numeric identifier for the Agro-Ecological Zone.}
#'   \item{geometry}{sfc_MULTIPOLYGON. The geometric representation of the zone.}
#' }
#' @details
#' This dataset is a simple feature collection with the following properties:
#' \itemize{
#'   \item Geometry type: MULTIPOLYGON
#'   \item Dimension: XY
#'   \item Bounding box: xmin: -180, ymin: -56, xmax: 180, ymax: 83.66667
#'   \item Geodetic CRS: WGS 84
#' }
#'
#' The AEZ methodology combines climatic parameters with soil and terrain conditions
#' to define zones of similar agricultural potential[2].
#'
#' @source Global Agro-Ecological Zones (GAEZ) methodology, FAO/IIASA
#'
#' @examples
#' data(aez18)
#'
#' # Plot the AEZ zones
#' plot(st_geometry(aez18))
#'
#' # Summarize the dataset
#' summary(aez18)
#'
#' # Get information for a specific AEZ
#' aez18[aez18$subnat_name == "AEZ1", ]
"aez18"
