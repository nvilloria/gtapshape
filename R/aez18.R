#' Agro-Ecological Zones (AEZ18) Dataset
#'
#' A spatial dataset containing 18 Agro-Ecological Zones (AEZ) defined
#' by the Global Agro-Ecological Zones (GAEZ) methodology. We follow the same
#' procedure used to create version 11 of the GTAP-AEZ LULC database described 
#' in \href{https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=7407}{Baldoz and Corong (2025)}.
#' Specifically, we use publicly available spatial data on the length of the 
#' growing period (LGP) and thermal climates at a global scale.  
#' Both of these datasets are available as rasters from the \href{https://gaez.fao.org/pages/data-viewer}{GAEZ data viewer}.
#' We use the LGP and thermal climate data from 1981 to 2010 to create the 
#' 18 AEZ map in our package. Both of these rasters will be downloaded when the 
#' \link[gtapshape]{getrawdata} function is executed. The
#' `vignette("create.18.aez.shapefile", package = "gtapshape")` contains the
#' code which creates the 18 AEZs from these rasters. Note, the same procedure
#' we describe below could be used to create maps using more recent data.
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
#' to define zones of similar agricultural potential.
#'
#' @source Global Agro-Ecological Zones (GAEZ) methodology, FAO/IIASA
#'
"aez18"
