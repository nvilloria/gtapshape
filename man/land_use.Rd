% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/land_use.R
\name{land_use}
\alias{land_use}
\title{Land use}
\usage{
land_use(
  crop_production_rast_dir,
  crop_area_rast_dir,
  livestock_density_rast_dir,
  workdir_dir = getwd()
)
}
\arguments{
\item{crop_production_rast_dir}{Directory containing rasters with
data on production of 175 crops}

\item{crop_area_rast_dir}{Directory containing rasters with data on
harvested area of 175 crops}

\item{livestock_density_rast_dir}{Directory containing rasters with data
on density of livestock animals}

\item{tmp_dir}{Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()}
}
\value{
Saves files containing land use data for each combination of GADM
country and subnational division
}
\description{
This function creates crop output, harvested area (ha) data, and livestock
numbers for country-BIOME combinations using the rasters from Monfreda et
al (2008) and the Gridded Livestock of the World livestock density data.
The countries will be aggregated to the regions (REG) defined in using the
define_sets function later. The concatenated country-BIOME raster is created
by the land_cover function.
}
