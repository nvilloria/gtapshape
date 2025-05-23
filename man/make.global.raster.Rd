% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.global.raster.R
\name{make.global.raster}
\alias{make.global.raster}
\title{Make a Global Raster}
\usage{
make.global.raster()
}
\value{
A raster.
}
\description{
This empty raster with has a set of properties that are imposed on
all the rasters in the package in order to ensure that they align
once they are converted to dataframes. This could be more directly
achieved using the package raster, but because the sheer number of
raster files (and their sheer sizes) working with the rasters
directly is impractical.
}
