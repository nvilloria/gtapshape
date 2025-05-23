% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_uses_by_subnatbound.R
\name{all_uses_by_subnatbound}
\alias{all_uses_by_subnatbound}
\title{Passes the names of the files with gridded data to the function
\code{\link[=aggregate_gridded_df_to_subnatbound]{aggregate_gridded_df_to_subnatbound()}}}
\usage{
all_uses_by_subnatbound(
  GADM_subnatbound_df = NULL,
  gridded.use.file.names = NULL
)
}
\arguments{
\item{GADM_subnatbound_df}{Dataframe mapping the centroids of
(five-minute?) grid-cell coordinate to countries and geographic
boundaries of interest.}

\item{gridded.use.file.names}{a vector of character strings with
the path to the dataframes with gridded information on land use
(crop output and harvested area) or land cover. These
dataframes need to have two columns labeled x and y with the
coordinates of the centroids of each five minutes gridcell on
earth, under the same projection GADM_subnatbound_df}
}
\value{
A data frame with the columns of \code{file.to.aggregate} (other
than x,y) aggregated to the countries and geographic boundaries
defined by GADM_subnatbound_df.
}
\description{
Passes the names of the files with gridded data to the function
\code{\link[=aggregate_gridded_df_to_subnatbound]{aggregate_gridded_df_to_subnatbound()}}
}
