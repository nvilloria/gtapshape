#' Round Up Coordinates of Gridded Dataframes
#'
#'@param raster.df a dataframe with x,y coordinates
#'@return a dataframe with coordinates rounded up to six digits
#'
#' @seealso \link{apply_raster_properties} which usually preceds the
#'     application of this function.
#' @export
#'
#' @examples
#' \dontrun{
#' anyraster <- raster(matrix(runif(100), 10, 10))
#' goodraster <- apply_raster_properties(anyraster)
#' }
round_up_coordinates <- function(raster.df){
    raster.df$x <- round(raster.df$x, 6)
    raster.df$y <- round(raster.df$y, 6)
    return(raster.df)
    }
