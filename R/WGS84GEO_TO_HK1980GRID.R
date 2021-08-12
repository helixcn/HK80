#' Covert WGS84GEO geographical coordinates TO HK1980GRID coordinates
#' 
#' Covert WGS84GEO geographical coordinates TO HK1980GRID coordinates
#' 
#' This function convert the latitude and longitude to HK80GEO coordinates
#' first, and converts the later to HK1980GRID coordinates.
#' 
#' @param latitude latitude in decimal degrees
#' @param longitude longitude in decimal degrees
#' @return \item{N}{Northern coordinate in meters} \item{E}{Eastern coordinate
#' in meters}
#' @note The coordinates should be within the range of Hong Kong.  Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK1980GRID_TO_WGS84GEO}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK1980GRID WGS84GEO
#' @examples
#' 
#' 
#' options(digits = 15)
#' HK1980GRID_TO_WGS84GEO(820351.389, 832591.320)
#' ### $latitude
#' ### [1] 22.3221739419203
#' ### 
#' ### $longitude
#' ### [1] 114.141179433862
#' 
#' ### Answer from the online conversion tool
#' ### http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' ### 22.322172084
#' ### 114.141187917
#' 
#' 
#' @export WGS84GEO_TO_HK1980GRID
WGS84GEO_TO_HK1980GRID <-
  function(latitude, longitude) {
    #### The latitude and longitude should be in decimal format.
    temp <- WGS84GEO_TO_HK80GEO(latitude, longitude)
    res <- HK80GEO_TO_HK1980GRID(temp$latitude, temp$longitude)
    return(res)
  }
