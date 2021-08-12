#' Convert HK1980GRID coordinates to WGS84GEO coordinates
#' 
#' Convert HK1980GRID coordinates to WGS84GEO coordinates
#' 
#' This function converts the coordinates to HK80GEO first, and convert the
#' later to WGS84GEO coordinates.
#' 
#' @param N Numeric, Northern coordinate in meters
#' @param E Numeric, Eastern coordinate in meters
#' @return \item{latitude }{latitude in decimal degrees} \item{longitude
#' }{longitude in decimal degrees}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{WGS84GEO_TO_HK1980GRID}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK1980GRID WGS84GEO
#' @examples
#' 
#' options(digits = 15)
#' HK1980GRID_TO_WGS84GEO(820351.389, 832591.320)
#' #### $latitude
#' #### [1] 22.3221739419203
#' #### 
#' #### $longitude
#' #### [1] 114.141179433862
#' 
#' ####  Answer from the online conversion tool
#' ####  http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' ####  22.322172084 	114.141187917
#' 
#' @export HK1980GRID_TO_WGS84GEO
HK1980GRID_TO_WGS84GEO <-
  function(N, E) {
    ### The unit for N and E is meters
    temp <- HK1980GRID_TO_HK80GEO(N, E)
    res <- HK80GEO_TO_WGS84GEO(temp$latitude, temp$longitude)
    return(res)
  }
