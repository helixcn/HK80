#' Convert HK80UTM coordinates to WGS84GEO coordinates
#' 
#' Convert HK80UTM coordinates to WGS84GEO coordinates
#' 
#' This function converts the HK80UTM coordinates into WGS84UTM coordinates
#' first, and convert the intermediate results into WGS84GEO coordinates
#' (latitude and longitude)
#' 
#' @param N Numeric, Northern coordinate in meters
#' @param E Numeric, Eastern coordinate in meters
#' @param zone zone, UTM zone number, for Hong Kong, either 49 or 50
#' @return \item{latitude }{latitude in decimal degrees} \item{longitude
#' }{longitude in decimal degrees}
#' @note The coordinates should be within the range of Hong Kong.  Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{WGS84GEO_TO_HK80UTM}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords WGS84GEO HK80UTM
#' @examples
#' 
#' #### The similar conversion is not available 
#' #### at http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' #### Therefore, its accuracy is unknown.
#' options(digits = 15)
#' HK80UTM_TO_WGS84GEO(N = 2471279, E = 205494, zone = 50)
#' 
#' @export HK80UTM_TO_WGS84GEO
HK80UTM_TO_WGS84GEO <-
  function(N, E, zone) {
    ### The unit for N and E is meter. The zone is either 49 or 50
    temp <- HK80UTM_TO_WGS84UTM(N, E, zone)
    res <- WGS84UTM_TO_WGS84GEO(temp$N, temp$E, temp$zone)
    return(res)
  }
