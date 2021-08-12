#' Convert WGS84UTM coordinates TO HK80UTM coordinates
#' 
#' Convert WGS84UTM coordinates TO HK80UTM coordinates
#' 
#' This functions uses a simplified relationship to convert the coordinates.
#' If zone is 49, res.N = N + 195, res.E = E - 245; if zone is 50, res.N = N +
#' 205, res.E = E - 260
#' 
#' @param N Numeric, Northern coordinate in meter
#' @param E Numeric, Eastern coordinate in meter
#' @param zone zone, numeric, either 49 or 50
#' @return \item{N }{Northern coordinates in meters} \item{E }{Eastern
#' coordinates in meters} \item{zone }{either 49 or 50}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK80UTM_TO_WGS84UTM}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK80UTM WGS84UTM
#' @examples
#' 
#' #### The similar conversion is not available at 
#' #### http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' options(digits = 15)
#' WGS84UTM_TO_HK80UTM(2471279, 205494, zone = 50)
#' 
#' 
#' @export WGS84UTM_TO_HK80UTM
WGS84UTM_TO_HK80UTM <-
  function(N, E, zone = c(49, 50)) {
    ### The unit for N and E is meter
    if (zone == 49) {
      res.N <- N + 195
      res.E <- E - 245
    }
    if (zone == 50) {
      res.N <- N + 205
      res.E <- E - 260
    }
    return(data.frame(N = res.N, E = res.E, zone = zone))
  }
