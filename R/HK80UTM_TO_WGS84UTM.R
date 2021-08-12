#' Convert HK80UTM coordinates to WGS84UTM coordinates
#' 
#' Convert HK80UTM coordinates to WGS84UTM coordinates
#' 
#' This function uses the simplified relationship described on page B6 to
#' convert HK80UTM to WGS84UTM coordinates.
#' 
#' @param N Numeric, Northern coordinate in meters.
#' @param E Numeric, Eastern coordinate in meters.
#' @param zone zone, UTM zone number, either 49 or 50.
#' @return
#' 
#' \item{N}{Northern coordinate in meters} \item{E}{Eastern coordinate in
#' meters} \item{zone}{UTM zone, either 49 or 50}
#' @note The coordinates should be within the range of Hong Kong.  Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{WGS84UTM_TO_HK80UTM}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK80UTM WGS84UTM
#' @examples
#' 
#' #### This function was not tested since similar 
#' #### conversion service is not available at 
#' #### http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' options(digits = 15)
#' HK80UTM_TO_WGS84UTM(N = 2471279, E = 205494, zone = 50)
#' 
#' @export HK80UTM_TO_WGS84UTM
HK80UTM_TO_WGS84UTM <-
  function(N, E, zone = c(49, 50)) {
    ### The unit for N and E is meter. The zone is either 49 or 50
    if (zone == 49) {
      res.N <- N - 195
      res.E <- E + 245
    }
    if (zone == 50) {
      res.N <- N - 205
      res.E <- E + 260
    }
    return(data.frame(N = res.N, E = res.E, zone = zone))
  }
