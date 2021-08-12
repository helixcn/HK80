#' Convert the HK80GEO coordinates to WGS84UTM coordinates
#' 
#' Convert the HK80GEO coordinates to WGS84UTM coordinates
#' 
#' This function convert the HK80GEO coordinates to HK80UTM, and convert the
#' intermediate output into WGS84UTM.  More details can be found on page C4.
#' 
#' @param latitude latitude in decimal degrees
#' @param longitude longitude in decimal degrees
#' @return \item{N}{Northern coordinate in meters} \item{E }{Eastern coordinate
#' in meters} \item{zone }{UTM zone, either 49 or 50}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{WGS84UTM_TO_HK80GEO}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords WGS84UTM HK80GEO
#' @examples
#' 
#' options(digits = 15)
#' HK80GEO_TO_WGS84UTM(22.323701767, 114.138734989 )
#' #### $N
#' #### [1] 2471278.72895382
#' #### 
#' #### $E
#' #### [1] 205493.220909862
#' #### 
#' #### $zone
#' #### [1] 50
#' 
#' ################################################
#' #### Answer from the online conversion tool 
#' #### 50Q, 2471279, 205494
#' 
#' @export HK80GEO_TO_WGS84UTM
HK80GEO_TO_WGS84UTM <-
  function(latitude, longitude) {
    #### The latitude and longitude should be both in decimal format.
    temp <- HK80GEO_TO_HK80UTM(latitude, longitude)
    res <- HK80UTM_TO_WGS84UTM(temp$N, temp$E, zone = temp$zone)
    return(res)
  }
