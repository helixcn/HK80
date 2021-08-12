#' Convert WGS84GEO coordinates to HK80UTM coordinates
#' 
#' Convert WGS84GEO coordinates to HK80UTM coordinates
#' 
#' This function first converts the WGS84GEO coordinates to WGS84UTM, and then
#' converts the corresponding WGS84UTM coordinates to HK80UTM.
#' 
#' @param latitude latitude in decimal degrees
#' @param longitude longitude in decimal degrees
#' @return
#' 
#' \item{N}{Northern coordinate in meters} \item{E}{Eastern coordinate in
#' meters} \item{zone}{zone, either 49 or 50}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK80UTM_TO_WGS84GEO}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords WGS84GEO HK80UTM
#' @examples
#' 
#' #### The similar conversion is not available at 
#' #### http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' #### Therefore it the output has not been verified.
#' options(digits = 15)
#' WGS84GEO_TO_HK80UTM(22.322172084, 114.141187917)
#' 
#' 
#' @export WGS84GEO_TO_HK80UTM
WGS84GEO_TO_HK80UTM <-
  function(latitude, longitude) {
    #### The latitude and longitude should be both in decimal format.
    temp <- WGS84GEO_TO_WGS84UTM(latitude, longitude)
    res <- WGS84UTM_TO_HK80UTM(temp$N, temp$E, temp$zone)
    return(res)
  }
