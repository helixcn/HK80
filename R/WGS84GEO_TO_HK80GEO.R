#' Convert WGS84GEO coordinates to HK80GEO coordinates
#' 
#' Convert WGS84GEO coordinates to HK80GEO coordinates
#' 
#' This function uses the simplified relationship between WGS84GEO and HK80GEO.
#' See Page C4 on the explanatory note.
#' 
#' @param latitude latitude in decimal degrees
#' @param longitude longitude in decimal degrees
#' @return \item{latitude }{latitude in decimal degrees}
#' \item{longtitude}{longitude in decimal degrees}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK80GEO_TO_WGS84GEO}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords WGS84GEO HK80GEO
#' @examples
#' 
#' options(digits = 15)
#' WGS84GEO_TO_HK80GEO(22.322172084,  	114.141187917)
#' #### $latitude
#' #### [1] 22.3236998617778
#' #### 
#' #### $longitude
#' #### [1] 114.138743472556
#' 
#' #### Answer from the online conversion tool
#' #### http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' #### 22.323701767,  114.138734989
#' 
#' 
#' @export WGS84GEO_TO_HK80GEO
WGS84GEO_TO_HK80GEO <-
  function(latitude, longitude) {
    #### The latitude and longitude should be both in decimal format.
    lat <- latitude + 5.5 / 3600
    long <- longitude - 8.8 / 3600
    res <- data.frame(latitude = lat, longitude = long)
    return(res)
  }
