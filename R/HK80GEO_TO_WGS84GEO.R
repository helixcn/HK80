#' Convert HK80GEO coordinates to WGS84GEO coordinates
#' 
#' Convert HK80GEO coordinates to WGS84GEO coordinates
#' 
#' This function utilizes the simplified relationship between HK80GEO and
#' WGS84GEO described on Page B6 to do the transformation.
#' 
#' @param latitude latitude in decimal degrees
#' @param longitude longitude in decimal degrees
#' @return
#' 
#' \item{latitude}{latitude in decimal degrees} \item{longitude}{longitude in
#' decimal degrees}
#' @note The coordinates should be within the range of Hong Kong.  Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{WGS84GEO_TO_HK80GEO}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords WGS84GEO HK80GEO
#' @examples
#' 
#' options(digits = 15)
#' HK80GEO_TO_WGS84GEO(22.323701767, 114.138734989)
#' 
#' ### $latitude
#' ### [1] 22.3221739892222
#' ### 
#' ### $longitude
#' ### [1] 114.141179433444
#' 
#' ### Answer from the online conversion tool
#' ### 22.322172084
#' ### 114.141187917
#' 
#' @export HK80GEO_TO_WGS84GEO
HK80GEO_TO_WGS84GEO <-
  function(latitude, longitude) {
    #### The latitude and longitude should be both in decimal format.
    lat <- latitude - 5.5 / 3600
    long <- longitude + 8.8 / 3600
    res <- data.frame(latitude = lat, longitude = long)
    return(res)
  }
