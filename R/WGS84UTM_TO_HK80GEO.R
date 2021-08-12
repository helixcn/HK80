#' Convert WGS84UTM coordinates to HK80GEO coordinates
#' 
#' Convert WGS84UTM coordinates to HK80GEO coordinates
#' 
#' This function converts the coordinates first to WGS84GEO and then to
#' HK80GEO.
#' 
#' @param N Numeric, The northern coordinate
#' @param E Numeric, The eastern coordinate
#' @param zone zone, numeric, either 49 or 50
#' @return
#' 
#' \item{latitude}{latitude in decimal degrees} \item{longitude}{longitude in
#' decimal degrees}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK80GEO_TO_WGS84UTM}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK80GEO WGS84UTM
#' @examples
#' 
#' 
#' options(digits = 15)
#' WGS84UTM_TO_HK80GEO(2471279, 205494, zone = 50)
#' 
#' #### $latitude
#' #### [1] 22.323667977877
#' #### 
#' #### $longitude
#' #### [1] 114.138738570444
#' 
#' #### Results from http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' #### 22.323701767
#' #### 114.138734989
#' 
#' 
#' @export WGS84UTM_TO_HK80GEO
WGS84UTM_TO_HK80GEO <-
  function(N, E, zone) {
    ### The unit for N and E is meter, and zone is either 49 or 50
    temp1 <- WGS84UTM_TO_WGS84GEO(N, E, zone)
    res <- WGS84GEO_TO_HK80GEO(temp1$latitude, temp1$longitude)
    return(res)
  }
