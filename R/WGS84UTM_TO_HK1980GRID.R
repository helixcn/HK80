#' Convert WGS84UTM coordinates to HK1980GRID coordinates
#' 
#' Convert WGS84UTM coordinates to HK1980GRID coordinates
#' 
#' This function converts the coordinates to WGS84GO, and then to HK80GO and
#' eventually to HK1980GRID.
#' 
#' @param N Numerical, Northern coordinate in meters.
#' @param E Numerical, Eastern coordinate in meters.
#' @param zone Numerical, either 49 or 50.
#' @return \item{N}{Numerical, Northern coordinate in meters. }
#' \item{E}{Numerical, Eastern coordinate in meters. }
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK1980GRID_TO_WGS84UTM}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords WGS84UTM HK1980GRID
#' @examples
#' 
#' options(digits = 15)
#' WGS84UTM_TO_HK1980GRID(2471279, 205494, zone = 50)
#' #### $N
#' #### [1] 820347.647263194
#' #### 
#' #### $E
#' #### [1] 832591.689923123
#' 
#' ######################
#' #### Results from http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' #### 820351.389, 832591.320
#' 
#' 
#' @export WGS84UTM_TO_HK1980GRID
WGS84UTM_TO_HK1980GRID <-
  function(N, E, zone) {
    temp1 <- WGS84UTM_TO_WGS84GEO(N, E, zone)
    temp2 <- WGS84GEO_TO_HK80GEO(temp1$latitude, temp1$longitude)
    res <- HK80GEO_TO_HK1980GRID(temp2$latitude, temp2$longitude)
    return(res)
  }
