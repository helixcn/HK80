#' Convert HK1980GRID coordinates to HK80UTM coordinates
#' 
#' Convert HK1980GRID coordinates to HK80UTM coordinates
#' 
#' This function converts the HK1980GRID coordinates into HK80GEO coordinates
#' (latitude and longitude) first, and converts the later into HK80UTM
#' coordinates.
#' 
#' @param N Numeric, Northern coordinate in meters
#' @param E Numeric, Eastern coordinate in meters
#' @return \item{N}{Northern coordinate in meters} \item{E}{Eastern coordinate
#' in meters} \item{zone}{UTM zone number, either 49 or 50 }
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK80UTM_TO_HK1980GRID}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK1980GRID HK80UTM
#' @examples
#' 
#' 
#' ##### This function was not tested since 
#' ##### the conversion is not available on the manual nor online.
#' options(digits = 15)
#' HK1980GRID_TO_HK80UTM(832699, 836055)
#' 
#' @export HK1980GRID_TO_HK80UTM
HK1980GRID_TO_HK80UTM <-
  function(N, E) {
    ### The unit for N and E is meter
    temp <- HK1980GRID_TO_HK80GEO(N, E)
    res <- HK80GEO_TO_HK80UTM(temp$latitude, temp$longitude)
    return(res)
  }
