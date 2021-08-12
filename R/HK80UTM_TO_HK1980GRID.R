#' Convert HK80UTM coordinates to HK1980GRID coordinates
#' 
#' Convert HK80UTM coordinates to HK1980GRID coordinates
#' 
#' This function converts the coordinates to HK80GEO first, and convert the
#' intermediate results into HK1980GRID coordinates.
#' 
#' @param N Numeric, Northern coordinate in meters
#' @param E Numeric, Eastern coordinate in meters
#' @param zone zone, UTM zone, either 49 or 50 for Hong Kong.
#' @return \item{N}{Northern coordinate in meters} \item{E}{Eastern coordinate
#' in meters}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK1980GRID_TO_HK80UTM}}
#' @references
#' 
#' Survey & Mapping Office Lands Department, Hong Kong Government (1995).
#' Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK1980GRID HK80UTM
#' @examples
#' 
#' 
#' #### The similar conversion is not available at 
#' #### http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' #### The accuracy is unknown
#' options(digits = 15)
#' HK80UTM_TO_HK1980GRID(N = 2471279, E = 205494, zone = 50)
#' 
#' 
#' @export HK80UTM_TO_HK1980GRID
HK80UTM_TO_HK1980GRID <-
  function(N, E, zone) {
    ### The unit for N and E is meter. The zone is either 49 or 50
    temp <- HK80UTM_TO_HK80GEO(N, E, zone)
    res <- HK80GEO_TO_HK1980GRID(temp$latitude, temp$longitude)
    return(res)
  }
