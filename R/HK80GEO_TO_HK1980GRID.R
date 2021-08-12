#' Convert the HK80GEO coordinates to HK1980GRID coordinates
#' 
#' Convert the HK80GEO coordinates to HK1980GRID coordinates
#' 
#' This functions uses equation 1,2,3 in the explanatory note (page C9) to
#' convert the HK80GEO coordinates into HK1980GRID coordinates.
#' 
#' @param latitude latitude in decimal degrees
#' @param longitude longitude in decimal degrees
#' @return \item{N }{Northern coordinate in meters} \item{E }{Eastern
#' coordinate in meters}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK1980GRID_TO_HK80GEO}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK1980GRID HK80GEO
#' @examples
#' 
#' options(digits = 15)
#' HK1980GRID_TO_HK80GEO(820351.389, 832591.320)
#' #### $latitude
#' #### [1] 22.3237017196981
#' #### 
#' #### $longitude
#' #### [1] 114.138734989417
#' 
#' ####  Answer from the online conversion tool
#' ### 22.323701767
#' ### 114.138734989
#' 
#' @export HK80GEO_TO_HK1980GRID
HK80GEO_TO_HK1980GRID <-
  function(latitude, longitude) {
    #### The latitude and longitude should be both in decimal format.
    phi <- latitude / (180 / pi)
    lambda <- longitude / (180 / pi)
    N0 <- 819069.80
    E0 <- 836694.05
    phi0 <- (22 + (18 / 60) + 43.68 / (3600)) / (180 / pi)
    lambda0 <- (114 + (10 / 60) + (42.80 / 3600)) / (180 / pi)
    m0 <- 1
    M0 <- 2468395.723
    niu_s <- 6381480.500
    rou_s <- 6359840.760
    psi_s <- 1.003402560
    a <- 6378388
    e2 <- 6.722670022e-3

    A0 <- 1 - ((e2) / 4) - (3 * (e2^2) / 64)
    A2 <- (3 / 8) * (e2 + (e2^2) / 4)
    A4 <- (15 / 256) * (e2^2)
    M <- a * (A0 * phi - A2 * sin(2 * phi) + A4 * sin(4 * phi))
    M0 <- a * (A0 * phi0 - A2 * sin(2 * phi0) + A4 * sin(4 * phi0))

    #### Eq. 1
    N <- N0 + m0 * ((M - M0) + niu_s * (sin(phi)) * ((lambda - lambda0)^2 / 2) * (cos(phi)))
    #### Eq. 2
    E <- E0 + m0 * (niu_s * (lambda - lambda0) * cos(phi) + niu_s * ((lambda - lambda0)^3 / 6) * (cos(phi)^3) * (psi_s - tan(phi)^2))
    res <- data.frame(N = N, E = E)
    return(res)
  }
