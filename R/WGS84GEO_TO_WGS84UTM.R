#' Convert WGS84GEO coordinates to WGS84UTM coordinates
#' 
#' Convert WGS84GEO coordinates to WGS84UTM coordinates
#' 
#' This function uses equation 1, 2, 3 in the explanatory notes to compute the
#' WGS84UTM coordinates.
#' 
#' @param latitude Latitude in decimal degrees
#' @param longitude longitude in decimal degrees
#' @return \item{N }{The northern coordinates in meters} \item{E }{The eastern
#' coordinates in meters} \item{zone}{ zone for UTM, either 49 or 50}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{WGS84UTM_TO_WGS84GEO}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords WGS84GEO WGS84UTM
#' @examples
#' 
#' 
#' options(digits = 15)
#' WGS84GEO_TO_WGS84UTM(22 + 26/60 + 1.26/3600, 114 + 10/60 + 29.31/3600)
#' 
#' #### 22.433683333333334531
#' #### 114.17480833333333123
#' 
#' #### $N
#' #### [1] 2483566.19687669
#' #### 
#' #### $E
#' #### [1] 209189.467417282
#' #### 
#' #### $zone
#' #### [1] 50
#' 
#' ### Answer from the explanatory notes
#' ### 2483566m N
#' ### 209194m 
#'  
#' ### Answer from 
#' ### http://www.geodetic.gov.hk/smo/tform/tform.aspx
#' ### 2483568m N
#' ### 209192m E
#' 
#' 
#' @export WGS84GEO_TO_WGS84UTM
WGS84GEO_TO_WGS84UTM <-
  function(latitude, longitude) {
    #### The latitude and longitude should both in decimal format.
    phi <- latitude / (180 / pi)
    lambda <- longitude / (180 / pi)
    #########################
    #### Constants ##########
    ### WGS84LL
    N0 <- 0
    E0 <- 500000
    phi0 <- 0

    if (longitude < 114) {
      lambda0 <- 111 / (180 / pi) ### (zone 49Q)
      zone <- 49
    } else {
      if (longitude >= 114) {
        lambda0 <- 117 / (180 / pi)
      } ### (zone 50Q)
      zone <- 50
    }

    m0 <- 0.9996
    M0 <- 0
    niu_s <- 6381309.467
    rou_s <- 6344897.718
    psi_s <- 1.005738745
    a <- 6378137
    e2 <- 6.694379989e-3

    A0 <- 1 - ((e2) / 4) - (3 * (e2^2) / 64)
    A2 <- (3 / 8) * (e2 + (e2^2) / 4)
    A4 <- (15 / 256) * (e2^2)
    M <- a * (A0 * phi - A2 * sin(2 * phi) + A4 * sin(4 * phi))
    M0 <- a * (A0 * phi0 - A2 * sin(2 * phi0) + A4 * sin(4 * phi0))

    #### Eq. 1
    N <- N0 + m0 * ((M - M0) + niu_s * (sin(phi)) * ((lambda - lambda0)^2 / 2) * (cos(phi)))
    #### Eq. 2
    E <- E0 + m0 * (niu_s * (lambda - lambda0) * cos(phi) + niu_s * ((lambda - lambda0)^3 / 6) * (cos(phi)^3) * (psi_s - tan(phi)^2))
    res <- data.frame(N, E, zone)
    names(res) <- c("N", "E", "zone")
    return(res)
  }
