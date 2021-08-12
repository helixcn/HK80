#' Convert HK80 geographical coordinates to HK80 UTM coordinates
#' 
#' Convert HK80 geographical coordinates to HK80 UTM coordinates
#' 
#' Using equation (1) to (3) in the explanatory Note within this package.  The
#' document is available at :
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' 
#' @param latitude Numeric, latitude in decimal format.
#' @param longitude Numeric, longitude in decimal format.
#' @return A list including the HK80UTM Coordinates, including: N, E, and Zone.
#' @note The latitude and longitude input should be constrained to Hong Kong.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK80UTM_TO_HK80GEO}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK80GEO HK80UTM
#' @examples
#' 
#' options(digits = 15)
#' #### The example on the explanatory notes
#' HK80GEO_TO_HK80UTM(22 + 26/60 + 6.76/3600, 114 + 10/60 + 20.46/3600)
#' #### Output
#' ##  $N
#' ##  [1] 2483772.487
#' ##  
#' ##  $E
#' ##  [1] 208931.9164
#' ##  
#' ##  $zone
#' ##  [1] "50Q"
#' 
#' ###### The results from the explanatory notes
#' #### 2483772, 208932
#' 
#' ###### The similar conversion is not available 
#' ###### from the online conversion tool.
#' 
#' 
#' @export HK80GEO_TO_HK80UTM
HK80GEO_TO_HK80UTM <-
  function(latitude, longitude) {
    #### The latitude and longitude should be both in decimal format.
    phi <- latitude / (180 / pi)
    lambda <- longitude / (180 / pi)
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
    niu_s <- 6381480.500
    rou_s <- 6359840.760
    psi_s <- 1.003402560
    a <- 6378388
    e2 <- 6.722670022e-3

    A0 <- 1 - ((e2) / 4) - (3 * (e2^2) / 64)
    A2 <- (3 / 8) * (e2 + (e2^2) / 4)
    A4 <- (15 / 256) * (e2^2)
    M <- a * (A0 * phi - A2 * sin(2 * phi) + A4 * sin(4 * phi))
    #### M0 is computed using M by putting phi = phi0
    M0 <- a * (A0 * phi0 - A2 * sin(2 * phi0) + A4 * sin(4 * phi0))

    #### Eq. 1
    N <- N0 + m0 * ((M - M0) + niu_s * (sin(phi)) * ((lambda - lambda0)^2 / 2) * (cos(phi)))
    #### Eq. 2
    E <- E0 + m0 * (niu_s * (lambda - lambda0) * cos(phi) + niu_s * ((lambda - lambda0)^3 / 6) * (cos(phi)^3) * (psi_s - tan(phi)^2))
    res <- data.frame(N, E, zone)
    colnames(res) <- c("N", "E", "zone")
    return(res)
  }
