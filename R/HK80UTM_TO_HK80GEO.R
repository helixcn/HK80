#' Convert the HK80UTM coordinates to HK80GEO coordinates
#' 
#' Convert the HK80UTM coordinates to HK80GEO coordinates
#' 
#' This function uses equation 3, 4, 5 in the explanatory notes to convert the
#' HK80UTM coordinates into HK80GEO coordinates (latitude and longitude) in
#' decimal degrees.  Mode details could be found at: Page C9 and C10 on the
#' explanatory notes.
#' 
#' @param N Numeric, Northern coordinate in meters.
#' @param E Numeric, Eastern coordinate in meters.
#' @param zone zone, UTM zone, either 49 or 50 for Hong Kong.
#' @return \item{latitude }{latitude in decimal degrees}
#' \item{longitude}{longitude in decimal degrees}
#' @note The coordinates should be within the range of Hong Kong. Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK80GEO_TO_HK80UTM}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK80GEO HK80UTM
#' @examples
#' 
#' options(digits = 15)
#' HK80UTM_TO_HK80GEO(2483775, 208930, zone = 50)
#' 
#' #### $latitude
#' #### [1] 22.435188997523
#' #### 
#' #### $longitude
#' #### [1] 114.172349350774
#' 
#' ##### Answer from the explanatory note:
#' ### 22 + 26/60 + 06.89/3600 = 22.43524722
#' ### 114 + 10/60 + 20.39/3600 = 114.1723306
#' 
#' @export HK80UTM_TO_HK80GEO
HK80UTM_TO_HK80GEO <-
  function(N, E, zone = c(49, 50)) {
    ### The unit for N and E is meter. The zone is either 49 or 50
    N0 <- 0
    E0 <- 500000
    phi0 <- 0
    if (zone == 49) {
      lambda0 <- 111
    }
    if (zone == 50) {
      lambda0 <- 117
    }
    m0 <- 0.9996
    M0 <- 0
    niu_s <- 6381480.500
    rou_s <- 6359840.760
    psi_s <- 1.003402560
    a <- 6378388
    e2 <- 6.722670022e-3

    A0 <- 1 - (e2 / 4) - (3 * (e2^2) / 64)
    A2 <- (3 / 8) * (e2 + (e2^2) / 4)
    A4 <- (15 / 256) * (e2^2)
    delta_N <- N - N0
    ###################################################################
    fm <- function(x) {
      return((((delta_N + M0) / m0) / a + A2 * sin(2 * x) - A4 * sin(4 * x)) / A0)
    }
    #### Iterations
    iterate <- function(x, d) {
      a <- x
      b <- fm(a)
      k <- 0
      while (((a - b) > d) | ((a - b) < -1 * d)) {
        ### print(a);
        a <- b
        b <- fm(a)
        k <- k + 1
        if (k > 1000) {
          stop("The equation does not converge. Stopped computation.")
          return(0)
        }
      }
      return(b)
    }
    phi_rou <- iterate(0.5, d = 1e-30)
    ######################################
    ##### Verification
    #####  a*(A0 * phi_rou - A2 * sin(2 * phi_rou) + A4 * sin(4*phi_rou))
    ##### (delta_N + M0) / m0

    t_rou <- tan(phi_rou)
    niu_rou <- a / sqrt(1 - e2 * (sin(phi_rou)^2))
    rou_rou <- a * (1 - e2) / ((1 - e2 * (sin(phi_rou)^2))^(3 / 2))
    psi_rou <- niu_rou / rou_rou

    delta_E <- E - E0

    #### Equation 4.
    lambda <- (lambda0 / (180 / pi) + (1 / cos(phi_rou)) * (delta_E / (m0 * niu_rou)) -
      (1 / cos(phi_rou)) * (delta_E^3 / (6 * m0^3 * niu_rou^3)) * (psi_rou + 2 * t_rou^2))

    #### Equation 5.
    phi <- phi_rou - (t_rou / (m0 * rou_rou)) * (delta_E^2 / (2 * m0 * (niu_rou)))

    res <- data.frame(latitude = phi * (180 / pi), longitude = lambda * (180 / pi))
    return(res)
  }
