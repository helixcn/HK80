#' Convert HK1980GRID coordinates to HK80GEO coordinates
#' 
#' Convert HK1980GRID coordinates to HK80GEO coordinates
#' 
#' This function uses equation 3, 4, 5 to convert the HK1980GRID coordinates
#' into HK80GEO coordinates. More details could be found on the explanatory
#' notes Page C9.
#' 
#' @param N Numeric, Northern coordinate in meters
#' @param E Numeric, Eastern coordinate in meters
#' @return \item{latitude }{latitude in decimal degrees} \item{longitude
#' }{longitude in decimal degrees}
#' @note The coordinates should be within the range of Hong Kong.  Providing
#' coordinates outside Hong Kong will lead to wrong results.
#' @author Jinlong Zhang
#' @seealso \code{\link{HK80GEO_TO_HK1980GRID}}
#' @references Survey & Mapping Office Lands Department, Hong Kong Government
#' (1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
#' \url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
#' @keywords HK80GEO HK1980GRID
#' @examples
#' 
#' options(digits = 15)
#' HK1980GRID_TO_HK80GEO(820351.389, 832591.320)
#' 
#' ### $latitude
#' ### [1] 22.3237017196981
#' ### 
#' ### $longitude
#' ### [1] 114.138734989417
#' 
#' #### The answer from the online conversion tool
#' ### 22.323701767
#' ### 114.138734989
#' 
#' 
#' @export HK1980GRID_TO_HK80GEO
HK1980GRID_TO_HK80GEO <-
  function(N, E) {
    ### The unit for N and E is meter
    #### HK80
    N0 <- 819069.80
    E0 <- 836694.05
    phi0 <- 22 + (18 / 60) + 43.68 / (3600)
    lambda0 <- 114 + (10 / 60) + (42.80 / 3600)
    m0 <- 1
    M0 <- 2468395.723
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
    #### iterations
    iterate <- function(x, d) {
      a <- x
      b <- fm(a)
      k <- 0
      while (((a - b) > d) | ((a - b) < -1 * d)) {
        a <- b
        b <- fm(a)
        k <- k + 1
        if (k > 1000) {
          stop("The equation does not converge. Computation Stopped .")
          return(0)
        }
      }
      return(b)
    }
    phi_rou <- iterate(0.5, d = 1e-30)
    ######################################
    ##### Verification of the value
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
