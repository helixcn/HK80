% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HK1980GRID_TO_HK80UTM.R
\name{HK1980GRID_TO_HK80UTM}
\alias{HK1980GRID_TO_HK80UTM}
\title{Convert HK1980GRID coordinates to HK80UTM coordinates}
\usage{
HK1980GRID_TO_HK80UTM(N, E)
}
\arguments{
\item{N}{Numeric, Northern coordinate in meters}

\item{E}{Numeric, Eastern coordinate in meters}
}
\value{
\item{N}{Northern coordinate in meters} \item{E}{Eastern coordinate
in meters} \item{zone}{UTM zone number, either 49 or 50 }
}
\description{
Convert HK1980GRID coordinates to HK80UTM coordinates
}
\details{
This function converts the HK1980GRID coordinates into HK80GEO coordinates
(latitude and longitude) first, and converts the later into HK80UTM
coordinates.
}
\note{
The coordinates should be within the range of Hong Kong. Providing
coordinates outside Hong Kong will lead to wrong results.
}
\examples{


##### This function was not tested since 
##### the conversion is not available on the manual nor online.
options(digits = 15)
HK1980GRID_TO_HK80UTM(832699, 836055)

}
\references{
Survey & Mapping Office Lands Department, Hong Kong Government
(1995).  Explanatory Notes on Geodetic Datums in Hong Kong, available at:
\url{http://www.geodetic.gov.hk/smo/gsi/data/pdf/explanatorynotes.pdf}
}
\seealso{
\code{\link{HK80UTM_TO_HK1980GRID}}
}
\author{
Jinlong Zhang
}
\keyword{HK1980GRID}
\keyword{HK80UTM}
