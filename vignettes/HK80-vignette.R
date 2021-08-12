## -----------------------------------------------------------------------------
library(HK80)
HK1980GRID_TO_WGS84GEO(N = 820359.389, E = 832591.320)

## -----------------------------------------------------------------------------
library(jsonlite)
data1 <- fromJSON("http://www.geodetic.gov.hk/transform/v2/?inSys=hkgrid&e=832591.320&n=820359.389")
names(data1)

data1$wgsLat

data1$wgsLong

## -----------------------------------------------------------------------------
library(sf)
p1 = st_point(c(832591.320, 820359.389))
sfc = st_sfc(p1, crs = 2326)
(st_transform(sfc, 4326))

## -----------------------------------------------------------------------------
library(HK80)
WGS84GEO_TO_HK1980GRID(latitude = 22.32224, longitude = 114.14118)

## -----------------------------------------------------------------------------
# Copy the following URL to browser
# http://www.geodetic.gov.hk/transform/v2/?inSys=wgsgeog&outSys=hkgrid&lat=22.32224&long=114.14118&h=23.128
# {"hkN": 820358.910,"hkE": 832590.508,"hkpd": 26.009}

library(jsonlite)
data1 <- fromJSON("http://www.geodetic.gov.hk/transform/v2/?inSys=wgsgeog&outSys=hkgrid&lat=22.32224&long=114.14118&h=23.128")

names(data1)

data1$hkN
data1$hkE

## -----------------------------------------------------------------------------
library(sf)
p1 = st_point(c(114.14118, 22.32224))
sfc = st_sfc(p1, crs = 4326)
(ccc <- st_transform(sfc, 2326))

## -----------------------------------------------------------------------------
library(sp)
dd2dms(114.14118) # decimal to Degree, Minute, Second format
as.numeric(dd2dms(114.14118)) #

char2dms("47d15'6.12\"E")
as.numeric(char2dms("47d15'6.12\"E"))

## -----------------------------------------------------------------------------
library(biogeo)
res <- dms2dd(47,15,6.12,"E") # ns letters (N,S,E,W)
print(res)

dd2dmslong(114.14118)
dd2dmslat(22.32224)

## -----------------------------------------------------------------------------
sessionInfo()

