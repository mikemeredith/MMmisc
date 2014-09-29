
# Conversion of coordinates for simple polygons with just x, y data

# Arguments:
#  coords : a two-column matrix or data frame with easting/longitude
#     in the first column, northing/latitude in the second column.
# from : the EPSG code for the Coordinate Reference System of the
#      input data (default WGS84)
# to : the EPSG code for the CRS of the output data.
#
# Value : a matrix or data frame with the transformed coordinates.

# Requires sp and rgdal packages

convertCoords <- function(coords, from=4326, to) {
  stopifnot(ncol(coords) == 2)
  coordsDF <- as.data.frame(coords)
  require(sp)
  require(rgdal)

  coordinates(coordsDF) <- colnames(coordsDF)
  fromCRS <- paste0("+init=epsg:", from)
  toCRS <- paste0("+init=epsg:", to)
  proj4string(coordsDF) <- CRS(fromCRS)
  out <- coordinates(spTransform(coordsDF, CRS(toCRS)))
  if(is.matrix(coords))
    out <- as.matrix(out)
  invisible(out)
}

