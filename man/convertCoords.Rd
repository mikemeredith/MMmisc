\name{convertCoords}
\alias{convertCoords}

\title{
Convert coordinates from one Coordinate Reference System to another
}

\description{
Convert coordinates from one Coordinate Reference System to another
}

\usage{
convertCoords(coords, from = 4326, to)
}

\arguments{
  \item{coords}{
a two-column matrix or data frame with easting/longitude in the first column, northing/latitude in the second column
}
  \item{from}{
the EPSG code for the Coordinate Reference System of the input data (default WGS84)
}
  \item{to}{
the EPSG code for the CRS of the output data.
}
}
\details{
For EPSG codes, go to http://spatialreference.org/ref/epsg/ or just Google 'EPSG' with the desired CRS name.

The function requires the 'sp' and 'rgdal' packages.
}
\value{
A data frame or matrix with the converted coordinates.
}

\author{
Mike Meredith
}

\examples{
# A matrix of coordinates
x <- cbind(c(0, 1, 4, 5), c(0, 1, 3, 1))
print(convertCoords(x, to=32631))  # UTM zone 31N
}

\keyword{spatial}
