\name{getArea}
\alias{getArea}

\title{
Function to calculate the area of a polygon
}
\description{
Calculates the area of a polygon
}

\usage{
getArea(x, y = NULL)
}

\arguments{
  \item{x}{
a vector of coordinates OR a 2-column matrix of coordinates OR a data frame with columns 'x' and 'y' containing the coordinates. The last pair of coordinates need not be the same as the first pair.
}
  \item{y}{
if x is a vector, the corresponding vector of coordinates, otherwise NULL
}
}

\value{
The area of the polygon. If the vertices are listed in anticlockwise order, the area will be negative.
}
\references{
Andy Royle's function from scrbook package
}
\author{
Andy Royle, adapted (slightly) by Mike Meredith
}

\examples{
# An arbitrary polygon
x <- c(0, 1, 4, 5)
y <- c(0, 1, 3, 1)
plot(x, y)
polygon(x, y)
getArea(x, y)

# A data frame with an anticlockwise listing
getArea(data.frame(y=c(0, 1, 3, 1), x=c(0, 5, 4, 1)))
}

\keyword{spatial}
