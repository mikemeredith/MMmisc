\name{cloglog}
\alias{cloglog}
\alias{invcloglog}
\title{
The complementary log-log function
}
\description{
The complementary log-log function and its inverse function are provided. 
}
\usage{
cloglog(p)
invcloglog(x)
}
\arguments{
  \item{p}{
a vector of probabilities p in the interval [0,1] that will be transformed to the real line.
}
  \item{x}{
a vector of real values that will be transformed to the interval [0,1].
}
}

\value{
cloglog returns x, invcloglog returns probability p. 
}
\author{
Mike Meredith
}
\seealso{
\code{\link[LaplacesDemon]{cloglog}} in \code{LaplacesDemon}, which also includes log-log function and its inverse.
}
\examples{
p <- (0:5)/5
p
x <- cloglog(p)
x
invcloglog(x)
}
