\name{mcmcHPD}
\alias{mcmcHPD}
\title{
Highest Probability Density interval
}
\description{
Calculate Highest Probability Density interval from a vector of values drawn from a distribution. Typically used with output from MCMC estimation of a posterior density.
}
\usage{
mcmcHPD(x, ci = 0.95)
}
\arguments{
  \item{x}{
a vector of values representing the distribution of interest
}
  \item{ci}{
the width of the credible interval required
}
}
\value{
A names numeric vector of length 2, with upper and lower limits of the credible interval.
}
\author{
Mike Meredith
}
\note{
Similar functions appear in TeachingDemos and in Kruschke's book.
}

\examples{
# Create some simulated data:
simdat <- rnorm(10000)
mcmcHPD(simdat)
}
