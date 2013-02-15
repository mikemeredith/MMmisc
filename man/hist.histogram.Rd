\name{hist.histogram}
\alias{hist.histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Provides a hist method for objects of class histogram.
}
\description{
Seemed crazy to me that hist would work with a wide range of object classes *except* histogram.
All it does is to plot the histogram and return the original object.
}
\usage{
\method{hist}{histogram}(x, \dots)
}
\arguments{
  \item{x}{
an object of class histogram
}
  \item{\dots}{
other arguments passed to plot.histogram
}
}
\value{
Returns the histogram object.
}
\author{
Mike Meredith
}


\seealso{
\code{\link{hist}}
}
