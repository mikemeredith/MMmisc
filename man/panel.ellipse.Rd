\name{panel.ellipse}
\alias{panel.ellipse}
\title{
Display a confidence ellipse region in a scatterplot matrix
}
\description{
Display the estimated bivariate mean and its 50% confidence region in the panel of a scatterplot matrix}
}
\usage{
panel.ellipse(x, y, col.fill = "pink", level = 0.5, ...)
}
\arguments{
  \item{x, y}{
numeric vectors of the same length
}
  \item{col.fill}{
numeric or character codes for the fill colour of the ellipse.
}
  \item{level}{
confidence level, default 0.50
}
  \item{\dots}{
further arguments to ellipse.
}
}
\details{
The function uses the function ellipse in package ellipse; if this is not available, the ellipse is not drawn. No warning is given.
}
\value{
None.
}
\author{
Mike Meredith, 26 Aug 2012
}
\seealso{
\code{\link{pairs}}
}
\examples{
require(graphics)
data(trees)
pairs(trees, upper.panel=panel.ellipse)
}

