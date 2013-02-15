\name{panel.hist}
\alias{panel.hist}
\title{
Draw histograms and density plots in a scatterplot matrix.
}
\description{
Draws histograms with density plots along the diagonal of a scatterplot matrix.
}
\usage{
panel.hist(x, ...)
}
\arguments{
  \item{x}{
The vector of data used to plot the histogram.
}
  \item{\dots}{
Not used.
}
}
\details{
Implement by setting 'diag.panel = panel.hist' when calling the scatterplot
function.
}
\value{
None.
}
\author{
Mike Meredith, 2012-08-29, based on code from the pairs help file. 
}

\seealso{
\code{\link{pairs}}
}
\examples{
require(graphics)
data(trees)

pairs(trees, diag.panel=panel.hist)

}