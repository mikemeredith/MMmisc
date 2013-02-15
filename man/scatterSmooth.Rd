\name{scatterSmooth}
\alias{scatterSmooth}
\title{
Scatter plot with smooth curve fitted by loess
}
\description{
Plot and add a smooth curve computed by loess to a scatter plot. 
}
\usage{
scatterSmooth(x, y = NULL, data = parent.frame(),
  span = 0.75, degree = 2, family = "gaussian",
  xlab = NULL, ylab = NULL, ylim, evaluation = 50, add = FALSE, CI = NULL,
  col.smooth = "red", lwd.smooth = 1, lty.smooth = 1, ...)
}
\arguments{
  \item{x,y}{
the x and y arguments provide the x and y coordinates for the plot. Any reasonable way of defining the coordinates is acceptable. See the function xy.coords for details.
}
  \item{data}{
data frame or environment containing the variables.
}
  \item{span}{
smoothness parameter for loess.
}
  \item{degree}{
degree of local polynomial used.
}
  \item{family}{
if "gaussian" fitting is by least-squares, and if family="symmetric" a re-descending M estimator is used.
}
  \item{xlab, ylab}{
labels for the x and y axes
}
  \item{ylim}{
y limits of the plot
}
  \item{evaluation}{
number of points at which to evaluate the smooth curve.
}
  \item{add}{
if TRUE, the loess line will be added to an existing plot.
}
  \item{CI}{
the confidence interval to be plotted, or NULL (the default) to suppress plotting of the confidence interval.
}
  \item{col.smooth, lwd.smooth, lty.smooth}{
colour, line width and line type for the loess smoother line.
}
  \item{\dots}{
graphical parameters.
}
}
\details{
This is identical to stats::scatter.smooth except that:
1. it allows you to specify the colour, width and line type of the loess line; defaults to col="red", lwd=1, lty=1. 
2. it suppresses warnings from the loess.smooth function for sparse data sets.
3. with add = TRUE, it allows a loess line to be added to an existing scatterplot.
}
\value{
None
}
\author{
Mike Meredith, 28 Aug 2012, adapted from stats::scatter.smooth.
}
\seealso{
\code{\link{loess}}, \code{\link{scatter.smooth}}
}
\examples{
require(graphics)
data(trees)

scatterSmooth(trees$Girth, trees$Volume)

with(trees, scatterSmooth(Volume ~ Height))

plot(trees$Volume ~ trees$Girth)
abline(lm(Volume ~ Girth, data = trees), col='blue')
scatterSmooth(trees$Volume ~ trees$Girth, add=TRUE)

}
