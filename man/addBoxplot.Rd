\name{addBoxplot}
\alias{addBoxplot}
\title{Add a box plot in the margin of a scatterplot}
\description{
Add a box plot in the margin of a scatterplot
}
\usage{
addBoxplot(x, gap = 0.01, side, outpch = 20)
}
\arguments{
  \item{x}{
a numeric vector, the data to display in the boxplot
}
  \item{gap}{
distance above or right of the edge of the plot, as a proportion of the width of the plot; negative values will position the boxplot below or left of the edge.
}
  \item{side}{
1=bottom, 2=left, 3=top, 4=right
}
  \item{outpch}{
plotting symbol to use for outliers; choose one which won't be confused with the points in the scatterplot.
}
}
\details{
Box plots of each variable along the appropriate side are a nice idea (cf Quinn & Keough) but take up lots of space and require multiplot layout.
As Tufte points out, the box isn't necessary, so this fits a box-ish plot into the frame of the scatter plot.
}
\value{
Nothing; used for its plotting side-effect.
}
\references{
Quinn, G P; M J Keough 2002. Experimental design and data analysis for biologists. Cambridge University Press, Cambridge UK.
}
\author{
Mike Meredith, 29 April 2012
}

\examples{
data(trees)
plot(Volume ~ Girth, data=trees, bty='l')
addBoxplot(trees$Volume, side=4)
addBoxplot(trees$Girth, side=3)
}
