\name{panel.cor}
\alias{panel.cor}
\title{
Display correlation coefficients in a scatterplot matrix.
}
\description{
Displays the absolute value of the Pearson correlation coefficient between pairs of variables in a scatterplot matrix. The size of the test depends on the size of the coefficient. The background colour depends on the p-value for the test of the null hypothesis of zero correlation: \cr
orange < 0.001 < gold < 0.01 < yellow < 0.05 < pale yellow < 0.1 < white\cr
For large data sets, all coefficients may be significant.
}
\usage{
panel.cor(x, y, digits = 2, prefix = "", cex.cor, ...)
}
\arguments{
  \item{x,y}{
Two variables for which the correlation is required.
}
  \item{digits}{
Number of digits to display.
}
  \item{prefix}{
Text to display in front of the value.
}
  \item{cex.cor}{
Size of the characters displayed. By default this is proportional to the value of the correlation coefficient.
}
  \item{\dots}{
further graphical parameters.
}
}
\details{
Implement by setting panel arguments when calling the scatterplot function: see examples.
Note that providing values for digits, prefix or cex.cor causes \code{pairs} to produce many warnings that these are not graphical parameters.
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

pairs(trees, panel=panel.cor)

pairs(trees, lower.panel=panel.cor, prefix="|r|=")
  # Produces 36 warnings.

}
