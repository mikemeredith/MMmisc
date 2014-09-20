\name{labelArrow}
\alias{labelArrow}
\title{
Interactively place labels on a plot
}
\description{
Interactively position labels and associated arrows on a plot and return code to reproduce the effect.
}
\usage{
labelArrow(txt, pos = 3, ...)
}
\arguments{
  \item{txt}{
The text of the label.
}
  \item{pos}{
the position of the label with respect to the end of the arrow: values of 1, 2, 3 and 4, mean left of, above and to the right. See \code{\link{text}}.
}
  \item{\dots}{
further graphical parameters.
}
}
\details{
When the function is run, it calls \code{\link{locator}} to get the position of 2 mouse-clicks on the graphics window. The first determines the position of the label and the arrow tail, the second the position of the arrow head.

Code to reproduce the label and arrow is displayed in the Console; this can be cut-pasted into a script for future use.
}
\value{
Returns invisibly a list with the coordinates of the two points selected.
}
\author{
Mike Meredith
}

\seealso{
\code{\link{locator}}.
}
\examples{
\dontrun{
plot(Volume ~ Girth, data=trees)
labelArrow("Largest tree")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
