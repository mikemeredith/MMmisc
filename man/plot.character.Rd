\name{plot.character}
\alias{plot.character}
\title{
A plot method for objects of class 'character'.
}
\description{
A plot method for objects of class 'character'.
}
\usage{
\method{plot}{character}(x, align = "center", ...)
}
\arguments{
  \item{x}{
A character object to plot. If it is a vector of strings, each will appear on a separate line.
}
  \item{align}{
control alignment of text: may be 'left' or 'right' (or just 'l' or 'r'), the default is to center the text.
}
  \item{\dots}{
further graphical parameters
}
}

\value{
None.
}
\author{
Mike Meredith, 6 July 2007
}

\seealso{
\code{\link{plot.default}}
}
\examples{
plot("This is a test")
}

