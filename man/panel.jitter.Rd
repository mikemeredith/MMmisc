\name{panel.jitter}
\alias{panel.jitter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add jittering when plotting a scatterplot matrix.
}
\description{
Produces a scatterplot from 2 variables, but adds jittering to variables
which have <5 values.
}
\usage{
panel.jitter(x, y, ...)
}
\arguments{
  \item{x,y}{
Two vectors of values to plot.
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
None
}
\author{
Code from the pairs help file.
}
\seealso{
\code{\link{pairs}}
}
