\name{panel.boxplot}
\alias{panel.boxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draws boxplots along the diagonal of a scatterplot matrix.
}
\description{
Draws boxplots along the diagonal of a scatterplot matrix, such as produced by 'pairs' or 'plot.data.frame'.
}
\usage{
panel.boxplot(x, ...)
}
\arguments{
  \item{x}{
The vector of data to use to plot the boxplot.
}
  \item{\dots}{
Not used.
}
}
\details{
Implement by setting 'diag.panel = panel.boxplot' when calling the scatterplot
function.
}
\value{
Nothing of interest.
}
\author{
Mike Meredith, 29 May 2012, prompted by Quinn & Keough (2002) p62
}

\seealso{
\code{\link{pairs}}, \code{\link{boxplot}}
}
