\name{leaf.colors}
\alias{leaf.colors}
\title{
Create a colour palette varying from dark green through yellow to white
}
\description{
An alternative to \code{\link{heat.colors}} or \code{\link{terrain.colors}}.
}
\usage{
leaf.colors(n, alpha = 1)
}
\arguments{
  \item{n}{
the number of colors (= 1) to be in the palette.
}
  \item{alpha}{
the alpha transparency, a number in [0,1], see argument alpha in hsv.
}
}
\value{
A character vector of color names. 
}
\author{
Mike Meredith
}

\seealso{
\code{\link{heat.colors}}
}
\examples{
op <- par(mfrow=1:2)
image(t(volcano)[ncol(volcano):1,], col = heat.colors(100))
image(t(volcano)[ncol(volcano):1,], col = leaf.colors(100))
image(t(volcano)[ncol(volcano):1,], col = terrain.colors(100))
image(t(volcano)[ncol(volcano):1,], col = rev(leaf.colors(100)))
par(op)
}

