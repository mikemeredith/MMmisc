\name{addTo<-}
\alias{addTo<-}
\title{
Increases 'x' by 'value'.
}
\description{
Increases 'x' by 'value'. Equivalent to x <- x + value. 
}
\usage{
addTo(x) <- value
}
\arguments{
  \item{x}{
Object whose value is to be changed.
}
  \item{value}{
value to add to 'x'; can be negative.
}
}
\value{
Returns 'value'.
}
\author{
Charles Dupont authored the inc<- function in Frank Harrell's Hmisc package.
Renamed addTo rather than inc as the latter implies "add 1".
}
\note{
%%  ~~further notes~~
}

\seealso{
See also \code{\link[Hmisc]{inc<-}}.
}
\examples{
x <- 2:6
addTo(x) <- 3
x
addTo(x) <- -2:2
x
}
