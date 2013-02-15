\name{round.data.frame}
\alias{round.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A 'round' method for data frames. 
}
\description{
Numeric elements in the data frame are rounded to the number of decimal places given by 'digits'; non-numeric elements are not changed.
}
\usage{
\method{round}{data.frame}(x, digits = 0)
}
\arguments{
  \item{x}{
a data frame
}
  \item{digits}{
number of digits for rounding
}
}
\value{
A data frame with numeric values rounded.
}

\author{
Mike Meredith, 4 Sept 2007
}

\seealso{
\code{\link{round}}
}
\examples{
x <- data.frame(a=runif(5, 0, 4), b=1:5, c=LETTERS[1:5])
x
round(x, 2)
}
