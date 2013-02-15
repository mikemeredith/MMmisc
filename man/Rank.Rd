\name{Rank}
\alias{Rank}
\title{
An alternative to 'rank' which behaves like 'all.equal'.
}
\description{
Returns the sample ranks of the values in a vector. Values are treated as ties if difference < tolerance.
}
\usage{
Rank(x, na.last = TRUE, ties.method = c("average", "first", "random", "max", "min"), tolerance = .Machine$double.eps^0.5)
}
\arguments{
  \item{x}{
a numeric, complex, character or logical vector.
}
  \item{na.last}{
for controlling the treatment of NAs. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed; if "keep" they are kept with rank NA.
}
  \item{ties.method}{
a character string specifying how ties are treated, see rank; can be abbreviated.
}
  \item{tolerance}{
numeric = 0. Differences smaller than tolerance are not considered.
}
}
\details{
The function 'rank' (package 'base') only recognizes ties if values are
*exactly* equal, ie. it behaves like '==' instead of 'all.equal'. 'Rank' behaves like 'all.equal', recognizing values as ties if the difference
is less than 'tolerance' (or thereabouts!)
}
\value{
A numeric vector of the same length as x with names copied from x (unless na.last = NA, when missing values are removed). The vector is of integer type unless ties.method = "average" when it is of double type (whether or not there are any ties). 
}

\author{
Mike Meredith, 7 Sept 2007
}

\seealso{
\code{\link{rank}}, \code{\link{all.equal}}
}
\examples{
(x <- c(3.93-2.13, 1.97-0.17))	#  1.8 1.8
x[1] == x[2]	# not EXACTLY equal
rank(x)	# 2 1
all.equal(x[1], x[2])	# but difference < tolerance
Rank(x)		#  1.5 1.5
}
