\name{lsSizes}
\alias{lsSizes}
\title{
List object sizes
}
\description{
Produces a data frame of objects in the specified environment (default is the
Global environment) together with approximate object sizes in Kb, sorted by
size.
}
\usage{
lsSizes(name = 1)
}
\arguments{
  \item{name}{
which environment to use in listing the available objects. Defaults to the
current environment. Can be an integer (the position in the search list); 
the character string name of an element in the search list; or an explicit
environment. See the help on ls.
}
}
\value{
A data frame with one column, the approximate size of each object in kilobytes,
sorted by size (largest first). Row names are the object names.
}
\author{
Mike Meredith 19 July 2011
}

\seealso{
See also \code{\link{ls}}.
}
\examples{
require(stats)
lsSizes("package:stats")
}
