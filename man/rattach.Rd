\name{rattach}
\alias{rattach}
\title{
Smart alternative to 'attach'
}
\description{
Checks if object of the same name is already attached and detaches it. Also
checks for objects in .GlobalEnv which will mask items in the attachment.
}
\usage{
rattach(db, pos = 2, overwrite = NA, ...)
}
\arguments{
  \item{db}{
Name of object to attach
}
  \item{pos}{
Where in the search list to insert
}
  \item{overwrite}{
If TRUE, objects in .GlobalEnv which would mask items in db are deleted. If
NA, a dialogue box pops up for the user to decide whether to delete them or not.
}
  \item{\dots}{
Additional arguments passed to attach.
}
}
\details{
See help on attach for more details
}
\value{
Nothing of interest, used for its side effect.
}

\author{
Mike Meredith, 23 Sept 08, inspired by features of R2WinBUGS::attach.bugs
}

\seealso{
See also \code{\link{attach}}.
}
\examples{
data(trees)
names(trees)
rattach(trees)
Girth
## trees$Girth is actually the diameter in inches;
## correct it:
trees$Girth <- Girth * pi
Girth	# still the old values
rattach(trees)
Girth	# now the corrected values
detach(trees)
}

