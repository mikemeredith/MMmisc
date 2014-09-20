\name{hms}
\alias{hms}
\title{
Converts time in seconds to hrs mins secs.
}
\description{
Converts a time in seconds (or an object of class 'proc_time' as returned
by system.time) to a matrix of hours, minutes, seconds, or, if 
\code{char > 0}, a character string with time in hrs, mins, secs.
}
\usage{
hms(x, char = 0)
}
\arguments{
  \item{x}{
Either a vector of times in seconds, or a (single) proc_time object.
}
  \item{char}{
Controls the type of output; see Value.
}
}
\value{
If char == 0, a matrix with named columns for hours, mins, secs; the matrix may have only one row.

If char == 1, a vector of strings of the form "6 mins 20 secs".

If char == 2, a vector of strings of the form "00:06:20".
}
\author{
Mike Meredith, 10 Nov 2008
}
\seealso{
See also \code{\link{proc.time}} and \code{\link{system.time}}
}
\examples{
hms(1234)
#      hrs  mins  secs 
#[1]     0    20    34 
hms(1234, char=1)
# "20 mins 34 secs"
hms(12345, char=2)
# "03:25:45"
}
