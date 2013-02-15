\name{hms}
\alias{hms}
\title{
Converts time in seconds to hrs mins secs.
}
\description{
Converts a time in seconds (or an object of class 'proc_time' as returned
by system.time) to a named vector of hours, minutes, seconds, or, if 
char=TRUE, a character string with time in hrs, mins, secs.
}
\usage{
hms(x, char = FALSE)
}
\arguments{
  \item{x}{
Either a number of seconds, or a proc_time object.
}
  \item{char}{
If TRUE, a character string is returned.
}
}
\value{
If char=TRUE, a character string of the form "1 mins 29 secs" or
"3 hrs 33 mins 9 secs".
If char=FALSE (the default) a named vector with 3 elements: hours, mins and secs
}
\author{
Mike Meredith, 10 Nov 2008
}
\seealso{
See also \code{\link{proc.time}} and \code{\link{system.time}}
}
\examples{
hms(1234)
#   hrs  mins  secs 
#     0    20    34 
hms(1234, char=TRUE)
# "20 mins 34 secs"
hms(12345, char=TRUE)
# "3 hrs 25 mins 45 secs"
}
