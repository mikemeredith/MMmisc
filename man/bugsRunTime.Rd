\name{bugsRunTime}
\alias{bugsRunTime}
\title{
Run time of WinBUGS called by R2WinBUGS::bugs
}
\description{
Compares the time stamp of the script created before the WinBUGS run starts with the time stamp of the coda files created at the end of the run and calculates the difference.
}
\usage{
bugsRunTime(look.in = NULL)
}
\arguments{
  \item{look.in}{
Directory where WinBUGS scripts and logs are stored. See Details.
}
}
\details{
When bugs starts, it creates a file 'script.txt' in the directory (folder) specified by the 'working.directory' argument, for which the default is a temporary directory accessed with 'tempdir'. When WinBUGS completes the run, it writes the file 'log.txt' to the same directory.

bugsRunTime searches for the 'log.txt' file in the directory(s) specified by look.in (default: first tempdir, then the current working directory). If successful, it prints the time stamp on 'log.txt' and returns the difference between the time stamps on 'log.txt' and 'script.txt'. If they are not found, it returns NA with a message.

You can force bugs.run.time to look in the current working directory rather than tempdir by setting 'look.in=getwd()'. 

You can also use 'look.in=choose.dir()' and browse to the required folder.

If bugs is run with debug=TRUE, log.txt is written as soon as the run is complete, NOT when WinBUGS is closed, so in this case bugs.run.time gives a better indication of actual computing time than system.time(bugs(...))
}
\value{
See details.
}
\author{
Mike Meredith, 15 Oct 2008
}


\seealso{
\code{\link[R2WinBUGS]{bugs}} in the \code{R2WinBUGS} package.
}
\examples{
##
}
