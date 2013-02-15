\name{dumpFunc}
\alias{dumpFunc}
\title{
Writes the code of a function to a .R file.
}
\description{
Writes the code of a function to a .R file.
}
\usage{
dumpFunc(name)
}
\arguments{
  \item{name}{
a character vecter giving the name of the function to write (needs quotes).
}
}
\details{
Writes the function code to a file called "func_<functionName>.R" in the current directory.
Used when a function is to be modified: dump, edit, then source. Unlike 'fix', it leaves a record of changes.
}
\value{
An invisible character vector containing the names of the objects which were dumped. 
}
\author{
Mike Meredith, 27 Nov 2006
}
\seealso{
Also as \code{\link{dump}}.
}
\examples{
dumpFunc("panel.smooth")
# Check the working directory for the file:
Sys.glob("fun_*.R")   #   "fun_panel.smooth.R"

}
