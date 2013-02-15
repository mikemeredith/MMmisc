\name{rmSelect}
\alias{rmSelect}
\title{
Remove (delete) objects from the Global environment.
}
\description{
Displays a dialog box with a list of objects in the Global environment. Select those  you wish to remove. 
}
\usage{
rmSelect()
}
\value{
A character vector with the names of the objects deleted, or NULL if nothing was deleted.
}
\author{
Mike Meredith, 27 Nov 06
}
\seealso{
\code{\link{rm}}.
}
\examples{
\dontrun{
delete.me <- 5
me.too <- "foobar"
rmSelect()	# select "delete.me" and "me.too" in the dialogue box (use 
            #   Shift-click or Ctrl-click) and press enter.
delete.me   # Error: object 'delete.me' not found
}
}
