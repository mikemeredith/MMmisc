\name{detachSelect}
\alias{detachSelect}
\title{
Remove selected objects from the search path.
}
\description{
Displays a dialog box with a list of items in the search path. Select those  you wish to remove. 
}
\usage{
detachSelect()
}
\value{
A character vector with the names of the objects detached from the search path, or NULL if nothing was detached.

}
\author{
Mike Meredith, 9 May 2007
}
\seealso{
\code{\link{detach}}, \code{\link{search}}.
}
\examples{
\dontrun{
require(splines) # package
attach(trees)	# data frame
mean(Girth)
detachSelect()   # select "package:splines" and "trees" in the dialogue box (use 
            #   Shift-click or Ctrl-click) and press enter.
mean(Girth) # Error in mean(Girth) : object 'Girth' not found
}
}
