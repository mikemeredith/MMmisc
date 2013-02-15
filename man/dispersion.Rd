\name{dispersion}
\alias{dispersion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A smart alternative to \code{stripchart}.
}
\description{
  Plots a "dispersion graph", a variation of stripchart, with the stacked points distributed either side of the line. 
}
\usage{
dispersion(x, grps, data = parent.frame(), method = "dispersion", jitter = 0.1, offset = 1/3, vertical = TRUE, group.names, add = FALSE, at = NULL, datlim = NULL, grplim = NULL, main = NULL, datlab = NULL, grplab = NULL, log = FALSE, pch = 18, col = par("fg"), cex = 1, line=TRUE, ...)
}
\arguments{
  \item{x}{
a formula, list of grouped data, or a vector of values.
}
  \item{grps}{
if 'x' is a vector, an optional factor determining how the data should be grouped.
}
  \item{data}{
if 'x' is a formula, a data frame containing the variables.
}
  \item{method}{
one of "dispersion", "stacked", "jitter", or "overplot" (or abbreviations); see code{stripchart} for details.
}
  \item{jitter, offset}{
control jittering and stacking; see code{stripchart}.
}
  \item{vertical}{
if TRUE, data are on the vertical axis, otherwise horizontal.
}
  \item{group.names}{
an optional vector of names for the groups.
}
  \item{add}{
if TRUE, results will be added to an existing plot.
}
  \item{at}{
specifies the positions to plot groups; only meaningful when adding to an existing plot.
}
  \item{datlim}{
limits for the data axis; if vertical=TRUE, this will be the vertical axis.
}
  \item{grplim}{
limits for the groups axis.
}
  \item{main, datlab, grplab}{
plot title and names for the data and group axes.
}
  \item{log}{
if TRUE, the data will be plotted on a log scale (if you do this, points may overlap, so an open plotting character is recommended).
}
  \item{pch, col, cex}{
the plotting character type, colour and size.
}
  \item{line}{
logical; if TRUE, a grey line is drawn through the points.
}
  \item{\dots}{
additional plotting parameters.
}
}
\value{
   Returns (invisibly) a list with the values for the groups plotted.
}
\references{
  Magnusson, W E; G Mourano. 2004. \emph{Statistics without math}. Sinauer,
   Sunderland MA. pp28-30
}
\author{
   Mike Meredith, 3 Dec 2006, based on \code{stripchart} in package stats.
}

\seealso{
\code{\link{stripchart}}.
}
\examples{
dispersion(decrease ~ treatment,
    main = "dispersion(OrchardSprays)", 
    vertical = FALSE, log = TRUE, data = OrchardSprays)
}
