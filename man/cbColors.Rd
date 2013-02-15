\name{cbColors}
\alias{cbColors}
\docType{data}
\title{
Palette of colorblind colours
}
\description{
Palette of 8 colours that are unambiguous both to colorblinds and non-colorblinds
}
\usage{data(cbColors)}
\format{
  A named character vector of 8 colours in "#FFFFFF" format
}
\source{
http://jfly.iam.u-tokyo.ac.jp/color/#pallet
}
\examples{
# Bar plot with default colours:
barplot(rep(1,10),col=1:10, yaxt="n")

# Set the palette to cbColors
palette(cbColors)
barplot(rep(1,10),col=1:10, yaxt="n")

# Reset the default palette
palette("default")

}
\keyword{datasets}
