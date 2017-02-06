
###########  panel functions for pairs plots (scatterplot matrices)  ######

# NB: panel.smooth is in the graphics package.
# See also MMmisc::scatterSmooth for a stand-alone plot.

# Draw histograms and density curves along the diagonal of a 
#   scatterplot matrix; see 'pairs'
panel.hist <- function(x, ...) {
  oldusr <- par("usr"); on.exit(par(oldusr))
  par(usr = c(oldusr[1:2], 0, 1.5) )
  nB <- nclass.Sturges(x) + 1  # number of breaks = 1 + number of classes
  brks <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length=nB)
  h <- hist(x, breaks=brks, plot = FALSE)
  y <- h$density/max(h$density)
  rect(brks[-nB], 0, brks[-1], y, col="cyan", ...) 
  den <- density(x, na.rm=TRUE)
  lines(den$x, den$y/max(h$density), col='red')
} 
# -----------------------------------

# Display the correlation coefficient in the panel of a scatterplot matrix; see pairs
# Based on code in pairs help file. Significance colours add by MM 29 Aug 2012
panel.cor <- function(x, y, digits=2, prefix="", cex.cor,...) {
  ok <- is.finite(x) & is.finite(y)
  if (length(ok) > 2)  {
    oldusr <- par("usr"); on.exit(par(oldusr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x[ok], y[ok]))
    dfr <- length(x[ok]) - 2
    Fstat <- r^2 * dfr / (1 - r^2)
    pval <- pf(Fstat, 1, dfr, lower.tail=FALSE)
    sig <- which(c(0.001, 0.01, 0.05, 0.1, 1) > pval)[1]
    rect(0,0,1,1,       # modified 2012-08-29
      col=c('orange', 'gold','yellow', 'lightyellow', 'white')[sig])
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
  }     
}
# ------------------------------
# Display the estimated bivariate mean and its 50% confidence region in the panel
#	 of a scatterplot matrix; see pairs
# By MM, added 25 Aug 2012
panel.ellipse <- function(x,y, col.fill = "pink", level=0.5, ...) {
	ok <- is.finite(x) & is.finite(y)
	if(any(ok) && require(ellipse, quietly=TRUE, warn.conflicts=FALSE))  {
    elps <- ellipse::ellipse(cor(x[ok], y[ok]), scale = c(sd(x[ok]),sd(y[ok])),
			centre = c(mean(x[ok]), mean(y[ok])), level=level)
		polygon(elps, col=col.fill) # Do the fill for the ellipse
    points(x,y)                 # Plot the points; may obscure ellipse.
    polygon(elps, border='red') # Do the outline
		points(mean(x[ok]), mean(y[ok]), pch=3, col='red', cex=2,...)
		box()
  } else {
    points(x,y)
  }
}
# ------------------------------
# Produces a scatterplot, adding jittering to variables which have <5 values.
panel.jitter <- function(x, y, ...) {
   jit.in <- function(x) {
      t <- jitter(x)
      if(length(unique(x)) > 1) {
         t <- ifelse(t < min(x), 2*min(x) - t, t)
         t <- ifelse(t > max(x), 2*max(x) - t, t)
      }
      return(t)
   }

   if(length(unique(x)) < 5)       x <- jit.in(x)
   if(length(unique(y)) < 5)       y <- jit.in(y)
   points(x, y, ...)
}
# ------------------------------
# Draws boxplots on the diagonal
# Added by MM, 29 May 2012, prompted by Quinn & Keough (2002) p62
panel.boxplot <- function(x, ...) {
  oldusr <- par("usr"); on.exit(par(oldusr))
  par(usr = c(oldusr[1:2], 0, 1.5) )
  bx <- boxplot.stats(x, do.conf = FALSE)
  arrows(bx$stats[1], 0.5, bx$stats[5], 0.5, angle=90, length=0.1, code=3)
  rect(bx$stats[2], 0.3, bx$stats[4], 0.7, col="wheat", ...) 
	segments(bx$stats[3], 0.3, y1=0.7, lwd=3, lend=1)
	if(n <- length(bx$out))
		points(bx$out, rep(0.5, n), pch=1)
	box()
} 
