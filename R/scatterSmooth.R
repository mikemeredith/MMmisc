# Identical to stats::scatter.smooth except that:
# 1. it allows you to specify the colour, width and line type of the lowess line; 
#  	defaults to col="red", lwd=1, lty=1. 
# 2. avoids warnings from the loess.smooth function.
# 3. with add = TRUE, it allows a loess line to be added to an existing scatterplot.

# This version allows addition of CI to loess lines (11 Sept 2012)
# "Identical to stats::scatter.smooth except..." is now misleading.

scatterSmooth <-
function (x, y = NULL, data = parent.frame(),
    span = 0.75, degree = 2, family = "gaussian",  # defaults changed 11 Sept 2012
    xlab = NULL, ylab = NULL, ylim,
    evaluation = 50, add = FALSE, CI = NULL,
    col.smooth="red", lwd.smooth=1, lty.smooth=1, ...) 
{
  xlabel <- if (!missing(x)) 
    deparse(substitute(x))
  ylabel <- if (!missing(y)) 
    deparse(substitute(y))
  if(is.language(x))  {   # Making it work with formula added 7 Sep 2012
    xy <- model.frame(x, data=data)[, 2:1]
    if(is.null(xlab))
      xlab <- names(xy)[1]
    if(is.null(ylab))
      ylab <- names(xy)[2]
    names(xy) <- c("x", "y")
  } else {
    xy <- xy.coords(x, y, xlabel, ylabel)
    if (is.null(xlab)) 
      xlab <- xy$xlab
    if (is.null(ylab)) 
      ylab <- xy$ylab
  }
  #TODO: deal with NAs
  fit <- loess(y ~ x, data=xy, span = span, degree=degree, 
      family = family, method = "loess")#, ...)
  newx <- seq(min(xy$x), max(xy$x), length=evaluation)
  pred <- predict(fit, data.frame(x = newx), se = !is.null(CI))
  newy <- if(is.list(pred)) pred$fit else pred
  if(missing(ylim))
    ylim <- range(xy$y, newy)
  if(!is.null(CI))  {
    if(CI > 1)
      CI <- CI/100
    crit <- qnorm((1-CI)/2, lower.tail=FALSE)
    upp <- newy + crit*pred$se.fit
    low <- newy - crit*pred$se.fit
    ylim <- range(xy$y, upp, low)
  }
  if(!add)
    plot(x=xy$x, y=xy$y, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  lines(newx, newy, col=col.smooth, lty=lty.smooth, lwd=lwd.smooth)
  if(!is.null(CI))  {
    lines(newx, low, col=col.smooth, lty=2)
    lines(newx, upp, col=col.smooth, lty=2)
  }
  invisible()
}
