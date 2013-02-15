# Function to interactively place labels on a plot

labelArrow <- function(txt, pos=3, ...) {
  where <- c("above", "right of", "below", "left of")
  cat("Click on the plot", where[pos], "the location for the label\n")
  flush.console()
	arr1 <- locator(1)
	text(arr1$x, arr1$y, txt, pos=pos, ...)
  cat("Now click where the arrow head should be\n")
  flush.console()
	arr2 <- locator(1)
	arrows( arr1$x, arr1$y, arr2$x, arr2$y, length=0.1, ...)
  # Create text for a script:
  s1 <- sprintf("text(%f, %f, '%s', pos=%d)", arr1$x, arr1$y, txt, pos)
  s2 <- sprintf("arrows(%f, %f, %f, %f, length=0.1)", arr1$x, arr1$y, arr2$x, arr2$y)
  cat("Here is code to paste into a script to repeat this:")
  cat("\n   ", s1, "\n   ", s2, "\n")
  invisible(c(arr1, arr2))
}


