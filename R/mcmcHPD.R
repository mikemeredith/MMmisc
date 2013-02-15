mcmcHPD <-
function(x, ci=0.95) {
   n <- length(x)
   x <- sort(x)
   excl <- floor(n * (1-ci))     # Number of values to exclude
   low.poss <- x[1:excl]         # Possible lower limits...
   upp.poss <- x[(n-excl+1):n]   # ... and corresponding upper limits
   best <- which.min(upp.poss - low.poss)      # Combination giving the narrowest interval
   c(lower = low.poss[best], upper = upp.poss[best])
}
