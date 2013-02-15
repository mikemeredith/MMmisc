Rank <-
function(x, na.last = TRUE,
      ties.method = c("average", "first", "random", "max", "min"),
      tolerance=.Machine$double.eps ^ 0.5) 
   rank(round(x, -floor(log10(tolerance))), na.last=na.last,
      ties.method=ties.method)
