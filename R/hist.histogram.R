hist.histogram <-
function(x,...) {
   if(inherits(x, "histogram")) plot(x,...)
   invisible(x)
}
