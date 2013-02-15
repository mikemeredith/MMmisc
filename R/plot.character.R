plot.character <-
function (x, align="center", ...) 
{
   if (!is.character(x)) 
        stop("Argument <x> must be a character variable.\n")
   if(length(x) > 1)
      x <- paste(x, collapse="\n")
   align <- tolower(substr(align, 1, 1))
   old.mai <- par(mai=rep(0,4)) ; on.exit(par(old.mai))
   plot.new()
   if(align=="l") {
      text(0.01, 0.5, label = x, pos=4)
   } else if(align=="r") {
      text(0.99, 0.5, label = x, pos=2)
   } else
      text(0.5, 0.5, label = x)
}
