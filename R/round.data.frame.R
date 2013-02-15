round.data.frame <-
function(x, digits=0) {
   stopifnot(is.data.frame(x))
   stopifnot(is.numeric(digits))
   digits <- round(digits)
   for(i in 1:ncol(x))
      if(is.numeric(x[[i]]))
         x[[i]] <- round(x[[i]], digits=digits)
   return(x)
}
