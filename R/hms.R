hms <-
function(x, char=FALSE) {
  if(inherits(x, "proc_time"))
     x <- x[3]
  t <- numeric(3)
  t[1] <- x %/% 3600
  x1 <- x %% 3600
  t[2] <- x1 %/% 60
  t[3] <- round(x1 %% 60)
  if(char) {
    th <- if(t[1] > 0) paste(t[1], "hrs ") else NULL
    tm <- if(t[2] > 0) paste(t[2], "mins ") else NULL
    ts <- paste(t[3], "secs") 
    return(paste(th, tm, ts, sep=""))
  } else {
     names(t) <- c("hrs", "mins", "secs")
     return(t)
  }
}
