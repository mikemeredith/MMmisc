# Function to convert time in seconds or a 'proc_time' object
#   to hours, min, secs.
# x is a vector of times in seconds, or a 'proc_time' object.
# Returns:
#  If char == 0, a matrix with named columns for hours, mins, secs
#  If char == 1, a vector of strings of the form "6 mins 20 secs"
#  If char == 2, a vector of strings of the form "00:06:20"

hms <- function(x, char=0) {
  if(inherits(x, "proc_time"))
     x <- x[3]
  x1 <- x %% 3600
  t <- cbind(hrs=x %/% 3600, mins=x1 %/% 60, secs=round(x1 %% 60))
  if(char==1) {
    tmp <- sprintf("%d hrs %d mins %d secs", t[, 1], t[, 2], t[, 3])
    start <- ifelse(t[, 1] == 0, 
                ifelse(t[, 2] == 0, 14, 7), 1)
    out <- substr(tmp, start, 30)
  } else  if (char==2) {
    out <- sprintf("%02d:%02d:%02d", t[, 1], t[, 2], t[, 3])
  } else {
     out <- t
  }
  return(out)
}
