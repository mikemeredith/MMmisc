
# Calculate the area of a polygon

# This is Andy Royle's function from scrbook package

# Arguments:
#   x : a vector of coordinates OR a 2-column matrix of coordinates OR
#         a data frame with columns 'x' and 'y' containing the coordinates.
#   y : if x is a vector, the corresponding vector of coordinates
#
# Value:
#   the area of the polygon

getArea <- function (x, y = NULL) 
{
    if (is.null(y)) {
        if (is.matrix(x) && ncol(x) == 2) {
            y <- x[, 2]
            x <- x[, 1]
        }
        else if (!is.null(x$x) && !is.null(x$y)) {
            y <- x$y
            x <- x$x
        }
    }
    x <- c(x, x[1])
    y <- c(y, y[1])
    i <- 2:length(x)
    return(0.5 * sum(x[i] * y[i - 1] - x[i - 1] * y[i]))
}

