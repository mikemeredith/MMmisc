dispersion <-
function (x, grps, data = parent.frame(), method = "dispersion", jitter = 0.1, offset = 1/3, 
    vertical = TRUE, group.names, add = FALSE, at = NULL, datlim = NULL, 
    grplim = NULL, main = NULL, datlab = NULL, grplab = NULL, log = FALSE, pch=18, 
    col = par("fg"), cex = 1, line=TRUE, ...) 
{
   # Rescue names:
    xname <- grpsname <- dataname <- ""
    xname <- deparse(substitute(x))
    if(!missing(grps))
        grpsname <- deparse(substitute(grps))
    if(!missing(data))
        dataname <- deparse(substitute(data))

   # Sort out method:
    method <- pmatch(method, c("overplot","jitter","stack","dispersion"))[1]
    if (is.na(method) || method == 0) 
        stop("invalid plotting method")

   # Turn 'x' and 'grps' into a list of 'groups':
    groups <- if (inherits(x, "formula") && length(x) == 3) {
        if(is.null(datlab)) datlab <- x[[2]]
        if(is.null(grplab)) grplab <- x[[3]]
        if(is.null(main)) main <- dataname
        groups <- eval(x[[3]], data)
        x <- eval(x[[2]], data)
        split(x, groups)
    } else if (!missing(grps) && is.factor(grps)) {
        split(x,grps)
    } else if (is.list(x)) {
        x
    } else if (is.numeric(x)) 
        list(x)
    if(is.null(datlab)) datlab <- xname
    if(is.null(grplab)) grplab <- grpsname
    if(is.null(main)) main <- dataname

    if (0 == (n <- length(groups))) 
        stop("invalid first argument")
    if (!missing(group.names)) 
        attr(groups, "names") <- group.names
    else if (is.null(attr(groups, "names"))) 
        attr(groups, "names") <- 1:n

   # Fix the 'at' argument:
    if (is.null(at)) 
        at <- 1:n
    else if (length(at) != n) 
        stop(gettextf("'at' must have length equal to the number %d of groups", 
            n), domain = NA)

   # Draw the box and axes:
    if (!add) {
        if(is.null(datlim)) {
            datlim <- c(NA, NA)  # limits for data axis
            for (i in groups)
                datlim <- range(datlim, i[is.finite(i)], na.rm = TRUE)
        }
        if(is.null(grplim)) {
           grplim <- c(1, n)    # limits for groups axis
           if (method == 2) {                         # jitter
               grplim <- grplim + jitter * if (n == 1) c(-5, 5) else c(-2, 2)
           } else if (method == 3) {                  # stacked
               grplim <- grplim + if (n == 1) c(-1, 1) else c(0, 0.5)
           } else if (method == 4) {                  # dispersion
               grplim <- grplim + if (n == 1) c(-1, 1) else c(-0.5, 0.5)
           }
        }
        xlim <- if (vertical) grplim else datlim
        ylim <- if (vertical) datlim else grplim
        lg <- if(log) {
           if(vertical) "y" else "x"
        } else ""
        plot(xlim, ylim, type = "n", ann = FALSE, axes = FALSE, log = lg,...)
        box(bty="l")
        if (vertical) {
            if (n > 1) 
                axis(1, at = at, labels = names(groups),...)
            axis(2)
            title(main=main, xlab = grplab, ylab = datlab)
        }
        else {
            axis(1)
            if (n > 1) 
                axis(2, at = at, labels = names(groups),...)
            title(main=main, xlab = datlab, ylab = grplab)
        }
    }
    # Get info on size of the plotting character in units used on x and y axes:
    csize <- cex * if (vertical)   # Character size, used to calculate offset for stacking
        xinch(par("cin")[2])
    else yinch(par("cin")[2])
    if(!log) {
        cspace <- 0.5 * cex * if (vertical)  # Character spacing, used for deciding ties in data
            yinch(par("cin")[1])
        else xinch(par("cin")[1])
    }

    # Fix data for plotting points:
    for (i in 1:n) {
        x <- groups[[i]]                     # 'x' is the data, 'y' is the group position.
        y <- rep.int(at[i], length(x))
        if (method == 2) {                              # jitter
            y <- y + runif(length(y), -jitter, jitter)
        } else if (method >2 ) {                        # stacked or dispersion...
            if(method == 3) {                           # ...with different functions for each
                f <- function(x) seq(length = length(x))-1
            } else f <- function(x) { t <- (length(x)-1)/2 ; seq(-t,t,by=1) }
    # Turn into character-spacing units, round, and turn back:
            if(!log)
                x <- cspace * round(x / cspace)
            xg <- split(x, factor(x))           # a list grouping tied values together
            xo <- lapply(xg, f)                 # a similar list with 1:length...
            x <- unlist(xg, use.names = FALSE)  # reconstitute a vector, but now with tied values grouped
            y <- rep.int(at[i],length(x)) + (unlist(xo,use.names = FALSE))*offset*csize
        }
        if (vertical) {
				if(line)
					abline(v=at, col='grey')
            points(y, x, col = col[(i - 1)%%length(col) + 1], 
                pch = pch[(i - 1)%%length(pch) + 1], cex = cex, xpd=NA,...)
		
        } else {
				if(line)
					abline(h=at, col='grey')
				points(x, y, col = col[(i - 1)%%length(col) + 1], 
                pch = pch[(i - 1)%%length(pch) + 1], cex = cex, xpd=NA,...)
			}
    }
    invisible(groups)
}
