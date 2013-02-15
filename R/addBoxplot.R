addBoxplot <-
function(x, gap=0.01, side, outpch=20)	{
	if(missing(side)) 
		stop("Argument 'side' is missing with no default")
	if(missing(x))
		stop("Argument 'x' is missing with no default")
		
	usr <- par("usr")	# Coordinates of the edges of the plot
	stats <- boxplot.stats(x)$stats	# Stats to plot
	outs <- boxplot.stats(x)$out	# outliers
	
	if(side==1 || side==3) {	# Bottom or top
		posy <- ifelse(side==1, usr[3], usr[4]) + 
			(usr[4]-usr[3])*gap
		arrows(stats[1], posy, stats[5],	# whiskers
			length=0.05, angle=90, code=3, lty=2, xpd=TRUE)
		segments(stats[2], posy, stats[4],		# box
			lwd=3, lend=2, xpd=TRUE)
		points(stats[3], posy, col='white', pch=20, xpd=TRUE)	# median
		if(!is.null(outs))
			points(outs, rep(posy, length(outs)),	# outliers
				pch=outpch, xpd=TRUE)
	}
	if(side==2 || side==4) {	# left or right
		posx <- ifelse(side==2, usr[1], usr[2]) + 
			(usr[2]-usr[1])*gap
		arrows(posx, stats[1], posx, stats[5],	# whiskers
			length=0.05, angle=90, code=3, lty=2, xpd=TRUE)
		segments(posx, stats[2], posx, stats[4],	# box
			lwd=3, lend=2, xpd=TRUE)
		points(posx, stats[3], col='white', pch=20, xpd=TRUE)	# median
		if(!is.null(outs))
			points(rep(posx, length(outs)), outs,	# outliers
				pch=outpch, xpd=TRUE)
	}
}
