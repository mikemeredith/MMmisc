cloglog <-
function(p) {
	if(any(p < 0 | p > 1))
		stop("p must be between 0 and 1")
	log(-log(1-p))
}
