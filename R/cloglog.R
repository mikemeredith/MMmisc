# Function to calculate the complimentary log log (cloglog) function
#   and its inverse.

cloglog <-
function(p) {
	if(any(p < 0 | p > 1))
		stop("p must be between 0 and 1")
	log(-log(1-p))
}


invcloglog <-
function(x)  { 1 - exp(-exp(x)) }
