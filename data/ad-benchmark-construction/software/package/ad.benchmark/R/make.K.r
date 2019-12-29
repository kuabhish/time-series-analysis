#####
#   Copyright 2016 Andrew Emmott (emmotta@oregonstate.edu)
#
#   This work is licensed under the Creative Commons Attribution License
#   (CC BY) version 4.0. A copy of the license should be included with this
#   file, but can also be viewed at: 
#
#   	http://creativecommons.org/licenses/by/4.0/legalcode.txt
#
#   While this work includes software and Creative Commons Licenses are not
#   typically used for software, this was deemed the easiest path and it
#   gets at what is most important to us: credit where credit is due.
#
#   In short:
#
#	If you publish material based on this software, benchmarks constructed
#	from this software, or from the any of the benchmarks in the corpus that
#	accompanies this software, please cite our academic paper found at:
#
#		http://ir.library.oregonstate.edu/xmlui/handle/1957/59114
#
#	If you otherwise use this software, benchmarks constructed from this
#	software, or any of the benchmarks in the corpus that accompanies this
#	software, please refer to the above URI where appropriate.
#####
make.K <-
function(X) {
	D <- dist(X)
	K <- as.matrix(D)^2
	diag(K) <- Inf
	sigma.cans <- apply(K,2,min)
	sigma.squared <- median(sigma.cans[sigma.cans!=0])
	diag(K) <- 0
	dist.f <- function(d) exp(-d/(2*sigma.squared))
	k.f <- function(x,y) {
		t(if (!is.null(dim(x))) {
			apply(x,1,function(a) k.f(a,y))
		} else if (!is.null(dim(y))) {
			apply(y,1,function(b) k.f(x,b))
		} else {
			dist.f(sum((x-y)^2))
		})
	}
	K <- dist.f(K)
	named.list(K,sigma.squared,dist.f,k.f)
}
