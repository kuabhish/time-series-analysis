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
fast.klr.sub <-
function(X,y,s.size=10000,n.samp=10,C.vec=10^(-3:3),eps=0.001,verbose=F) {
	n <- length(y)
	nn <- sum(y>0)
	na <- sum(y<0)
	best.model <- c()
	best.score <- -Inf
	nom.size <- ceiling(s.size*nn/n)
	anom.size <- s.size-nom.size
	for (i in 1:n.samp) print.time.if.verbose(paste0('Testing sample: ',i),verbose,{
		nom.samp <- sample(nn,min(nn,3*nom.size))
		anom.samp <- sample(na,min(na,3*anom.size))
		samp.mask <- c(which(y>0)[nom.samp[1:nom.size]],which(y<0)[anom.samp[1:anom.size]])
		train.data <- X[samp.mask,]
		train.label <- y[samp.mask]
		valid.mask <- c(which(y>0)[nom.samp[(nom.size+1):length(nom.samp)]],which(y<0)[anom.samp[(anom.size+1):length(anom.samp)]])
		test.data <- X[valid.mask,]
		test.label <- y[valid.mask]
		best.local.model <- c()
		best.local.score <- -Inf
		for (C in C.vec) print.time.if.verbose(paste0('Testing C = ',C),verbose,{
			m <- fast.klr.model(train.data,train.label,C=C,eps=eps,verbose=verbose)
			print.time.if.verbose('Scoring ...',verbose,{
				score <- ll.score(m$resp.f(test.data),test.label)
			})
			if (score > best.local.score) {
				if (verbose) print(paste0('New local best: ',score))
				best.local.score <- score
				best.local.model <- m
			}
			rm(m)
		})
		if (best.local.score > best.score) {
			if (verbose) print(paste0('New global best: ',best.local.score))
			best.score <- best.local.score
			best.model <- best.local.model
		}
	})
	best.model
}
