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
fast.klr.cv <-
function(X,y,n.folds=5,C.vec=10^(-3:3),eps=0.001,verbose=F) {
	n <- length(y)
	nn <- sum(y>0)
	na <- sum(y<0)
	fold.size <- ceiling(n/n.folds)
	nom.size <- ceiling(fold.size*nn/n)
	anom.size <- fold.size-nom.size
	nom.samp <- sample(nn)
	anom.samp <- sample(na)
	if (length(C.vec)>1) {
		if (n.folds>1) {
			C.score <- matrix(-Inf,ncol=length(C.vec),nrow=n.folds)
		} else {
			mdls <- list()
			C.score <- rep(-Inf,length(C.vec))
		}
		for (i in 1:length(C.vec)) {
			print.time.if.verbose(paste0('Testing C = ',C.vec[i]),verbose,{
				if (n.folds>1) {
					for (j in 1:n.folds) {
						print.time.if.verbose(paste0('Fold #',j),verbose,{
							fold.mask <- c(which(y>0)[((j-1)*nom.size+1):min(nn,j*nom.size)],which(y<0)[((j-1)*anom.size+1):min(na,j*anom.size)])
							m <- fast.klr.model(X[-fold.mask,],y[-fold.mask],
												C=C.vec[i],eps=eps,verbose=verbose)
							print.time.if.verbose('Scoring ...',verbose,{
								C.score[j,i] <- ll.score(m$resp.f(X[fold.mask,]),y[fold.mask])
								print(paste0('ll.score = ',C.score[j,i]))
							})
						})
					}
				} else {
					mdls[[i]] <- fast.klr.model(X,y,C=C.vec[i],eps=eps,verbose=verbose)
					print.time.if.verbose('Scoring ...',verbose,{
						C.score[i] <- ll.score(mdls[[1]]$trn.response,y)
						print(paste0('ll.score = ',C.score[i]))
					})
				}
			})
		}
		if (n.folds>1) {
			C <- C.vec[which.max(apply(C.score,2,sum))]
		}
	} else {
		C <- C.vec[1]
	}
	if (n.folds>1 | length(C.vec)<2) {
		fast.klr.model(X,y,C,eps,verbose)
	} else {
		mdls[[which.max(C.score)]]
	}
}
