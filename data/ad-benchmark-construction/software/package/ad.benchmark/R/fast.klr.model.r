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
fast.klr.model <-
function(X,y,C=1,eps=0.001,verbose=F) {
	n <- length(y)
	cw <- class.weights(y)
	cw <- cw/min(cw)
	a <- rep(C/n,n)
	a.min <- 1-mach.omg()
	a.max <- C*mach.omg()
	print.time.if.verbose('Computing K',verbose,{
			K.meta <- make.K(X)
	})
	K <- K.meta$K
	print.time.if.verbose('Computing initial F and H',verbose,{
		F <- apply(a*y*K,2,sum)
		H <- cw*(F + y*G_(a/C))
	})
	i <- which.max(H)
	j <- which.min(H)
	H_i <- H[i]
	H_j <- H[j]
	a_i <- a[i]
	a_j <- a[j]
	diff <- H_i-H_j
	bigeps <- max(diff*eps,eps)
	iter <- 1
	while(diff > 2*eps) {
		if (iter >= 2000) {
			bigeps <- bigeps*2
			iter <- 1000
		}
		if (iter >= 1000) {
			eps <- bigeps
			iter <- iter + 1
		} else {
			iter <- iter + 1
		}
		print.time.if.verbose(paste0('H_max - H_min = ',diff),verbose,{
			t_ <- Inf
			t <- 0
			t.iter <- 1
			while (diff > 2*eps & t.iter < 100) {
				t_ <- t
				if (verbose) print(paste0('t = ',t))
				H_i_ <- H_i
				H_j_ <- H_j
				a_i_ <- a_i
				a_j_ <- a_j
				t <- t_-(1/(phi__(nu(K,i,j),a_i_,a_j_,C)))*diff
				a_i <- a[i]+t*y[i]
				if (a_i < a.min | a_i > a.max) {
					H_i <- NA
					a_i <- min(a.max,max(a.min,a_i))
					if (verbose) print(paste0('Excluding point ',i))
					t <- (a_i-a[i])*y[i]
				}
				a_j <- a[j]-t*y[j]
				if (a_j < a.min | a_j > a.max) {
					H_j <- NA
					a_j <- min(a.max,max(a.min,a_j))
					if (verbose) print(paste0('Excluding point ',j))
					t__ <- (a[j]-a_j)*y[j]
					if (abs(t) > abs(t__)) t <- t__
					a_i <- a[i]+t*y[i]
				}
				if (is.na(H_i) | is.na(H_j)) break
				H_i <- 	H_i_ + cw[i]*(
						y[i]*(a_i-a_i_)*K[i,i] +
						y[j]*(a_j-a_j_)*K[i,j] +
						y[i]*(G_(a_i/C)-G_(a_i_/C)))
				H_j <- 	H_j_ + cw[j]*(
						y[i]*(a_i-a_i_)*K[j,i] +
						y[j]*(a_j-a_j_)*K[j,j] +
						y[j]*(G_(a_j/C)-G_(a_j_/C)))
				diff <- H_i-H_j
				t.iter <- t.iter + 1
			}
			if (verbose) print(paste0('choosing t = ',t))
			a[i] <- a_i
			a[j] <- a_j
			H[i] <- H_i
			H[j] <- H_j
			k.mask <- 1:n!=i&1:n!=j
			H[k.mask] <- H[k.mask]+t*(K[k.mask,i]-K[k.mask,j])
			i <- which.max(H)
			j <- which.min(H)
			H_i <- H[i]
			H_j <- H[j]
			a_i <- a[i]
			a_j <- a[j]
			diff <- H_i-H_j
			if (length(diff) < 1) break
		})
	}
	print.time.if.verbose('Computing final F and H',verbose,{
		F <- apply(a*y*K,2,sum)
		H <- F + y*G_(a/C)
	})
	H_i <- H[which.max(H)]
	H_j <- H[which.min(H)]
	b <- mean(c(H_i,H_j))
	a <- a*y
	resp.f <- function(x) sigmoid((K.meta$k.f(x,X) %*% a)-b) 	
	trn.response <- sigmoid((K%*%a)-b)
	named.list(resp.f,K,a,b,trn.response)
}
