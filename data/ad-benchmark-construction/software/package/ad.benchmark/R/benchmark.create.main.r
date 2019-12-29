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
benchmark.create.main <-
function() main.factory(benchmark.create.opt(),{
	n <- nrow(data)
	norm.mask <- which(ground.truth=='nominal')
	anom.mask <- which(ground.truth=='anomaly')
	c.rate <- length(anom.mask)/n
	diff.base <- median(diff.score)

	reset.selection <- function() {
		indices <- c()
		diff.mean <- 0
		diff.n <- 0
		norm.mean <- rep(0,ncol(data))
		norm.M <- norm.mean
		norm.n <- 0
		norm.var <- 0
		anom.mean <- rep(0,ncol(data))
		anom.M <- anom.mean
		anom.n <- 0
		anom.var <- 0
		named.list(indices,diff.mean,diff.n,norm.mean,norm.M,norm.n,norm.var,anom.mean,anom.M,anom.n,anom.var)
	}

	compute.filter <- function(gt.allowed=c('nominal','anomaly'),for.removal=F) {
		if (for.removal) {
			indices <- selection$indices[ground.truth[selection$indices] %in% gt.allowed]
		} else {
			indices <- (1:n)[(!(1:n %in% selection$indices)) & ground.truth %in% gt.allowed]
		}
		filter.gt <- ground.truth[indices]
		filter.diff <- diff.score[indices]
		if (length(diff.range)>1) {
			after.fn <- if (for.removal) mean.without else mean.after
			after.diff <- after.fn(selection$diff.mean,selection$diff.n,filter.diff)
			mask <- after.diff >= diff.range[1] & after.diff < diff.range[2]
			indices <- indices[mask]
			filter.gt <- filter.gt[mask]
			filter.diff <- filter.diff[mask]
			after.diff <- after.diff[mask]
		} else {
			after.diff <- c()
		}
		if (length(indices)>0 & s.algo != 'none') {
			nmask <- filter.gt=='nominal'
			amask <- filter.gt=='anomaly'
			n.vec <- ((nmask)*selection$norm.n)+((amask)*selection$anom.n)
			after.var <- rep(0,length(indices))
			after.fn <- if (for.removal) M.without else M.after
			sapply(1:ncol(data),function(i) {
				u.vec <- ((nmask)*selection$norm.mean[i])+((amask)*selection$anom.mean[i])
				M.vec <- ((nmask)*selection$norm.M[i])+((amask)*selection$anom.M[i])
				after.var <<- after.var + after.fn(M.vec,u.vec,n.vec,data[indices,i])
			})
			denom <- if (for.removal) pmax(n.vec-2,1) else pmax(n.vec,1)
			after.var <- after.var/denom
			after.var[after.var==0] <- 1
			other.var <- ((nmask)*selection$anom.var)+((amask)*selection$norm.var)
			other.var[other.var==0] <- 1
			after.cl.score <- after.var/other.var
			recip.mask <- (2*as.numeric(nmask)-1)*(2*as.numeric(s.algo=='cluster')-1)
			after.cl.score <- after.cl.score^recip.mask
			mask <- after.cl.score >= 1
			indices <- indices[mask]
			after.diff <- after.diff[mask]
			after.cl.score <- after.cl.score[mask]
		} else {
			after.cl.score <- c()
		}
		named.list(indices,after.diff,after.cl.score)
	}

	score.filter <- function() {
		if (length(diff.range)>1 & s.algo != 'none') {
			(abs(filter$after.diff-diff.base)*filter$after.cl.score)^0.5
		} else if (length(diff.range)>1) {
			abs(filter$after.diff-diff.base)
		} else if (s.algo != 'none') {
			filter$after.cl.score
		} else {
			rep(1,length(filter$indices))
		}
	}

	update.selection <- function(i,for.removal=F) {
		if (for.removal) {
			selection$indices <<- selection$indices[selection$indices != i]
			mean.fn <- mean.without
			M.fn <- M.without
			inc.val <- -1
		} else {
			selection$indices <<- c(selection$indices,i)
			mean.fn <- mean.after
			M.fn <- M.after
			inc.val <- 1
		}
		selection$diff.mean <<- mean.fn(selection$diff.mean,selection$diff.n,diff.score[i])
		selection$diff.n <<- selection$diff.n + inc.val
		point <- t(data[i,])[,1]
		if (ground.truth[i]=='nominal') {
			selection$norm.M <<- M.fn(selection$norm.M,selection$norm.mean,selection$norm.n,point)
			selection$norm.mean <<- mean.fn(selection$norm.mean,selection$norm.n,point)
			selection$norm.var <<- sum(selection$norm.M)/max(selection$norm.n-(2*for.removal),1)
			selection$norm.n <<- selection$norm.n + inc.val
		} else {
			selection$anom.M <<- M.fn(selection$anom.M,selection$anom.mean,selection$anom.n,point)
			selection$anom.mean <<- mean.fn(selection$anom.mean,selection$anom.n,point)
			selection$anom.var <<- sum(selection$anom.M)/max(selection$anom.n-(2*for.removal),1)
			selection$anom.n <<- selection$anom.n + inc.val
		}
	}

	failstr <- paste0('Too many benchmark creation failures at these settings (diff.range,a.rate,s.algo)',c(diff.range,a.rate,s.algo))

	n.fails <- 0
	for (b in 1:n.replicates) print.time.if.verbose(paste0('Attempting replicate: ',b),verbose,{
		succ <- F
		while (!succ & n.fails < (b+1)*20) {
			if (verbose) print(paste0('Number of failures: ',n.fails))
			nn.targ <- max(floor(min(0.9*length(norm.mask),min((1-a.rate)*max.size,max.size-2))),2)
			na.targ <- max(ceiling(min((nn.targ/(1-a.rate))-nn.targ,0.9*length(anom.mask))),2)
			nn.targ <- max(floor(min(nn.targ,(na.targ/a.rate)-na.targ)),2)
			selection <- reset.selection()
			reattempting <- F
			gt.allowed <- 'nominal'
			n.switch <- 0
			init.flag <- T
			while ((init.flag | n.switch < 100) & ((a.rate > 0 & (selection$norm.n!=nn.targ | selection$anom.n!=na.targ)) | (a.rate == 0 & length(selection$indices) < nn.targ+na.targ))) {
				if (verbose) print(paste0('Number of direction switches: ',n.switch))
				for.removal <- a.rate > 0 & (selection$anom.n > na.targ | selection$norm.n > nn.targ)
				if (reattempting) {
					init.flag <- F
					gt.allowed <- if (gt.allowed[1] == 'nominal') 'anomaly' else 'nominal'
					if (for.removal) {
						if (gt.allowed == 'nominal' & selection$norm.n <= nn.targ) {
							gt.allowed <- c()
						} else if (gt.allowed == 'anomaly' & selection$anom.n <= na.targ) {
							gt.allowed <- c()
						}
					} else if (a.rate > 0) {
						if (gt.allowed == 'nominal' & selection$norm.n >= nn.targ) {
							gt.allowed <- c()
						} else if (gt.allowed == 'anomaly' & selection$anom.n >= na.targ) {
							gt.allowed <- c()
						}
					}
				} else if (init.flag & (length(selection$indices)==0 | length(selection$indices)==2)) {
					gt.allowed <- 'nominal'
				} else if (init.flag & (length(selection$indices)==1 | length(selection$indices)==3)) {
					gt.allowed <- 'anomaly'
				} else if (for.removal) {
					init.flag <- F
					gt.allowed <- if (runif(1) < (max(0,selection$anom.n - na.targ))/(max(0,selection$anom.n - na.targ)+max(0,selection$norm.n - nn.targ))) 'anomaly' else 'nominal'
				} else if (selection$anom.n < floor(0.2 * na.targ) & selection$norm.n < floor(0.2 * nn.targ)) {
					init.flag <- F
					gt.allowed <- c('nominal','anomaly')
				} else if (a.rate > 0) {
					init.flag <- F
					gt.allowed <- if (runif(1) < (max(0,na.targ - selection$anom.n))/(max(0,na.targ - selection$anom.n)+max(0,nn.targ - selection$norm.n))) 'anomaly' else 'nominal'
				} else {
					init.flag <- F
					gt.allowed <- if (runif(1) < c.rate) 'anomaly' else 'nominal'
				}
				if (verbose) print(paste0('At size ',length(selection$indices),' attempting to ',if (for.removal) 'remove ' else 'add ',gt.allowed,' point ...'))
				filter <- compute.filter(gt.allowed,for.removal)
				if (length(filter$indices)>0) {
					reattempting <- F
					if (length(selection$indices)<max(ceiling(0.05*(nn.targ+na.targ)),4)) {
						scores <- rep(1,length(filter$indices))
					} else {
						n.blank <- floor(length(filter$indices)*0.2)
						scores <- score.filter()
						scores[sample(length(scores),n.blank)] <- 0
					}
					if (any(scores==Inf)) {
						new.index <- filter$indices[scores==Inf][sample(sum(scores==Inf),1)]
					} else if (length(scores)>1) {
						lsum <- log.add(scores[1],scores[2])
						if (length(scores)>2) for (i in 3:length(scores)) {
							lsum <- log.add(lsum,scores[i])
						}
						scores <- cumsum(exp(scores-lsum))
						new.index <- filter$indices[which.max(scores>runif(1))]
					} else {
						new.index <- filter$indices[1]
					}
					if (verbose) print(new.index)
					update.selection(new.index,for.removal)
				} else if (reattempting & a.rate == 0) {
					succ <- selection$anom.n >= 2 & selection$norm.n >= 2
					break
				} else if (reattempting) {
					reattempting <- F
					n.switch <- n.switch + 1
					if (length(selection$indices) <= 4) break
					if (	(((selection$anom.n/length(selection$indices)) > a.rate) & !for.removal) |
							(((selection$anom.n/length(selection$indices)) < a.rate) & for.removal)) {
						nn.targ <- selection$norm.n
						na.targ <- ceiling((nn.targ/(1-a.rate))-nn.targ)
					} else {
						na.targ <- selection$anom.n
						nn.targ <- floor((na.targ/a.rate)-na.targ)
					}
					if (nn.targ < 2 | na.targ < 2) break
				} else {
					reattempting <- T
				}
			}
			succ <- succ | (((a.rate > 0 & (selection$norm.n==nn.targ & selection$anom.n==na.targ)) | (a.rate == 0 & length(selection$indices) <= nn.targ+na.targ)) & (selection$norm.n >= 2 & selection$anom.n >= 2))
			if (!succ) {
				n.fails <- n.fails + 1
			}
		}
		if (!succ) {
			print(failstr)
			break
		} else {
			if (exists('meta.data')) {
				outd <- data.frame(meta.data[selection$indices,],diff.score[selection$indices],ground.truth[selection$indices],data[selection$indices,])
				colnames(outd) <- c(colnames(meta.data),'diff.score','ground.truth',colnames(data))
			} else {
				outd <- data.frame(diff.score[selection$indices],ground.truth[selection$indices],data[selection$indices,])
				colnames(outd) <- c('diff.score','ground.truth',colnames(data))
			}
			irr.rate <- 1
			if (alpha > 1) print.time.if.verbose(paste0('Adding irrelevant features with alpha=',alpha),verbose,{
				irr.rate <- sum(apply(data[selection$indices,],2,var))
				irr.matrix <- noise.columns(data,length(selection$indices),pad.dim(ncol(data),alpha))
				colnames(irr.matrix) <- paste('noise.',1:ncol(irr.matrix))
				outd <- data.frame(outd,irr.matrix)
				rm (irr.matrix)
			})
			cur.bench.id <- paste0(basename(out.name),'_benchmark_',formatC(bench.num,width=4,format='d',flag='0'))
			bench.fn <- file.path(dirname(out.name),paste0(cur.bench.id,'.csv'))
			make.csv(outd,bench.fn)
			rm(outd)
		}
		bench.num <- bench.num + 1
	})
})
