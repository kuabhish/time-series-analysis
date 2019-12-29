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
benchmark.klr.diff.main <-
function() main.factory(benchmark.klr.diff.opt(),{
	ground.truth <- response
	y <- 2*(ground.truth=='nominal')-1

	if (nrow(data)<=max.size) {
		print.time.if.verbose('building klr model',verbose,{
			m <- fast.klr.cv(data,y,n.folds=n.folds,C.vec=C.vec,eps=eps,verbose=verbose)
		})
		print.time.if.verbose('scoring points',verbose,{
			diff.score <- m$trn.response
			diff.score[y==1] <- 1-diff.score[y==1]
		})
		if (exists('meta.data')) {
			outd <- data.frame(meta.data,diff.score,ground.truth,data)
		} else {
			outd <- data.frame(diff.score,ground.truth,data)
		}
		make.csv(outd,out.name)
	} else {
		print.time.if.verbose('building klr model',verbose,{
			m <- fast.klr.sub(data,y,s.size=min(max.size,ceiling((nrow(data)*(n.folds-1))/n.folds)),n.samp=2*n.folds,C.vec=C.vec,eps=eps,verbose=verbose)
		})
		print.time.if.verbose('scoring points',verbose,{
			i <- 1
			while (i <= nrow(data)) {
				print.time.if.verbose(paste0('Scoring from point: ',i),verbose,{
					j <- min(i+999,nrow(data))
					diff.score <- m$resp.f(data[i:j,])
					diff.score[y[i:j]==1] <- 1-diff.score[y[i:j]==1]
					if (exists('meta.data')) {
						outd <- data.frame(meta.data[i:j,],diff.score,ground.truth[i:j],data[i:j,])
						colnames(outd) <- c(colnames(meta.data),'diff.score','ground.truth',colnames(data))
					} else {
						outd <- data.frame(diff.score[i:j],ground.truth[i:j],data[i:j,])
						colnames(outd) <- c('diff.score','ground.truth',colnames(data))
					}
					if (i==1) {
						make.csv(outd,out.name)
					} else {
						tmp.name <- paste0('tmp.',Sys.getpid())
						write.table(outd,tmp.name,quote=F,col.names=F,row.names=F,sep=',')
						system(paste0('cat ',tmp.name,' >> ',out.name))
						system(paste0('rm -f ',tmp.name))
					}
				})
				i <- j+1
			}
		})
	}
})
