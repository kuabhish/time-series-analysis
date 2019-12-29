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
benchmark.meta.main <-
function() main.factory(benchmark.meta.opt(),{
	ground.truth <- response
	bench.id <- strsplit(undir(in.name),'.csv')[[1]][1]
	benchnum <- as.numeric(strsplit(bench.id,'_')[[1]][3])
	size <- nrow(data)
	amask <- ground.truth == 'anomaly'
	n.anom <- sum(amask)
	n.norm <- size-n.anom
	diff.mean <- mean(diffs)
	anom.diff.mean <- mean(diffs[amask])
	norm.diff.mean <- mean(diffs[!amask])
	creation.factors <- factors.by.benchnum(benchnum)
	anomaly.rate.level <- creation.factors$ar.level
	point.difficulty.level <- creation.factors$pd.level
	clustering.algo <- creation.factors$cl.algo
	irrelevance.level <- creation.factors$ir.level
	anomaly.rate <- n.anom/size
	ir.levels <- c(1,1.2,1.5,2)	
	ir.num <- ir.levels[as.numeric(strsplit(irrelevance.level,'-')[[1]][2])+1]
	true.d <- unpad.dim(ncol(data),ir.num)
	true.data <- data[,1:true.d]
	true.var <- sum(apply(true.data,2,var))
	ir.var <- sum(apply(data,2,var))
	anom.var <- sum(apply(true.data[amask,],2,var))
	norm.var <- sum(apply(true.data[!amask,],2,var))
	irrelevance.score <- log(ir.var/true.var)
	clusteredness.score <- log(norm.var/anom.var)
	out.d <- data.frame(bench.id,benchnum,origin,mset,size,n.anom,n.norm,
						anomaly.rate.level,point.difficulty.level,
						clustering.algo,irrelevance.level,anomaly.rate,
						diff.mean,anom.diff.mean,norm.diff.mean,
						clusteredness.score,irrelevance.score)

    if (!file.exists(out.name)) {
		if (verbose) print('Creating new meta register ...')
		make.csv(out.d,out.name)
	} else {
		if (verbose) print('Concatenating to meta register ...')
		write.table(out.d,paste0('tmp.',bench.id,'.',Sys.getpid()),quote=F,row.names=F,col.names=F,sep=',')
		system(paste0('cat tmp.',bench.id,'.',Sys.getpid(),' >> ',out.name),F)
		system(paste0('rm -f tmp.',bench.id,'.',Sys.getpid()),F)
	}
})
