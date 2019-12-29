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
benchmark.preproc.main <-
function() main.factory(benchmark.preproc.opt(),{
	print.time.if.verbose('Removing non-numeric data.',verbose,{
		to.rm <- c()
		for (i in 1:ncol(data))
			if (class(data[,i])!='numeric' & class(data[,i])!='integer')
				to.rm <- c(to.rm,i)
		if (!is.null(to.rm))
			data <- data[,-to.rm]
		cn <- colnames(data)
	})

	print.time.if.verbose('Translating by mean and scaling by sd.',verbose,{
		mu.vec <- apply(data,2,mean)
		sd.vec <- apply(data,2,sd)
		data <- t((t(data)-mu.vec)/sd.vec)
		colnames(data) <- cn
	})

	print.time.if.verbose(paste0('Converting ',origin,' response to ground truth.'),verbose,{
		ground.truth <- get(paste0(origin,'.ground.truth'))(response,data,verbose)
	})

	point.id <- paste0(motherset,'_point_',formatC(1:nrow(data),digits=nchar(as.character(nrow(data)))-1,flag='0')) 
	original.label <- response
	motherset <- rep(motherset,nrow(data))
	origin <- rep(origin,nrow(data))
	
	if (!exists('meta.data')) {
		meta.data <- data.frame(point.id,motherset,origin,original.label,ground.truth)
	} else {
		meta.data <- data.frame(point.id,motherset,origin,original.label,meta.data,ground.truth)
	}

	data <- data.frame(meta.data,data)

	make.csv(data,out.name)
})
