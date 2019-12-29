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
multiclass.ground.truth <- function(response,data,verbose) {
	response <- as.factor(response)
	print.time.if.verbose("Training RF model.",verbose,{
		rf <- randomForest(data,response,ntree=1000,mtry=1)
	})
	print.time.if.verbose("Getting RF confusion mass.",verbose,{
		rpr <- predict(rf,data,type="prob")
	})
	print.time.if.verbose("Summing confusion masses.",verbose,{
	#Not quite the same as simply computing the off-diagonal of a confusion matrix.
	#Even non-confused points might contain some signal about confusion between points. 
	#Abstractly, cm is a fully connected graph between classes with confusion mass
	#as edge weights.
		cm <- matrix(0,nrow=length(levels(response)),ncol=length(levels(response)))
		for (i in 1:nrow(data)) {
			target <- as.numeric(response[i])
			for (j in 1:ncol(rpr)) {
				if (j != target) {
					cm[target,j] <- cm[target,j] + rpr[i,j]
					cm[j,target] <- cm[target,j]
				}
			}
		}
	})
	#Abstractly, an MST of this graph can inform a confusing partition of the classes.
	#The MST is two-colored (classA and classB) as it is built.
	print.time.if.verbose("Splitting classes",verbose,{
		maxv <- apply(cm,1,max)
		cands <- which(maxv==max(maxv))
		classA <- cands[1]
		classB <- cands[2]
		taken <- cands
		while (length(taken) < length(levels(response))) {
			for (t in taken) for (t_ in taken) cm[t,t_] <- 0
			maxv <- apply(cm[taken,],1,max)
			target <- taken[which(maxv==max(maxv))[1]]
			cand <- which(cm[target,]==max(cm[target,]))
			if (target %in% classA) {
				classB <- c(classB,cand)
			} else {
				classA <- c(classA,cand)
			}
			taken <- c(taken,cand)
		}
	})

	ground.truth <- as.numeric(response) %in% classA
	binary.ground.truth(ground.truth,data,verbose)
}
