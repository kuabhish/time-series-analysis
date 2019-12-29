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
regression.ground.truth <- function(response,data,verbose) {
	if (length(unique(response)) < 3 | (class(response)!='numeric' & class(response)!='integer')) {
		exit('Regression response is either not numeric or too sparse.')
	}
	med <- median(response)
	low.mask <- response < med
	high.mask <- response > med
	low.var <- sum(diag(var(data[low.mask,])))
	high.var <- sum(diag(var(data[high.mask,])))
	if (low.var < high.var) {
		nom.mask <- response <= med
	} else if (low.var >= high.var) {
		nom.mask <- response >= med
	}
	ground.truth <- as.factor(nom.mask)
	levels(ground.truth) <- c('anomaly','nominal')
	ground.truth
}
