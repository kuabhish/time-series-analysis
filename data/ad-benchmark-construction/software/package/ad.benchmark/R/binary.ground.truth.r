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
binary.ground.truth <- function(response,data,verbose) {
	ground.truth <- as.factor(response)
	if (length(levels(ground.truth))!=2)
		exit('Binary response has more than two responses.')
	one.mask <- ground.truth==levels(ground.truth)[1]
	two.mask <- ground.truth==levels(ground.truth)[2]
	if (sum(one.mask) < sum(two.mask)) {
		levels(ground.truth) <- c('anomaly','nominal')
	} else if (sum(one.mask) > sum(two.mask)) {
		levels(ground.truth) <- c('nominal','anomaly')
	} else if (sum(one.mask) == sum(two.mask)) {
		one.var <- sum(diag(var(data[one.mask,])))
		two.var <- sum(diag(var(data[two.mask,])))
		if (one.var < two.var) {
			levels(ground.truth) <- c('nominal','anomaly')
		} else if (one.var >= two.var) {
			levels(ground.truth) <- c('anomaly','nominal')
		}
	}
	ground.truth
}
