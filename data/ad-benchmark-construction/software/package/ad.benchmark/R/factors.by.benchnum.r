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
factors.by.benchnum <-
function(n) {
	ar.levels <-  c('ar-0','ar-1','ar-2','ar-3','ar-4','ar-6')
	pd.levels <- c('pd-0','pd-1','pd-2','pd-3','pd-4')
	cl.algos <- c('none','scatter','cluster')
	ir.levels <- c('ir-0','ir-1','ir-2','ir-3')
	n <- ceiling(n/5)
	roll.mod <- function(x,m) {r<-x %% m;if (r==0) m else r}
	ir.level <- ir.levels[roll.mod(n,4)]
	n <- ceiling(n/4)
	cl.algo <- cl.algos[roll.mod(n,3)]
	n <- ceiling(n/3)
	pd.level <- pd.levels[roll.mod(n,5)]
	n <- ceiling(n/5)
	ar.level <- ar.levels[roll.mod(n,6)]
	named.list(ar.level,pd.level,cl.algo,ir.level)
}
