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
benchmark.create.opt <- 
function() {
	io <- input.output.opt()
	io[[1]][[1]]@help <- "Path to the motherset to build from. (Required)."
	io[[2]][[1]]@help <- "You may specify a base name for the output benchmarks. (Required)"
	list.c(
		io,
		list(
			list(
				make_option(
					c('-s','--max.size'),
					action = 'store',
					dest = 'max.size', default = 6000,
					help = "You may specify the maximum size of the benchmarks."
				),
				function(opt) {
					if (opt$max.size <= 0 | !ck.int(opt$max.size)) {
						exit("Benchmark size must be a positive integer.")
					}
					opt
				}
			),
			list(
				make_option(
					c('-d','--diff.range'),
					action = 'store',
					dest = 'diff.range', default = '-1',
					help = "You may specify the point difficulty range to enforce, separated by a comma. Default or specifying %default will not enforce difficulty range."
				)
			),
			list(
				make_option(
					c('-P','--pd.level'),
					action = 'store',
					dest = 'pd.level', default = NULL,
					help = "Instead of specifying the difficulty range you may specify a factor in {'pd-0','pd-1','pd-2','pd-3','pd-4'}. Will override --diff.range if specified."
				),
				function(opt) {
					if (!is.null(opt$pd.level)) {
						if (opt$pd.level=='pd-0') {
							opt$diff.range <- -1
						} else if (opt$pd.level=='pd-1') {
							opt$diff.range <- c(0,1/6)
						} else if (opt$pd.level=='pd-2') {
							opt$diff.range <- c(1/6,1/3)
						} else if (opt$pd.level=='pd-3') {
							opt$diff.range <- c(1/3,1/2)
						} else if (opt$pd.level=='pd-4') {
							opt$diff.range <- c(1/2,1)
						} else {
							exit("pd level must be in {'pd-0','pd-1','pd-2','pd-3','pd-4'}.")
						}
					} else {
						opt$pd.level <- 'n/a'
						opt$diff.range <- comma.to.numbers(opt$diff.range)
					}
					if (length(opt$diff.range)>1) {
						if (opt$diff.range[1] < 0)
							exit("Difficulty range minimum must be greater than 0.")
						if (opt$diff.range[2] > 1)
							exit("Difficulty range maximum must be less than 1.")
						if (opt$diff.range[1] >= opt$diff.range[2])
							exit("Difficulty minimum must be less than difficulty maximum.")
					} else if (opt$diff.range!=-1) {
						exit('Difficulty must be a comma-separated minimum and maximum, or -1')
					}
					opt
				}
			),
			list(
				make_option(
					c('-f','--freq'),
					action = 'store',
					dest = 'a.rate', default = 0,
					help = "You may specify the anomaly rate. Default or %default will not enforce a rate."
				),
				function(opt) {
					if (opt$a.rate < 0 | opt$a.rate > 1) {
						exit('Anomaly rate must be in the range [0,1].')
					}
					opt
				}
			),
			list(
				make_option(
					c('-S','--scatter.algo'),
					action = 'store',
					dest = 's.algo', default = 'none',
					help = "You may specify the scattering constrained from among {'none','scatter','cluster'}. Default is %default."
				),
				function(opt) {
					if (!(opt$s.algo %in% c('none','scatter','cluster')))
						exit("Scatter constraint must be in {'none','scatter','cluster'}")
					opt
				}
			),
			list(
				make_option(
					c('-a','--alpha'),
					action = 'store',
					dest = 'alpha', default = 1,
					help = "You may specify the irrelevant feature alpha level. Default is %default."
				),
				function(opt) {
					if (opt$alpha < 1) {
							exit("alpha must be >= 1.")
					}
					opt
				}
			),
			list(
				make_option(
					c('-n','--replicates'),
					action = 'store',
					dest = 'n.replicates', default = 5,
					help = "Specify the number replciates per hyper-paramter intersection. Default is %default"
				),
				function(opt) {
					if (opt$n.replicates < 1 | !ck.int(opt$n.replicates)) {
						exit("Number of replicates must be a positive integer.")
					}
					opt
				}
			),
			list(
				make_option(
					c('-x','--count'),
					action = 'store',
					dest = 'bench.num', default = 1,
					help = "Specify the benchid to begin counting from. Default is %default"
				),
				function(opt) {
					if (!ck.int(opt$bench.num))
						exit("Benchid must be an integer.")
					opt
				}
			)
		),
		postload.opt(
			opts=list(
				list(
					make_option(
						c('-R','--response.col'),
						action = 'store',
						dest = 'rep.col', default = 6,
						help = "The column that includes the original response. Default is toolchain default. (%default)"
					),
					function(opt) {
						if (is.null(opt$rep.col))
							exit("Providing a response column with '--response.col' option is required. Use option '--help' for more details.")
						opt$rm.cols <- paste0(opt$rep.col,',',opt$rm.cols)
						opt$rep.col <- as.numeric(opt$rep.col)
						if (!ck.int(opt$rep.col)) {
							exit("Response column must be an integer.")
						}
						opt
					}
				),
				list(
					make_option(
						c('-D','--diff.col'),
						action = 'store',
						dest = 'diff.col', default = 5,
						help = "The column that includes the point difficulty score. Default is toolchain default. (%default)"
					),
					function(opt) {
						if (is.null(opt$diff.col))
							exit("Providing a difficulty score column with '--diff.col' option is required. Use option '--help' for more details.")
						opt$rm.cols <- paste0(opt$diff.col,',',opt$rm.cols)
						opt$diff.col <- as.numeric(opt$diff.col)
						if (!ck.int(opt$diff.col)) {
							exit("Difficulty column must be an integer.")
						}
						opt
					}
				)
			),
			post.fn=function(opt) {
				if (opt$rep.col < 1 | opt$rep.col > ncol(opt$data))
					exit("Ground truth column does not exist.")
				opt$ground.truth <- opt$data[,opt$rep.col]
				if (opt$diff.col < 1 | opt$diff.col > ncol(opt$data))
					exit("Difficulty column does not exist.")
				opt$diff.score <- opt$data[,opt$diff.col]
				opt
			},
			meta.default = '1,2,3,4'
		),
		seed.and.verbose.opt()
	)
}
