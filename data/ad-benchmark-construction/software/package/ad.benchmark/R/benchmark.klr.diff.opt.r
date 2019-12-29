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
benchmark.klr.diff.opt <- 
function() list.c(
		input.output.opt(),
		list(
			list(
				make_option(
					c('-n','--n.folds'),
					action = 'store',
					dest = 'n.folds', default = 5,
					help = "You may specify the number of folds fo cross validation. Default is %default."
				),
				function(opt) {
					if (opt$n.folds < 2 | !ck.int(opt$n.folds)) {
						exit("n.folds must be an integer 2 or greater.")
					}
					opt
				}
			),
			list(
				make_option(
					c('-s','--max.size'),
					action = 'store',
					dest = 'max.size', default = 5000,
					help = "You may specify the maximum size before subsampling. Default is %default."
				),
				function(opt) {
					if (opt$max.size < 2 | !ck.int(opt$max.size)) {
						exit("max.size must be an integer 2 or greater.")
					}
					opt
				}
			),
			list(
				make_option(
					c('-C','--c.range'),
					action = 'store',
					dest = 'C.range', default = 5,
					help = "You may specify the range, in terms of 10^(-2n:2n), to check in cross validation. Default is %default."
				),
				function(opt) {
					if (opt$C.range < 0 | !ck.int(opt$C.range)) {
						exit("C.range must be a non-negative integer.")
					}
					opt$C.vec <- 10^(2*(-opt$C.range):opt$C.range)
					opt
				}
			),
			list(
				make_option(
					c('-e','--eps'),
					action = 'store',
					dest = 'eps', default = 0.005,
					help = "You may specify the error tolerance for convergence. Default is %default."
				),
				function(opt) {
					if (opt$eps <= 0) {
						exit("epsilon must be positive.")
					}
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
						dest = 'rep.col', default = 5,
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
				)
			),
			post.fn=function(opt) {
				if (opt$rep.col < 1 | opt$rep.col > ncol(opt$data))
					exit("Response column does not exist.")
				opt$response <- opt$data[,opt$rep.col]
				opt
			},
			meta.default='1,2,3,4'
		),
		seed.and.verbose.opt()
)
