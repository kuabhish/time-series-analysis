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
benchmark.meta.opt <- 
function() list.c(
		input.output.opt(),
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
						help = "The column that includes the difficulty score. Default is toolchain default. (%default)"
					),
					function(opt) {
						if (is.null(opt$diff.col))
							exit("Providing a difficulty column with '--diff.col' option is required. Use option '--help' for more details.")
						opt$rm.cols <- paste0(opt$diff.col,',',opt$rm.cols)
						opt$diff.col <- as.numeric(opt$diff.col)
						if (!ck.int(opt$diff.col)) {
							exit("Difficulty column must be an integer.")
						}
						opt
					}
				),
				list(
					make_option(
						c('-M','--mother.col'),
						action = 'store',
						dest = 'mother.col', default = 2,
						help = "The column that contains the motherset. Default is toolchain default. (%default)"
					),
					function(opt) {
						if (is.null(opt$mother.col))
							exit("Providing a motherset column with '--mother.col' option is required. Use option '--help' for more details.")
						opt$rm.cols <- paste0(opt$mother.col,',',opt$rm.cols)
						opt$mother.col <- as.numeric(opt$mother.col)
						if (!ck.int(opt$mother.col)) {
							exit("Motherset column must be an integer.")
						}
						opt
					}
				),
				list(
					make_option(
						c('-O','--origin.col'),
						action = 'store',
						dest = 'origin.col', default = 3,
						help = "The column that contains the origin. Default is toolchain default. (%default)"
					),
					function(opt) {
						if (is.null(opt$mother.col))
							exit("Providing an origin column with '--origin.col' option is required. Use option '--help' for more details.")
						opt$rm.cols <- paste0(opt$mother.col,',',opt$rm.cols)
						opt$origin.col <- as.numeric(opt$origin.col)
						if (!ck.int(opt$origin.col)) {
							exit("Origin column must be an integer.")
						}
						opt
					}
				)
			),
			post.fn=function(opt) {
				if (opt$rep.col < 1 | opt$rep.col > ncol(opt$data))
					exit("Response column does not exist.")
				opt$response <- opt$data[,opt$rep.col]
				if (opt$diff.col < 1 | opt$diff.col > ncol(opt$data))
					exit("Difficulty column does not exist.")
				opt$diffs <- opt$data[,opt$diff.col]
				if (opt$mother.col < 1 | opt$mother.col > ncol(opt$data))
					exit("Motherset column does not exist.")
				opt$mset <- opt$data[1,opt$mother.col]
				if (opt$origin.col < 1 | opt$origin.col > ncol(opt$data))
					exit("Origin column does not exist.")
				opt$origin <- opt$data[1,opt$origin.col]
				opt
			},
			meta.default='1,2,3,4,5,6'
		),
		seed.and.verbose.opt()
)
