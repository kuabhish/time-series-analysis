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
benchmark.preproc.opt <-
function() list.c(
	input.output.opt(),
	list(
		list(
			make_option(
				c('-O','--origin'),
				action = 'store',
				dest = 'origin', default = NULL,
				help = "Specify the origin from among {'binary','multiclass','regression'}. (Required)."
			),
			function(opt) {
				if (is.null(opt$origin))
					exit("Providing an origin with '--origin' option is required. Use option '--help' for more details.")
				valid.origins <- c('binary','multiclass','regression')
				if (!(opt$origin %in% valid.origins))
					exit("Origin must be in {'binary','multiclass','regression'}.")
				opt
			}
		),
		list(
			make_option(
				c('-M','--motherset'),
				action = 'store',
				dest = 'motherset', default = NULL,
				help = "You may explicitly name the motherset, otherwise it will be inferred from input file name, (Perhaps incorrectly)."
			),
			function(opt) {
				if (is.null(opt$motherset)) {
					cl.name <- undir(opt$in.name)
					sp.name <- strsplit(cl.name,'original')[[1]]
					if (length(sp.name)==2) {
						opt$motherset <- substr(sp.name[1],1,nchar(sp.name[1])-1)
					} else {
						opt$motherset <- cl.name
					}
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
					dest = 'rep.col', default = NULL,
					help = "The column that includes the original response. (Required)."
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
		meta.default=NULL
	),
	seed.and.verbose.opt()	
)
