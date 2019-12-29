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
preproc.opt <-
function(meta.default=NULL) list(
	list(	
        make_option(
            c('-m','--meta.col'),
            action = 'store',
            dest = 'meta.cols', default = meta.default,
            help = "You may specify a comma-separated list of columns to leave intact as meta-data. Default is %default."
        ),
		function(opt) {
			print.time.if.verbose("Loading data set ...",opt$verbose,opt$data <- read.csv(opt$in.name))
			opt
		}
	),
	list(
        make_option(
            c('-r','--rows.rm'),
            action = 'store',
            dest = 'rm.rows', default = NULL,
            help = "You may provide a comma-separated list of rows to remove from the data set."
        )
	),
	list(
        make_option(
            c('-c','--cols.rm'),
            action = 'store',
            dest = 'rm.cols', default = NULL,
            help = "You may provide a comma-separated list of columns to remove from the data set."
        ),
		function(opt) {
			print.time.if.verbose("Preprocessing data ...",opt$verbose,{
				prpr <- preproc(opt$data,opt$meta.cols,opt$rm.rows,opt$rm.cols,TRUE)
				opt$data <- prpr$data
				opt$meta.data <- prpr$meta.data
			})
			opt
		}
	)
)
