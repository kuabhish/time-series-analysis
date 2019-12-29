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
input.output.opt <-
function() list(
	list(	
        make_option(
            c('-i','--input'),
            action = 'store',
            dest = 'in.name', default = NULL,
            help = "Path to the input file. (Required)."
        ),
    	function(opt) {
			if (is.null(opt$in.name))
				exit("Providing an input file with '--input' option is required. Use option '--help' for more details.")
		   
			if (!is.file(opt$in.name))
				exit("Provided input file does not exist or is not a regular file.")

			opt
		}
	),
	list(
        make_option(
            c('-o','--output'),
            action = 'store',
            dest = 'out.name', default = NULL,
            help = "Path to the output file. (Required)."
        ),
    	function(opt) {
			if (is.null(opt$out.name))
				exit("Providing an output file with '--output' option is required. Use option '--help' for more details.")

			if (is.dir(opt$out.name))
				exit("Provided output file is a directory.")
	
			opt
		}
	)
)
