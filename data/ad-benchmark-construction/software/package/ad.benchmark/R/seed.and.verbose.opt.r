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
seed.and.verbose.opt <-
function() list(
	list(
        make_option(
            c(NULL,'--seed'),
            action = 'store',
            dest = 'seed', default = NULL,
            help = "You may set the random seed for the purposes of making runs reproducible."
        ),
    	function(opt) {
			if (!is.null(opt$seed)) {
				opt$seed <- as.numeric(opt$seed)
				if (!ck.int(opt$seed)) {
					exit("Random seed must be an integer.")
				}
				set.seed(opt$seed)
			}
			opt
		}

	),
	list(
        make_option(
            c('-v','--verbose'),
            action = 'store_true',
            dest = 'verbose', default = FALSE,
            help = "Standard verbose flag. Will print status updates as the algorithm runs."
        )
	)
)
