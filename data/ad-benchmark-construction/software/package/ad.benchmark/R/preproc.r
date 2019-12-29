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
preproc <- function(data,meta.cols=NULL,rm.rows=NULL,rm.cols=NULL,rm.sparse=FALSE) {
    if (!is.null(rm.rows)) {
        rm.rows <- comma.to.numbers(rm.rows)
        if (sum(rm.rows < 1) > 0 | sum(rm.rows > nrow(data)) > 0)
            exit("Asked to remove rows that do not exist.")
    } else {
        rm.rows <- c()
    }
    if (!is.null(rm.cols)) {
        rm.cols <- comma.to.numbers(rm.cols)
        if (sum(rm.cols < 1) > 0 | sum(rm.cols > ncol(data)) > 0)
            exit("Asked to remove cols that do not exist.")
    } else {
        rm.cols <- c()
    }

    if (!is.null(meta.cols)) {
        meta.cols <- comma.to.numbers(meta.cols)	
        if (sum(meta.cols < 1) > 0 | sum(meta.cols > ncol(data)) > 0)
            exit("Meta data columns do not exist.")
		meta.data <- data.frame(data[,meta.cols])
		colnames(meta.data) <- colnames(data)[meta.cols]
    } else {
		meta.cols <- c()
		meta.data <- c()
	}

	rm.cols <- c(rm.cols,meta.cols)

    if (!is.null(rm.rows)) {
        rm.rows <- unique(rm.rows)
        data <- data[-rm.rows,]
    }
    if (!is.null(rm.cols)) {
        rm.cols <- unique(rm.cols)
        data <- data[,-rm.cols]
    }

	if (rm.sparse) {
		rm.cols <- c()
		for (i in 1:ncol(data)) {
			nu <- length(unique(data[,i]))
			if (nu <= 1) {
				rm.cols <- c(rm.cols,i)
			}
		}
		if (!is.null(rm.cols)) {
			data <- data[,-rm.cols]
		}
	}

    named.list(data,meta.data)
}
