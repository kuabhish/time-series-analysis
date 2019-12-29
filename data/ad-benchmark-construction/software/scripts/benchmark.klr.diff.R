#!/usr/bin/Rscript
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

####
#benchmark.klr.diff.R
#author: Andrew Emmott - emmotta@oregonstate.edu
#
#Given a preproccessed data set as input this script generates an "oracle"
#response by building a kernel logistic regression model that discriminates
#between nominal and anomaly points. The model response is used as the point
#difficulty score in analysis.
#
#Optionally, a model for each original label among the anomalies can be
#constructed as well, providing oracle responses for "anomaly classes."
#
#Run the script with option -h for a list of required and optional arguments.
####

###
#Required packages
###
suppressPackageStartupMessages(library(ad.benchmark))

benchmark.klr.diff.main()
