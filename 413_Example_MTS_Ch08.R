###############################################################################
# 413_Assignment_08_Example.R
# Last updated: 2016-02-23 by MJG
###############################################################################

# Adopted from Tsay
# Homework 3 Questions:
#   http://faculty.chicagobooth.edu/ruey.tsay/teaching/mts/sp2015/hw3-15.pdf
# Homework 3 Solutions:
#   http://faculty.chicagobooth.edu/ruey.tsay/teaching/mts/sp2015/hw3s-15.pdf
# Homework 3 R Code:
#   http://faculty.chicagobooth.edu/ruey.tsay/teaching/mts/sp2015/hw3-15-R.txt
# Homework 3 data (used below):
#   http://faculty.chicagobooth.edu/ruey.tsay/teaching/mts/sp2015/hw3p1.txt

# Clear workspace
rm(list=ls())

# Load packages
library(fBasics)
library(MTS)

# Set working directory
setwd("C:/Users/Michael/Dropbox/MSPA/413-DL/Data Sets")

da <- read.table("hw3p1.txt", header = T)

ccm(da)

VMAorder(da)
