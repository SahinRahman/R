########################################################################
#  Source this file from your standard .Rprofile, with option chdir=TRUE
########################################################################

#  Set the RHOME environment variable so can source from anywhere in file system
Sys.setenv(RHOME=paste(Sys.getenv('HOME'),'R',sep='/'))

options(digits.secs=3)  #  display times with milliseconds
options(digits=12)      #  display floating point values with lots of digits
options(width=180)      #  width of a terminal window. Need wide lines for oplot error reporting

options(stringsAsFactors=FALSE)  # for data.frame and read.table

#  set default packages
#  Strangely, the packages named in the options() call are loaded *after* this init file
options(defaultPackages=c("grDevices","methods","limSolve","quadprog","Cairo"))

#  Packages named explicitly are loaded right here,
#   and we need them in order to precompile the utils functions below.
library('graphics')
library('stats')
library('utils')
library('stringr')
library('compiler')
library('timeDate')
library(rgl)

source('utils/src.R',chdir=TRUE)
