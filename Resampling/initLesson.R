# Put initialization code in this file.
library(ggplot2)
data(InsectSprays)
#get correct paths for plotting
pathtofile <- function(fileName){
  mypath <- file.path(find.package("swirl"),
                      "Courses/Statistical_Inference/Resampling",
                      fileName)
}
fxfer <- function(fileName){
  mypath <- pathtofile(fileName)
  file.copy(mypath,fileName)
  fileName
}
#fname <- paste(getwd(),"Statistical_Inference/Hypothesis_Testing/father_son.csv",sep="/")
fname <- pathtofile("father_son.csv")
fs <- read.csv(fname)
sh <- fs$sheight
fh <- fs$fheight
nh <- length(sh)
B <- 1000
swirl_options(swirl_logging = TRUE)