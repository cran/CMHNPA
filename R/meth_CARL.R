# Setting the CMH_C function output presentation

# setting class name and its representation as a list
setClass( "CARL_test", representation("list"))

# setting how the test will be presented
setMethod("show", "CARL_test", function(object) {
  cat("\n",sep = "")
  cat("       CARL Test for the Latin Square Design\n",sep = "")
  cat("\n",sep = "")
  cat("data:  ",object$y_name,", ",object$treatment_name,",\n",sep = "")
  cat("       ",object$block1_name," and ",object$block2_name,"\n",sep = "")
  cat("\n",sep = "")
  cat("Test statistic = ", object$statistic,", df = ", object$df, ", p-value = ",object$pvalue,"\n",sep = "")
  cat("\n",sep = "")
  cat("For the group levels ",paste(object$treatment_levels,collapse = " "),", \n",sep="")
  cat("the aligned rank sums are: ",object$R_i..,"\n")
  cat("\n",sep = "")
})
