# Setting the CMH_C function output presentation

# setting class name and its representation as a list
setClass( "KW_test", representation("list"))

# setting how the test will be presented
setMethod("show", "KW_test", function(object) {
  cat("\n",sep = "")
  cat("       Kruskal-Wallis Rank Sum Test\n",sep = "")
  cat("\n",sep = "")
  cat("data:  ",object$response_name," and ",object$treatment_name,"\n",sep = "")
  cat("Adjusting for ties: \n",sep = "")
  cat("KW_A = ", object$KW_A,", df = ", object$df, ", p-value = ",object$p_val_A,"\n",sep = "")
  cat("\n",sep = "")
  cat("No adjustment for ties: \n",sep = "")
  cat("KW = ", object$KW,", df = ", object$df, ", p-value = ",object$p_val,"\n",sep = "")
  cat("\n",sep = "")
})
