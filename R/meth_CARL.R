# Setting the CMH_C function output presentation

# setting class name and its representation as a list
setClass( "carl_test", representation("list"))

# setting how the test will be presented
setMethod("show", "carl_test", function(object) {
  cat("\n",sep = "")
  cat("       LSD Rank Sum Test\n",sep = "")
  cat("\n",sep = "")
  cat("Treatment information:\n")
  print(signif(object$rank_info, object$sig_digits))
  cat("\n",sep = "")
  print(signif(object$results_table,object$sig_digits),sep = "")
  cat("\n")
  cat("\n")
})
