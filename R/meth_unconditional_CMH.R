# Setting the CMH function output presentation

# setting class name and its representation as a list
setClass( "unconditional_CMH_test", representation("list"))

# setting how the test will be presented
setMethod("show", "unconditional_CMH_test", function(object) {
  cat("\n",sep = "")
  cat("       Unconditional Analogues to the CMH Tests \n",sep = "")
  cat("\n",sep = "")
  print(signif(object$output_table_main,4),sep = "")
  cat("\n",sep = "")
  cat("\n",sep = "")
})
