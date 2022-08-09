# Setting the CMH function output presentation

# setting class name and its representation as a list
setClass( "CMH_test", representation("list"))

# setting how the test will be presented
setMethod("show", "CMH_test", function(object) {
  cat("\n",sep = "")
  cat("       Cochran Mantel Haenszel Tests \n",sep = "")
  cat("\n",sep = "")
  print(signif(object$output_table_main,4),sep = "")
  cat("\n",sep = "")
  cat("\n",sep = "")
  if(object$test_C){
    if(object$cor_breakdown){
      if(length(object$levels_strata)>1){
        cat("Correlations and statistics by strata: \n",sep = "")
        print(signif(object$output_table_cors,4),sep = "")
        cat("\n",sep = "")
      } else {
        cat("Correlation and statistics: \n",sep = "")
        print(signif(object$output_table_cors[,-(1:2)],4),sep = "")
        cat("\n",sep = "")
      }
    }
  }

})
