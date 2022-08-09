# Setting the gen_cor function output presentation

# setting class name and its representation as a list
setClass( "gen_cor_test", representation("list"))

# setting how the test will be presented
setMethod("show", "gen_cor_test", function(object) {
  cat("\n",sep = "")
  cat("       Generalised Correlations\n",sep = "")
  cat("\n",sep = "")
  cat("data:  ",object$treatment_name,", ",object$response_name,
      if(object$strata_name == "NULL"){""} else {paste(", and ",object$strata_name,sep = "")},
      "\n\n",sep = "")
  cat("Table of correlations and p-values \n",sep = "")
  for (w in 1:dim(object$correlations)[3]){
    cat(paste("w = ",w-1,"\n",sep = ""))
    print(round(object$output_table[,,w],object$rounding))
    cat(paste("\n",sep = ""))
  }

  cat("\n",sep = "")
})
