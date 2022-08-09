# Setting the CMH_C function output presentation

# setting class name and its representation as a list
setClass( "PARL_test", representation("list"))

# setting how the test will be presented
setMethod("show", "PARL_test", function(object) {
  cat("\n",sep = "")
  cat("       PARL Test for the Latin Square Design\n",sep = "")
  cat("             with ",object$N_perms," permutations \n",sep = "")
  cat("\n",sep = "")
  cat("data:  ",object$y_name,", ",object$treatment_name,",\n",sep = "")
  cat("       ",object$block1_name," and ",object$block2_name,"\n",sep = "")
  cat("\n",sep = "")
  cat("Test statistic = ", object$statistic,", p-value = ",object$pvalue,"\n",sep = "")
  cat("\n",sep = "")
  cat("For the group levels ",paste(object$treatment_levels,collapse = " "),", \n",sep="")
  cat("the aligned rank sums are: ",object$aligned_rank_sums,"\n")
  cat("\n",sep = "")
  if(object$components == T){
    cat("Chi-squared components and F components: \n")
    print(signif(object$results_table,4),sep = "")
    cat("\n")
    cat("\n")
  }
})
