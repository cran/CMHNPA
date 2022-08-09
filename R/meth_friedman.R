# Setting the friedman function output presentation

# setting class name and its representation as a list
setClass( "friedman_test", representation("list"))

# setting how the test will be presented
setMethod("show", "friedman_test", function(object) {
  cat("\n")
  cat("       Friedman Rank Sum Test\n")
  cat("\n")
  cat("Test statistic is adjusted for ties\n")
  cat("data: ", object$response_name, object$groups_name, object$blocks_name,"\n")
  cat("Fdm_A = ", object$Fdm_A,", df = ", object$df, ", p-value = ",object$p_value,"\n",sep = "")
  cat("\n")
  cat("For the group levels ",paste(object$groups_levels,collapse = " "),", \n",sep="")
  cat("the rank sums are: ",object$R_i.,"\n")
  cat("\n")
  cat("\n")
  if(object$components == T){
    cat("Chi-square components p-values\n")
    cat("Overall: ", round(object$p_value,5),"\n")
    cat("Linear: ", round(object$chi_p_val_linear,5),"\n")
    cat("Quadratic: ", round(object$chi_p_val_quad,5),"\n")
    cat("Remainder: ", round(object$chi_p_val_remain,5),"\n")
    cat("\n")
    cat("F components p-values\n")
    cat("Overall: ", round(object$F_p_val_overall,5),"\n")
    cat("Linear: ", round(object$F_p_val_linear,5),"\n")
    cat("Quadratic: ", round(object$F_p_val_quad,5),"\n")
    cat("Remainder: ", round(object$F_p_val_remain,5),"\n")
    cat("\n")
    cat("\n")
  }
})


