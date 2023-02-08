#' KW
#'
#' \code{KW} returns the test statistic and p-value for the Kruskal-Wallis test
#' adjusted for ties.
#'
#' @details
#' The Kruskal-Wallis test is a non-parametric equivalent to the one way ANOVA.
#'
#' @param treatment a factor that defines the group the response belongs to.
#' @param response a numeric variable measuring the response outcome.
#' @param n_components the number of polynomial components you wish to test. The maximum number of components is the number of treatments less one. If the number of components requested is less than \code{t-2}, a remainder component is created.
#' @param n_permutations the number of permutations you wish to run.
#' @param treatment_scores the scores to be applied to the treatment groups. If not declared these will be set automatically and should be checked.
#' @param sig_digits the number of significant digits the output should show.
#' @param verbose flag for turning on the status bar for permutation tests.
#'
#' @return
#' The function returns the test result of the Kruskal Wallis test with
#' adjustment for ties and without.
#'
#' @references
#' Rayner, J.C.W and Livingston Jr, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#' @examples
#' attach(whiskey)
#' KW(treatment = maturity, response = grade)
KW = function(treatment, response, n_components = 0, n_permutations = 0,
                      treatment_scores = NULL, sig_digits = 4, verbose = FALSE){

  if(length(treatment) != length(response)){
    stop("Supplied vectors treatment, and response should be the same lengths.")
  }

  if( (n_permutations < 100) & (n_permutations > 0) ) stop("Number of permutations are too low. Set this to a number as large as you have time to wait for the results.")

  treatment_name = deparse(substitute(treatment))
  response_name = deparse(substitute(response))
  treatment_levels = levels(treatment)

  if(!is.numeric(response)){
    warning("Response is not numeric, attempting to coerce to numeric with as.numeric(). Check that as.numeric(response) results in a suitable coercion.")
    response = as.numeric(response)
  }

  t = length(levels(treatment))

  if(!is.null(treatment_scores) & (length(treatment_scores) != t)) stop("An incorrect number of scores are provided!")

  if( n_components == (t - 2) ) {
    warning("The number of components you have requested will result in a remainder term that is made up of one component. Resetting n_components to remove the remainder term.")
    n_components = t-1
  }

  r_ij = rank(response)
  R_i = tapply(X = r_ij,INDEX = treatment,FUN = sum)
  n_i = tapply(X = treatment,INDEX = treatment,FUN = length)
  n_i[is.na(n_i)] = 0
  n = sum(n_i)
  p_i = n_i/n
  Var = sum(r_ij^2/n) - ((n+1)/2)^2
  #f = sqrt((n-1))/n
  #Z_i = f*(R_i - sum(R_i)*p_i)/sqrt(Var)
  #G_O = sum(Z_i^2/p_i) = n*f^2*sum((R_i - sum(R_i)*p_i)^2/(n_i*Var))

  KW = (12/(n*(n+1)))*sum( (R_i^2)/n_i) - 3*(n+1)

  KW_A = (n-1)*(-n*(n+1)^2/4 + sum(R_i^2/n_i))/
    (sum(r_ij^2) -n*(n+1)^2/4)

  df = t-1
  p_val = pchisq(q = KW,df = df,lower.tail = FALSE)
  p_val_A = pchisq(q = KW_A,df = df,lower.tail = FALSE)

  anova_object = anova(lm(rank(response)~treatment))
  SStreat = anova_object$`Sum Sq`[1]
  MSerror = tail(x = anova_object$'Mean Sq',1)
  edf = tail(anova_object$'Df',1)


  F_stat = anova_object$`F value`[1]
  p_val_F = anova_object$`Pr(>F)`[1]

  #
  # Components
  #

  Component_stats_chi2 = Component_pvals_chi2 = NULL
  Component_stats_F = Component_pvals_F = NULL

  remainder_stat_chi2 = remainder_pval_chi2 = NULL
  remainder_stat_F = remainder_pval_F = NULL
  remainder = FALSE

  if(n_components > 0){
    if( n_components > (t - 1) ){
      warning(paste0("Number of compenents is greater than allowed. Setting number of components to ", t-1,"."))
      n_components = (t - 1)
    }
    if( is.null(treatment_scores )) {
      warning("Components are requested and no treatment scores are given. Using default scores as a result of as.numeric(factor(levels(treatment))). Check that these are correct. The factor may need to have the levels reordered to the correct order.")
      treatment_scores = as.numeric(factor(treatment_levels))
    }

    polys = Poly(x = treatment_scores, p = p_i)[2:(n_components+1),]

    # chi2
    f = sqrt((n-1))/n
    Z_i = f*(R_i - sum(R_i)*p_i)/sqrt(Var)
    # G_O = sum(Z_i^2/p_i) = n*f^2*sum((R_i - sum(R_i)*p_i)^2/(n_i*Var)) # checking

    Component_stats_chi2 = (as.vector((polys%*%Z_i)^2))
    Component_pvals_chi2 = pchisq(q = Component_stats_chi2,df = 1,lower.tail = FALSE)

    # F
    Y_i = (R_i/n_i - sum(R_i)/n)*sqrt(n_i*p_i)

    Component_stats_F = as.vector((polys%*%Y_i)^2)/MSerror
    Component_pvals_F = pf(q = Component_stats_F,df1 = 1, df2 = edf, lower.tail = FALSE)


    # Remainder

    if(n_components < t-1) {
      remainder = TRUE
      remainder_stat_chi2 = KW_A - sum(Component_stats_chi2)
      remainder_pval_chi2 = pchisq(q = remainder_stat_chi2,df = t-n_components-1,lower.tail = FALSE)

      remainder_stat_F = ((SStreat - sum(as.vector((polys%*%Y_i)^2)))/(t-n_components-1))/MSerror
      remainder_pval_F = pf(q = remainder_stat_F,df1 = t-n_components-1, df2 = edf, lower.tail = FALSE)

    }

  }

  #
  # Permutations
  #

  p_val_perm = p_val_A_perm = p_val_F_perm = NULL
  Component_pvals_perm_chi2 = remainder_pval_perm_chi2 = NULL
  Component_pvals_perm_F = remainder_pval_perm_F = NULL
  if( n_permutations > 0 ){

    KW_perm_exceedances = KW_A_perm_exceedances = F_perm_exceedances = 0
    Component_exceedances_chi2 = rep(0, n_components)
    Component_exceedances_F = rep(0, n_components)
    if(remainder) {
      remainder_exceedances_chi2 = 0
      remainder_exceedances_F = 0
    }

    if(verbose) {
      pb = txtProgressBar(min = 0, max = n_permutations, initial = 0)
      cat("Permutation progress bar:")
      cat("\n")
    }

    for (i in 1:n_permutations){
      treatment_perm = sample(treatment, replace = FALSE)

      R_i_perm = tapply(X = r_ij,INDEX = treatment_perm,FUN = sum)

      # Chi/KW_A
      KW_perm = (12/(n*(n+1)))*sum( (R_i_perm^2)/n_i) - 3*(n+1)
      KW_A_perm = (n-1)*(-n*(n+1)^2/4 + sum(R_i_perm^2/n_i))/
        (sum(r_ij^2) -n*(n+1)^2/4)

      # F
      anova_object_perm = anova(lm(rank(response)~treatment_perm))
      SStreat_perm = anova_object_perm$`Sum Sq`[1]
      MSerror_perm = tail(x = anova_object_perm$'Mean Sq',1)
      #edf = tail(anova_object_perm$'Df',1)

      F_stat_perm = anova_object_perm$`F value`[1]

      KW_perm_exceedances = KW_perm_exceedances + as.numeric(KW_perm >= KW)
      KW_A_perm_exceedances = KW_A_perm_exceedances + as.numeric(KW_A_perm >= KW_A)
      F_perm_exceedances = F_perm_exceedances + as.numeric(F_stat_perm >= F_stat)


      if(n_components > 0){

        # chi2
        Z_i_perm = f*(R_i_perm - sum(R_i_perm)*p_i)/sqrt(Var)
        Component_stats_perm_chi2 = (as.vector((polys%*%Z_i_perm)^2))

        # F
        Y_i_perm = (R_i_perm/n_i - sum(R_i_perm)/n)*sqrt(n_i*p_i)
        Component_stats_perm_F =  as.vector((polys%*%Y_i_perm)^2)/MSerror_perm

        # Checking for component exceedances
        Component_exceedances_chi2 = Component_exceedances_chi2 +
          as.numeric(Component_stats_perm_chi2 > Component_stats_chi2)
        Component_exceedances_F = Component_exceedances_F +
          as.numeric(Component_stats_perm_F > Component_stats_F)

        # Remainders
        remainder_stat_perm_chi2 = NULL
        remainder_stat_perm_F = NULL
        if(remainder) {
          remainder_stat_perm_chi2 = KW_A_perm - sum(Component_stats_perm_chi2)
          remainder_stat_perm_F = ((SStreat_perm - sum(as.vector((polys%*%Y_i_perm)^2)))/(t-n_components-1))/MSerror_perm

          remainder_exceedances_chi2 = remainder_exceedances_chi2 + as.numeric(remainder_stat_perm_chi2 > remainder_stat_chi2)
          remainder_exceedances_F = remainder_exceedances_F + as.numeric(remainder_stat_perm_F > remainder_stat_F)
        }

      }

      if(verbose){
        setTxtProgressBar(pb,i)
      }
    }

    if(verbose){close(pb)}

    p_val_perm = KW_perm_exceedances/n_permutations
    p_val_A_perm = KW_A_perm_exceedances/n_permutations

    p_val_F_perm = F_perm_exceedances/n_permutations

    Component_pvals_perm_chi2 = Component_exceedances_chi2/n_permutations
    Component_pvals_perm_F = Component_exceedances_F/n_permutations

    if(remainder) {
      remainder_pval_perm_chi2 = remainder_exceedances_chi2/n_permutations
      remainder_pval_perm_F = remainder_exceedances_F/n_permutations
    }

  }

  # results_table

  all_observed_stats_chi2 = c(KW_A, Component_stats_chi2,remainder_stat_chi2)
  all_observed_stats_F = c(F_stat, Component_stats_F,remainder_stat_F)

  all_pvals_chi2 = c(p_val_A, Component_pvals_chi2, remainder_pval_chi2)
  all_pvals_F = c(p_val_F, Component_pvals_F, remainder_pval_F)

  all_pvals_perm_chi2 = c(p_val_A_perm, Component_pvals_perm_chi2, remainder_pval_perm_chi2)
  all_pvals_perm_F = c(p_val_F_perm, Component_pvals_perm_F, remainder_pval_perm_F)

  results_table = cbind(all_observed_stats_chi2,all_pvals_chi2,all_pvals_perm_chi2,all_observed_stats_F,all_pvals_F,all_pvals_perm_F)
  col_heading = c("chi2 Obs", if(n_permutations > 0) {c("p-value", "p perm")} else {"p-value"}, "F Obs", if(n_permutations > 0) {c("p-value", "p perm")} else {"p-value"})
  row_heading = c("Overall", if(n_components > 0) { c(paste0("Degree ", 1:n_components),if(remainder) {"Remainder"} else {NULL}) } else {NULL})

  dimnames(results_table)[[1]] = row_heading
  dimnames(results_table)[[2]] = col_heading

  rank_info = matrix(nrow = 1,ncol=t)
  rank_info[1,] = R_i/n_i
  if(n_components > 0){rank_info = rbind(rank_info,treatment_scores)}
  rownames(rank_info) = c("Rank means", if(n_components > 0) {"Scores"} else {NULL})
  colnames(rank_info) = treatment_levels

  ret = list(KW_A = KW_A,p_val_A = p_val_A,
             KW = KW, p_val = p_val,
             F_stat = F_stat,p_val_F = p_val_F,
             p_val_A_perm = p_val_A_perm,
             p_val_perm = p_val_perm,
             p_val_F_perm = p_val_F_perm,
             Component_stats_chi2 = Component_stats_chi2,
             Component_pvals_chi2 = Component_pvals_chi2,
             remainder_stat_chi2 = remainder_stat_chi2,
             remainder_pval_chi2 = remainder_pval_chi2,
             Component_pvals_perm_chi2 = Component_pvals_perm_chi2,
             remainder_pval_perm_chi2 = remainder_pval_perm_chi2,
             Component_stats_F = Component_stats_F,
             Component_pvals_F = Component_pvals_F,
             remainder_stat_F = remainder_stat_F,
             remainder_pval_F = remainder_pval_F,
             Component_pvals_perm_F = Component_pvals_perm_F,
             remainder_pval_perm_F = remainder_pval_perm_F,
             R_i = R_i,
             n_i = n_i,
             response_name = response_name,treatment_name = treatment_name,treatment_scores = treatment_scores,df=df,
             treatment_levels = treatment_levels,
             treatment_scores = treatment_scores,
             sig_digits = sig_digits,
             results_table = results_table,
             rank_info = rank_info)
  #return(ret)

  new("KW_test", ret)

}





