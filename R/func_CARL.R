#' CARL Test
#'
#' \code{CARL} returns the test statistic and p-value for the aligned RL test
#' with empirically fitted degrees of freedom.
#'
#' @details
#' This test is applicable to Latin square designs and is recommended over the
#' RL and ARL test. The test uses t+1 as the degrees of freedom of the
#' chi-squared null distribution and results in appropriate test sizes as well
#' as good power.
#'
#' @param y a numeric vector for the response variable.
#' @param treatment a vector giving the treatment type for the corresponding
#' elements of \code{y}.
#' @param block1 a vector giving the first blocking variable for the
#' corresponding elements of \code{y}.
#' @param block2 a vector giving the second blocking variable for the
#' corresponding elements of \code{y}.
#' @param n_components the number of polynomial components you wish to test. The maximum number of components is the number of treatments less one. If the number of components requested is less than \code{t-2}, a remainder component is created.
#' @param n_permutations the number of permutations you wish to run.
#' @param treatment_scores the scores to be applied to the treatment groups. If not declared these will be set automatically and should be checked.
#' @param sig_digits the number of significant digits the output should show.
#' @param verbose flag for turning on the status bar for permutation tests.
#'
#' @return The \code{CARL} test statistic adjusted for ties together with the
#' associated p-value using a chi-squared distribution with t+1 degrees of
#' freedom.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @seealso [ARL()] [PARL()]
#'
#' @examples
#' attach(peanuts)
#' CARL(y = yield, treatment = treatment, block1 = row, block2 = col)
#'
#' @importFrom methods new
#' @importFrom utils txtProgressBar setTxtProgressBar tail
#'
#' @export
CARL = function(y, treatment, block1, block2, n_components = 0, n_permutations = 0,
                      treatment_scores = NULL, sig_digits = 4, verbose = FALSE){

  if( length(unique(apply(X = cbind(y,treatment,block1,block2),MARGIN = 2,length))) != 1 ){
    stop("Supplied vectors y, treatment, block1, and block2 should be the same lengths.")
  }

  if(!is.factor(treatment) | !is.factor(block1) | !is.factor(block2)) stop("Treatment, block1, and block2 need to be factors")

  if( (n_permutations < 100) & (n_permutations > 0) ) stop("Number of permutations are too low. Set this to a number as large as you have time to wait for the results.")

  y_name = deparse(substitute(y))
  treatment_name = deparse(substitute(treatment))
  block1_name = deparse(substitute(block1))
  block2_name = deparse(substitute(block2))
  treatment_levels = levels(treatment)

  if(!is.numeric(y)){
    warning("y is not numeric, attempting to coerce to numeric with as.numeric(). Check that as.numeric(y) results in a suitable coercion.")
    response = as.numeric(response)
  }

  t = sqrt(length(y))

  if(!is.null(treatment_scores) & (length(treatment_scores) != t)) stop("An incorrect number of scores are provided!")

  if( t != length(treatment_levels)){
    stop("Square root of the length of the response variable is not equal to the number of treatment levels. Perhaps this is not a LSD?")
  }

  if( n_components == (t - 2) ) {
    warning("The number of components you have requested will result in a remainder term that is made up of one component. Resetting n_components to remove the remainder term.")
    n_components = t-1
  }

  row.col.effects = row_col_effect(to_align = y, rows = block1,
                                   cols = block2)
  aligned_response = y - row.col.effects

  r_ij = rank(aligned_response)
  R_i.. = tapply(X = r_ij, INDEX = treatment, sum)

  n_i = tapply(X = treatment,INDEX = treatment,FUN = length)
  n_i[is.na(n_i)] = 0
  n = sum(n_i)
  p_i = n_i/n
  Var = sum(r_ij^2/n) - ((n+1)/2)^2
  f = 1/t
  Z_i = f*(R_i.. - sum(R_i..)*p_i)/sqrt(Var)

  # chi
  G_O = sum(Z_i^2/p_i)
  # sum(Z_i^2/p_i)
  # n*f^2*sum((R_i.. - sum(R_i..)*p_i)^2/(n_i*Var))

  ARL_stat = G_O
  CARL_df = t+1
  ARL_df = t-1
  p_val_ARL = pchisq(q = ARL_stat,df = ARL_df,lower.tail = FALSE)
  p_val_CARL = pchisq(q = ARL_stat,df = CARL_df,lower.tail = FALSE)

  #
  # Components
  #

  Component_stats_chi2 = Component_pvals_ARL_chi2 = Component_pvals_CARL_chi2 = NULL
  remainder_stat_chi2 = remainder_pval_ARL_chi2 = remainder_pval_CARL_chi2 = NULL
  if(n_components > 0){
    if( n_components > (t - 1) ){
      warning(paste0("Number of compenents is greater than allowed. Setting number of components to t-1 = ", t-1,"."))
      n_components = (t - 1)
    }
    if( is.null( treatment_scores )) {
      warning("Components are requested and no treatment scores are given. Using default scores as a result of as.numeric(factor(levels(treatment))). Check that these are correct. The factor may need to have the levels reordered to the correct order.")
      treatment_scores = as.numeric(factor(levels(treatment)))
    }

    polys = Poly(x = treatment_scores, p = p_i)[2:(n_components+1),]

    # chi-square
    Component_stats_chi2 = as.vector((polys%*%Z_i)^2)
    # sum(Z_i^2/p_i)
    # n*f^2*sum((R_i.. - sum(R_i..)*p_i)^2/(n_i*Var))
    # sum((polys%*%Z_i)^2)

    Component_pvals_ARL_chi2 = pchisq(q = Component_stats_chi2,df = 1,lower.tail = FALSE)
    Component_pvals_CARL_chi2 = pchisq(q = Component_stats_chi2,df = (t+1)/(t-1),lower.tail = FALSE)

    remainder = FALSE
    if(n_components < t-1) {
      remainder = TRUE
      remainder_stat_chi2 = G_O - sum(Component_stats_chi2)
      remainder_pval_ARL_chi2 = pchisq(q = remainder_stat_chi2,df = t-1-n_components,lower.tail = FALSE)
      remainder_pval_CARL_chi2 = pchisq(q = remainder_stat_chi2,df = t+1-n_components*(t+1)/(t-1),lower.tail = FALSE)
    }
  }

  #
  # Permutations
  #

  p_val_ARL_perm = Component_pvals_perm_chi2 = remainder_pval_perm_chi2 = NULL
  if( n_permutations > 0 ){

    ARL_perm_exceedances = 0
    Component_exceedances_chi2 = rep(0, n_components)
    if(remainder) remainder_exceedances_chi2 = 0

    if(verbose) {
      pb = txtProgressBar(min = 0, max = n_permutations, initial = 0)
      cat("Permutation progress bar:")
      cat("\n")
    }

    for (i in 1:n_permutations){

      permutation_index = sample(x = 1:t^2,size = t^2,replace = FALSE)

      permuted_response = aligned_response[permutation_index] + row.col.effects
      permuted_row.col.effects = row_col_effect(to_align = permuted_response, rows = block1,
                                                cols = block2)

      permuted_aligned_response = permuted_response - permuted_row.col.effects


      r_ij_perm = rank(permuted_aligned_response)
      R_i.._perm = tapply(X = r_ij_perm, INDEX = treatment, sum)
      Z_i_perm = f*(R_i.._perm - sum(R_i.._perm)*p_i)/sqrt(Var)

      if(mean(diff(r_ij_perm)) == 0){
        G_O_perm = 0
      } else {
        G_O_perm = sum(Z_i_perm^2/p_i)
      }

      ARL_stat_perm = G_O_perm

      ARL_perm_exceedances = ARL_perm_exceedances + as.numeric(ARL_stat_perm >= ARL_stat)

      if(n_components > 0){

        # chi-square
        Component_stats_perm_chi2 = (as.vector((polys%*%Z_i_perm)^2))


        # Checking for component exceedances
        Component_exceedances_chi2 = Component_exceedances_chi2 +
          as.numeric(Component_stats_perm_chi2 > Component_stats_chi2)

        chi_remainder_stat_perm = NULL
        if(remainder) {
          chi_remainder_stat_perm = G_O_perm - sum(Component_stats_perm_chi2)
          remainder_exceedances_chi2 = remainder_exceedances_chi2 + as.numeric(chi_remainder_stat_perm >= remainder_stat_chi2)

        }
      }

      if(verbose){
        setTxtProgressBar(pb,i)
      }

    }

    if(verbose){close(pb)}

    p_val_ARL_perm = ARL_perm_exceedances/n_permutations

    Component_pvals_perm_chi2 = Component_exceedances_chi2/n_permutations
    if(remainder) remainder_pval_perm_chi2 = remainder_exceedances_chi2/n_permutations

  }


  # results_table

  all_observed_stats_chi2 = c(ARL_stat, Component_stats_chi2,remainder_stat_chi2)

  all_pvals_CARL_chi2 = c(p_val_CARL, Component_pvals_CARL_chi2, remainder_pval_CARL_chi2)
  all_pvals_ARL_chi2 = c(p_val_ARL, Component_pvals_ARL_chi2, remainder_pval_ARL_chi2)

  all_pvals_perm_chi2 = c(p_val_ARL_perm, Component_pvals_perm_chi2, remainder_pval_perm_chi2)

  results_table = cbind(all_observed_stats_chi2,all_pvals_CARL_chi2,all_pvals_ARL_chi2,all_pvals_perm_chi2)
  col_heading = c("ARL/CARL stat", if(n_permutations > 0) {c("CARL p-value","ARL p-value", "perm p-value")} else
  {c("CARL p-value","ARL p-value")})
  row_heading = c("Overall", if(n_components > 0) { c(paste0("Degree ", 1:n_components),if(remainder) {"Remainder"} else {NULL}) } else {NULL})

  dimnames(results_table)[[1]] = row_heading
  dimnames(results_table)[[2]] = col_heading

  rank_means = R_i../n_i

  rank_info = matrix(nrow = 1,ncol=t)
  rank_info[1,] = R_i../n_i
  if(n_components > 0){rank_info = rbind(rank_info,treatment_scores)}
  rownames(rank_info) = c("Aligned rank means", if(n_components > 0) {"Scores"} else {NULL})
  colnames(rank_info) = treatment_levels

  ret = list(ARL_stat=ARL_stat,
             CARL_df=CARL_df,
             ARL_df=ARL_df,
             p_val_ARL=p_val_ARL,
             p_val_CARL=p_val_CARL,
             p_val_ARL_perm = p_val_ARL_perm,

             Component_stats_chi2 = Component_stats_chi2,
             Component_pvals_CARL_chi2 = Component_pvals_CARL_chi2,
             Component_pvals_ARL_chi2 = Component_pvals_ARL_chi2,
             remainder_stat_chi2 = remainder_stat_chi2,
             remainder_pval_CARL_chi2 = remainder_pval_CARL_chi2,
             remainder_pval_ARL_chi2 = remainder_pval_ARL_chi2,
             Component_pvals_perm_chi2 = Component_pvals_perm_chi2,
             remainder_pval_perm_chi2 = remainder_pval_perm_chi2,

             R_i.. = R_i..,

             y_name = y_name,
             treatment_name = treatment_name,
             block1_name=block1_name,
             block2_name=block2_name,
             treatment_scores = treatment_scores,
             treatment_levels = treatment_levels,
             treatment_scores = treatment_scores,

             sig_digits = sig_digits,
             results_table = results_table,

             rank_info = rank_info
  )

  #return(ret)

  new("carl_test", ret)


}




