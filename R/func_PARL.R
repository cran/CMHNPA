#' PARL Test
#'
#' \code{PARL} returns the test statistic and p-value for the aligned RL test
#' performed as a permutation test.
#'
#' @details
#' This test is applicable to Latin square designs and is recommended over the
#' RL and ARL test. The CARL test is much faster to run.
#'
#' @param y a numericc vector for the response variable.
#' @param treatment a vector giving the treatment type for the corresponding
#' elements of \code{y}.
#' @param block1 a vector giving the first blocking variable for the
#' corresponding elements of \code{y}.
#' @param block2 a vector giving the second blocking variable for the
#' corresponding elements of \code{y}.
#' @param N_perms The number of permutations to perform.
#' @param components a TRUE or FALSE flag to indicate whether component p-values
#' should be calculated.
#'
#' @return
#' The PARL test statistic together with the associated p-value. Component
#' p-values may also be calculated and shown.

#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @seealso [ARL()] [CARL()]
#'
#' @export
#' @examples
#' attach(peanuts)
#' PARL(y = yield, treatment = treatment, block1 = row, block2 = col,
#' components = TRUE)
PARL = function(y,treatment,block1,block2,N_perms = 1000,components=FALSE){

  y_name = deparse(substitute(y))
  treatment_name = deparse(substitute(treatment))
  block1_name = deparse(substitute(block1))
  block2_name = deparse(substitute(block2))
  treatment_levels = levels(treatment)

  t = sqrt(length(y))
  row.col.effects = row_col_effect(to_align = y, rows = block1,
                                   cols = block2)
  aligned_response = y - row.col.effects

  ranks = rank(aligned_response)
  R_i.. = tapply(X = ranks, INDEX = treatment, sum)

  aligned_rank_sums = R_i..

  if(mean(diff(ranks)) == 0){
    statistic = 0
  } else {
    statistic = ( sum(R_i..^2) - (t^3*(t^2+1)^2)/4 ) / ( sum((ranks^2)/t) - t*(t^2+1)^2/4  )

  }
  if(components){
    c_i = matrix(nrow=t,ncol=t)
    c_i[,1:(t-1)] = poly(1:t,t-1)[,]
    c_i[,t] = 1/sqrt(t)
    M = t(c_i)

    # chi-square
    c = 1/(sum(ranks^2/t) - t*(t^2+1)^2/4)
    Y_i = (R_i.. - t*mean(ranks))*sqrt(c)
    # sum(Y_i^2)

    # F
    Z_i = (R_i.. - t*mean(ranks))/sqrt( t )
    #sum(Z_i^2)

    anova_object = anova(lm(ranks~treatment+block1+block2))
    MSerror = (anova_object$'Mean Sq')[length(anova_object$'Mean Sq')]
    edf = (anova_object$'Df')[length(anova_object$'Df')]
    F_statistic = anova_object$'F value'[1]
    #(sum(Z_i^2)/(t-1))/MSerror

    # component statistics
    chi2_component_statistics_observed = (M[1,]%*%Y_i)^2
    F_component_statistics_observed = (t(M[1,])%*%Z_i)^2/MSerror
    for (i in 2:(t-1)){
      chi2_component_statistics_observed[i] = (M[i,]%*%Y_i)^2
      F_component_statistics_observed[i] = (t(M[i,])%*%Z_i)^2/MSerror
    }
  }

  PARL_perms = NULL
  F_perms = NULL
  PARL_chi2_component_perms = matrix(nrow = N_perms,ncol = t-1)
  PARL_F_component_perms = matrix(nrow = N_perms,ncol = t-1)

  for (L in 1:N_perms){
    permutation_index = sample(x = 1:t^2,size = t^2,replace = FALSE)

    permuted_response = aligned_response[permutation_index] + row.col.effects
    permuted_row.col.effects = row_col_effect(to_align = permuted_response, rows = block1,
                                     cols = block2)

    permuted_aligned_response = permuted_response - permuted_row.col.effects

    ranks = rank(permuted_aligned_response)
    R_i.. = tapply(X = ranks, INDEX = treatment, sum)

    if(mean(diff(ranks)) == 0){
      permuted_statistic = 0
    } else {
      permuted_statistic = ( sum(R_i..^2) - (t^3*(t^2+1)^2)/4 ) / ( sum((ranks^2)/t) - t*(t^2+1)^2/4  )

    }
    if(components){
      # chi-square
      c = 1/(sum(ranks^2/t) - t*(t^2+1)^2/4)
      Y_i = (R_i.. - t*mean(ranks))*sqrt(c)
      # sum(Y_i^2)

      # F
      Z_i = (R_i.. - t*mean(ranks))/sqrt( t )
      #sum(Z_i^2)

      anova_object = anova(lm(ranks~treatment+block1+block2))
      MSerror = (anova_object$'Mean Sq')[length(anova_object$'Mean Sq')]
      edf = (anova_object$'Df')[length(anova_object$'Df')]
      F_statistic_permuted = anova_object$'F value'[1]
      #(sum(Z_i^2)/(t-1))/MSerror

      # component statistics
      chi2_component_statistics_permuted = (M[1,]%*%Y_i)^2
      F_component_statistics_permuted = (t(M[1,])%*%Z_i)^2/MSerror
      for (i in 2:(t-1)){
        chi2_component_statistics_permuted[i] = (M[i,]%*%Y_i)^2
        F_component_statistics_permuted[i] = (t(M[i,])%*%Z_i)^2/MSerror
      }
    }

    #PARL_perms[L] = ARL(permuted_response,treatment,block1,block2)$statistic

    PARL_perms[L] = permuted_statistic
    # component statistics
    if(components){
      PARL_chi2_component_perms[L,] = chi2_component_statistics_permuted
      PARL_F_component_perms[L,] = F_component_statistics_permuted
      F_perms[L] = F_statistic_permuted
    }
  }

  pvalue = mean(PARL_perms >= statistic)

  results_table = NULL
  if(components) {

    F_pvalue = mean(F_perms >= F_statistic)

    chi2_pvalue_components = colMeans(t(t(PARL_chi2_component_perms) >= chi2_component_statistics_observed))
    F_pvalue_components = colMeans(t(t(PARL_F_component_perms) >= F_component_statistics_observed))

    results_table = matrix(nrow = t, ncol=4)
    dimnames(results_table)[[1]] = c("Overall", paste0("Degree ", 1:(t-1)))
    dimnames(results_table)[[2]] = c("Chi2 Obs stats", "p-value", "F Obs stats", "p-value")

    results_table[,1] = c(statistic,chi2_component_statistics_observed)
    results_table[,2] = c(pvalue,chi2_pvalue_components)
    results_table[,3] = c(F_statistic,F_component_statistics_observed)
    results_table[,4] = c(F_pvalue,F_pvalue_components)

  }


  ret = list(statistic=statistic,pvalue=pvalue,
             y_name = y_name, treatment_name = treatment_name,
             block1_name = block1_name, block2_name = block2_name,
             N_perms = N_perms, results_table=results_table,components=components,
             treatment_levels=treatment_levels,aligned_rank_sums=aligned_rank_sums
  )
  new("PARL_test", ret)


}

