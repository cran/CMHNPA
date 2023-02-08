#' Durbin Test
#'
#' \code{durbin} returns the results of Durbin's Rank Sum test.
#'
#' @details
#' The test is a generalisation of Friedman's test that can be applied to BIBD.
#'
#'
#' @param y a numeric vector for the response variable.
#' @param groups a vector giving the group for the corresponding elements of
#' \code{y}.
#' @param blocks a vector giving the block for the corresponding elements of
#' \code{y}.
#' @param n_components the number of polynomial components you wish to test. The maximum number of components is the number of groups less one. If the number of components requested is less than \code{t-2}, a remainder component is created.
#' @param n_permutations the number of permutations you wish to run.
#' @param group_scores the scores to be applied to the groups. If not declared these will be set automatically and should be checked.
#' @param sig_digits the number of significant digits the output should show.
#' @param verbose flag for turning on the status bar for permutation tests.
#'
#' @return The Durbin test adjusted for tied results.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @importFrom stats anova lm pf poly
#' @export
#' @examples
#' attach(icecream)
#' durbin(y = rank, groups = variety, blocks = judge)
durbin = function(y, groups, blocks, n_components = 0,
                          n_permutations = 0, group_scores = NULL,
                          sig_digits = 4, verbose = FALSE){

  if( length(unique(apply(X = cbind(y,groups,blocks),MARGIN = 2,length))) != 1 ){
    stop("Supplied vectors y, groups, blocks should be the same lengths.")
  }

  if(!is.factor(groups) | !is.factor(blocks)) stop("Both groups and blocks need to be factors")

  if( (n_permutations < 100) & (n_permutations > 0) ) stop("Number of permutations are too low. Set this to a number as large as you have time to wait for the results.")

  if(!is.numeric(y)){
    warning("Response is not numeric, attempting to coerce to numeric with as.numeric(). Check that as.numeric(response) results in a suitable coercion.")
    y = as.numeric(y)
  }

  groups_name = deparse(substitute(groups))
  response_name = deparse(substitute(y))
  blocks_name = deparse(substitute(blocks))
  groups_levels = levels(groups)
  blocks_levels = levels(blocks)

  t = length(unique(groups))
  b = length(unique(blocks))
  b_j = table(blocks)

  if(!is.null(group_scores) & (length(group_scores) != t)) stop("An incorrect number of scores are provided!")

  if( n_components == (t - 2) ) {
    warning("The number of components you have requested will result in a remainder term that is made up of one component. Resetting n_components to remove the remainder term.")
    n_components = t-1
  }

  if(mean(diff(table(blocks))) != 0){
    stop("The design is not balanced. Durbin's test is not appropriate.")
  } else {
    k = unname(table(blocks)[1])
  }
  if(mean(diff(table(groups))) != 0){
    stop("The design is not balanced. Durbin's test is not appropriate.")
  } else {
    r = unname(table(groups)[1])
  }

  n_i = tapply(X = groups,INDEX = groups,FUN = length)
  n_i[is.na(n_i)] = 0
  n = sum(n_i)
  p_i = n_i/n

  y_ij = matrix(nrow = t, ncol = b)
  for (i in 1:n){
    y_ij[(1:t)[groups_levels == groups[i]],(1:b)[blocks_levels == blocks[i]]] =
      y[i]
  }

  r_ij = apply(X = t(y_ij),MARGIN = 1,FUN = rank,na.last = "keep")

  R_i. = rowSums(x = r_ij, na.rm = TRUE)


  # chi
  D_A = (t-1)*( sum(R_i.^2) - r*b*k*(k+1)^2/4 )/(
    sum(r_ij^2,na.rm = TRUE) - b*k*(k+1)^2/4
  )
  df = t-1
  D_A_p_val = pchisq(q = D_A, df = df, lower.tail = FALSE)

  # F

  y_anova = as.vector(r_ij)
  groups_anova = factor(rep(1:dim(r_ij)[1],dim(r_ij)[2]))
  blocks_anova = factor(rep(1:dim(r_ij)[2],each = dim(r_ij)[1]))

  anova_object = Anova(lm(y_anova~groups_anova+blocks_anova),type = 3)
  SSgroup = anova_object$`Sum Sq`[2]
  MSerror = tail(x = anova_object$'Sum Sq',1)/tail(x = anova_object$'Df',1)
  edf = tail(anova_object$'Df',1)

  F_stat = anova_object$`F value`[2]
  p_val_F = anova_object$`Pr(>F)`[2]


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

    if( is.null(group_scores)) {
      warning("Components are requested and no group scores are given. Using default scores as a result of as.numeric(factor(levels(groups))). Check that these are correct. The factor may need to have the levels reordered to the correct order.")
      group_scores = as.numeric(factor(levels(groups)))
    }

    S_j = 1/b_j * colSums(r_ij^2, na.rm=TRUE) - 1/b_j^2*colSums(r_ij, na.rm=TRUE)^2
    E_Ri = rowSums(t(as.matrix(table(blocks,groups))*as.vector(colSums(r_ij, na.rm = TRUE)/b_j)))
    f = sqrt((t-1)/(k*t))
    Z_i = f*(R_i. - E_Ri)/sqrt(sum(S_j))

    polys = Poly(x = group_scores, p = p_i)[2:(n_components+1),]
    #sum(Z_i^2/p_i)
    #(n*f^2*sum((R_i. - E_Ri)^2/n_i))/sum(S_j)
    #sum((polys%*%Z_i)^2)

    G_WB = sum(Z_i^2/p_i)

    # chi-square
    Component_stats_chi2 = (as.vector((polys%*%Z_i)^2))
    Component_pvals_chi2 = pchisq(q = Component_stats_chi2,df = 1,lower.tail = FALSE)


    # F

    Y_i = sqrt(p_i)*(R_i. - r*(k+1)/2)*sqrt( k/(t*r*(k-1)/(t-1)) )

    #SSgroup
    #sum(Y_i^2/p_i)
    #sum((polys%*%Y_i)^2)

    Component_stats_F = as.vector((polys%*%Y_i)^2)/MSerror
    Component_pvals_F = pf(q = Component_stats_F,df1 = 1, df2 = edf, lower.tail = FALSE)


    # Remainder

    if(n_components < t-1) {
      remainder = TRUE
      remainder_stat_chi2 = G_WB - sum(Component_stats_chi2)
      remainder_pval_chi2 = pchisq(q = remainder_stat_chi2,df = t-n_components-1,lower.tail = FALSE)

      remainder_stat_F = ((SSgroup - sum(as.vector((polys%*%Y_i)^2)))/(t-n_components-1))/MSerror
      remainder_pval_F = pf(q = remainder_stat_F,df1 = t-n_components-1, df2 = edf, lower.tail = FALSE)

    }

  }


  #
  # Permutations
  #

  D_A_p_val_perm = p_val_perm_F = NULL
  Component_pvals_perm_chi2 = remainder_pval_perm_chi2 = NULL
  Component_pvals_perm_F = remainder_pval_perm_F = NULL
  if( n_permutations > 0 ){

    D_A_perm_exceedances = F_perm_exceedances = 0
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
      # permute the score within blocks to preserve the block effect

      r_ij_perm = matrix(nrow = t, ncol = b)
      for (j in 1:b){
         r_ij_perm[!is.na(r_ij[,j]),j] = sample(r_ij[!is.na(r_ij[,j]),j])
      }

      R_i._perm = rowSums(r_ij_perm, na.rm = TRUE)

      # chi
      D_A_perm = (t-1)*( sum(R_i._perm^2,na.rm = TRUE) - r*b*k*(k+1)^2/4 )/(
        sum(r_ij_perm^2,na.rm = TRUE) - b*k*(k+1)^2/4
      )


      # F
      y_anova_perm = as.vector(r_ij_perm)
      groups_anova_perm = factor(rep(1:dim(r_ij_perm)[1],dim(r_ij_perm)[2]))
      blocks_anova_perm = factor(rep(1:dim(r_ij_perm)[2],each = dim(r_ij_perm)[1]))

      anova_object_perm = Anova(lm(y_anova_perm~groups_anova_perm+blocks_anova_perm),type=3)
      SSgroup_perm = anova_object_perm$`Sum Sq`[2]
      MSerror_perm = tail(x = anova_object_perm$'Sum Sq',1)/tail(x = anova_object$'Df',1)
      # edf = tail(anova_object_perm$'Df',1) # Not needed

      F_stat_perm = anova_object_perm$`F value`[2]


      D_A_perm_exceedances = D_A_perm_exceedances + as.numeric(D_A_perm >= D_A)
      F_perm_exceedances = F_perm_exceedances + as.numeric(F_stat_perm >= F_stat)


      if(n_components > 0){

        S_j_perm = 1/b_j * colSums(r_ij_perm^2,na.rm = TRUE) - 1/b_j^2*colSums(r_ij_perm,na.rm = TRUE)^2
        E_Ri_perm = rowSums(t(as.matrix(table(blocks,groups))*as.vector(colSums(r_ij_perm,na.rm = TRUE)/b_j)))
        Z_i_perm = f*(R_i._perm - E_Ri_perm)/sqrt(sum(S_j_perm))

        #polys = Poly(x = group_scores, p = p_i)[2:(n_components+1),]
        #sum(Z_i_perm^2/p_i)
        #(n*f^2*sum((R_i._perm - E_Ri_perm)^2/n_i))/sum(S_j_perm)
        #sum((polys%*%Z_i_perm)^2)

        G_WB_perm = sum(Z_i_perm^2/p_i)

        # chi-square
        Component_stats_perm_chi2 = (as.vector((polys%*%Z_i_perm)^2))

        # F

        Y_i_perm = sqrt(p_i)*(R_i._perm - r*(k+1)/2)*sqrt( k/(t*r*(k-1)/(t-1)) )

        #SSgroup_perm
        #sum(Y_i_perm^2/p_i)
        #sum((polys%*%Y_i_perm)^2)

        Component_stats_perm_F = as.vector((polys%*%Y_i_perm)^2)/MSerror_perm


        # Checking for component exceedances
        Component_exceedances_chi2 = Component_exceedances_chi2 +
          as.numeric(Component_stats_perm_chi2 > Component_stats_chi2)
        Component_exceedances_F = Component_exceedances_F +
          as.numeric(Component_stats_perm_F > Component_stats_F)

        # Remainders
        remainder_stat_perm_chi2 = NULL
        remainder_stat_perm_F = NULL
        if(remainder) {
          remainder_stat_perm_chi2 = G_WB_perm - sum(Component_stats_perm_chi2)
          remainder_stat_perm_F = ((SSgroup_perm - sum(as.vector((polys%*%Y_i_perm)^2)))/(t-n_components-1))/MSerror_perm

          remainder_exceedances_chi2 = remainder_exceedances_chi2 + as.numeric(remainder_stat_perm_chi2 > remainder_stat_chi2)
          remainder_exceedances_F = remainder_exceedances_F + as.numeric(remainder_stat_perm_F > remainder_stat_F)
        }

      }

      if(verbose){
        setTxtProgressBar(pb,i)
      }
    }

    if(verbose){close(pb)}

    D_A_p_val_perm = D_A_perm_exceedances/n_permutations

    p_val_perm_F = F_perm_exceedances/n_permutations

    Component_pvals_perm_chi2 = Component_exceedances_chi2/n_permutations
    Component_pvals_perm_F = Component_exceedances_F/n_permutations

    if(remainder) {
      remainder_pval_perm_chi2 = remainder_exceedances_chi2/n_permutations
      remainder_pval_perm_F = remainder_exceedances_F/n_permutations
    }

  }

  # results_table

  all_observed_stats_chi2 = c(D_A, Component_stats_chi2,remainder_stat_chi2)
  all_observed_stats_F = c(F_stat, Component_stats_F,remainder_stat_F)

  all_pvals_chi2 = c(D_A_p_val, Component_pvals_chi2, remainder_pval_chi2)
  all_pvals_F = c(p_val_F, Component_pvals_F, remainder_pval_F)

  all_pvals_perm_chi2 = c(D_A_p_val_perm, Component_pvals_perm_chi2, remainder_pval_perm_chi2)
  all_pvals_perm_F = c(p_val_perm_F, Component_pvals_perm_F, remainder_pval_perm_F)

  results_table = cbind(all_observed_stats_chi2,all_pvals_chi2,all_pvals_perm_chi2,all_observed_stats_F,all_pvals_F,all_pvals_perm_F)
  col_heading = c("chi2 Obs", if(n_permutations > 0) {c("p-value", "p perm")} else {"p-value"}, "F Obs", if(n_permutations > 0) {c("p-value", "p perm")} else {"p-value"})
  row_heading = c("Overall", if(n_components > 0) { c(paste0("Degree ", 1:n_components),if(remainder) {"Remainder"} else {NULL}) } else {NULL})

  dimnames(results_table)[[1]] = row_heading
  dimnames(results_table)[[2]] = col_heading

  rank_means = R_i./n_i

  rank_info = matrix(nrow = 1,ncol=t)
  rank_info[1,] = R_i./n_i
  if(n_components > 0){rank_info = rbind(rank_info,group_scores)}
  rownames(rank_info) = c("Rank means", if(n_components > 0) {"Scores"} else {NULL})
  colnames(rank_info) = groups_levels

  ret = list(D_A = D_A,
             D_A_p_val = D_A_p_val,

             F_stat = F_stat,
             p_val_F = p_val_F,

             D_A_p_val_perm = D_A_p_val_perm,
             p_val_perm_F = p_val_perm_F,

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

             R_i. = R_i.,

             response_name = response_name,
             groups_name = groups_name,
             group_scores = group_scores,
             groups_levels = groups_levels,

             df=df,
             sig_digits = sig_digits,
             results_table = results_table,

             rank_info = rank_info
  )

  #return(ret)

  new("durbin_test", ret)

}


