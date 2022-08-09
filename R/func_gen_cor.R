#' gen_cor Test
#'
#' \code{gen_cor} returns the generalised correlations and associated p-values
#' together with tests of normality.
#'
#' @details
#' This function calculates up to three way generalised correlations. The
#' function calculates three tests by default to test if the correlations are
#' statistically significantly different from 0 with an option to run permuation
#' testing.
#'
#' @param x a numeric vector or factor, commonly a response variable.
#' @param y a numeric vector or factor, commonly a treatment variable.
#' @param z an optional numeric vector or factor, commonly a block variable.
#' @param U the maximum degree of correlation relating to the variable \code{x}.
#' @param V the maximum degree of correlation relating to the variable \code{y}.
#' @param W the maximum degree of correlation relating to the variable \code{z}.
#' Required when \code{z} is included.
#' @param x_scores optional scores related to the variable \code{x}.
#' @param y_scores optional scores related to the variable \code{y}.
#' @param z_scores optional scores related to the variable \code{z}.
#' @param n_perms an optional numeric value indicating the number of
#' permutations required.
#' @param perms_info a TRUE of FALSE flag to indicate whether information
#' regarding the progress on the number of permutations should be printed.
#' @param rounding the number of decimal places the output should be rounded to.
#' The default is 4.
#'
#' @return
#' This function calculates the generalised correlations for up to three input
#' variables.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#' @importFrom stats pnorm shapiro.test t.test wilcox.test
#' @examples
#' attach(intelligence)
#' gen_cor(x = rank(score), y = age, U = 2, V = 2)

gen_cor = function(x, y, z = NULL, U, V, W = NULL, x_scores = NULL, y_scores = NULL, z_scores = NULL, n_perms = 0, perms_info = FALSE, rounding = 4){

  treatment_name = deparse(substitute(x))
  response_name = deparse(substitute(y))
  strata_name = deparse(substitute(z))

  if(   !( is.numeric(x) | is.factor(x) ) | !( is.numeric(y) | is.factor(y) ) | !( is.numeric(z) | is.factor(z) | is.null(z) ) ) {
    stop("The variables x, y, and z need to be either factors or numeric variables.")
  }


  if( (is.null(z) & !is.null(W)) | (!is.null(z) & is.null(W)) ) {stop("if z not used, W should be left out, OR, if z is used W is required")}
  if( (is.null(z) & !is.null(z_scores)) ) {stop("z_scores should only be used if z is used")}

  if(!is.factor(x) & !is.null(x_scores)) stop("x_scores is to rescore a categorical variable. If x is a numerical variable, simply change values in that variable to rescore. Or set as a factor and use scores.")
  if(!is.factor(y) & !is.null(y_scores)) stop("y_scores is to rescore a categorical variable. If y is a numerical variable, simply change values in that variable to rescore. Or set as a factor and use scores.")
  if(!is.factor(z) & !is.null(z_scores)) stop("z_scores is to rescore a categorical variable. If z is a numerical variable, simply change values in that variable to rescore. Or set as a factor and use scores.")


  # Rescale the x variable
  if(is.factor(x) & !is.null(x_scores)){
    x_num = as.numeric(x)
    for (i in 1:length(levels(x))){
      x_num[levels(x)[i] == x] = x_scores[i]
    }
    x = x_num
  }

  # Rescale the y variable
  if(is.factor(y) & !is.null(y_scores)){
    y_num = as.numeric(y)
    for (i in 1:length(levels(y))){
      y_num[levels(y)[i] == y] = y_scores[i]
    }
    y = y_num
  }

  # Rescale the z variable
  if(is.factor(z) & !is.null(z_scores)){
    z_num = as.numeric(z)
    for (i in 1:length(levels(z))){
      z_num[levels(z)[i] == z] = z_scores[i]
    }
    z = z_num
  }


  n = length(x)

  x = as.numeric(x)
  y = as.numeric(y)
  if(is.null(z)) {
    z = rep(1,n)
    W = 0
  } else {
    z = as.numeric(z)
  }

  if(n != length(y) | n != length(z) | length(y) != length(z)){
    stop("x, y, and z must be equal length")
  }

  if( length(unique(x)) < U | length(unique(y)) < V | length(unique(z)) < W ){
    stop("U, V, and W must be less than the number of unique values in x, y, and z respectively")
  }

  V_uvw = array(dim = c(U,V,W+1))
  p.val_chi2 = array(dim = c(U,V,W+1))
  p.val_t = array(dim = c(U,V,W+1))
  p.val_Wilcoxon = array(dim = c(U,V,W+1))
  p.val_Shapiro = array(dim = c(U,V,W+1))

  stats_chi2 = array(dim = c(U,V,W+1))
  stats_t = array(dim = c(U,V,W+1))
  stats_Wilcoxon = array(dim = c(U,V,W+1))

  poly_x = cbind(poly(x,U)[,])*sqrt(n)
  U = 1:U
  poly_y = cbind(poly(y,V)[,])*sqrt(n)
  V = 1:V

  if(W != 0) {
    poly_z = cbind(rep(1/sqrt(n),n),poly(z,W)[,])*sqrt(n)
    W = 0:W
  } else {
    poly_z = cbind(rep(1/sqrt(n),n))*sqrt(n)
  }

  # Calculate V_uvw and p-values
  for (u in U){
    for (v in V){
      for (w in W){
        abc = poly_x[,u]*poly_y[,v]*poly_z[,w+1]
        V_uvw[u,v,w+1] = sum(abc/sqrt(n))

        t_test_output = t.test(x = abc)
        wilcox_output = suppressWarnings(
          wilcox.test(abc, mu = 0, alternative = "two.sided",correct = FALSE)
        )

        stats_chi2[u,v,w+1] = (mean(abc)/(1/sqrt(n)))^2
        stats_t[u,v,w+1] = t_test_output$statistic
        stats_Wilcoxon[u,v,w+1] = wilcox_output$statistic


        p.val_chi2[u,v,w+1] = 2*pnorm(q = abs(mean(abc)/(1/sqrt(n))), lower.tail = F)
        p.val_t[u,v,w+1] = t_test_output$p.value
        p.val_Wilcoxon[u,v,w+1] = wilcox_output$p.value
        p.val_Shapiro[u,v,w+1] = shapiro.test(abc)$p.value
      }
    }
  }

  # Perform the permutation test if required

  if(n_perms > 0){
    if(perms_info) cat("Performing permutation testing... this may take a while... \n")

    observed_t = stats_t

    for (J in 1:n_perms){
      if(J == 1) {
        exceeds_t = array(0,dim(observed_t))
      }

      # Permute the response and then calculate the permuted stat
      permuted_poly_x = cbind(poly(sample(x,replace = F),max(U))[,])*sqrt(n)

      permuted_t = array(dim = dim(V_uvw))
      # Calculate the permuted statistic
      for (u in U){
        for (v in V){
          for (w in W){
            perm_abc = permuted_poly_x[,u]*poly_y[,v]*poly_z[,w+1]

            t_test_output = t.test(x = perm_abc)
            permuted_t[u,v,w+1] = t_test_output$statistic

          }
        }
      }
      if(perms_info) {
        if(J%%200 == 0) cat(paste0(J, " permutations completed of ", n_perms,"\n"))
      }
      exceeds_t = exceeds_t + (abs(permuted_t) > abs(observed_t))
    }
    p.val_perm = exceeds_t/n_perms
  }

  # Create the table of p-values and correlations
  if(n_perms > 0) {
    output_table = array(dim = c(6,max(U)*max(V),max(W)+1))
  } else {
    output_table = array(dim = c(5,max(U)*max(V),max(W)+1))
  }

  for (w in W+1){
    output_row_corr = output_row_norm = output_row_ttes = output_row_wilc =
      output_row_shap = output_row_perm = NULL

    for (u in U){
      for (v in V){
        output_row_corr = c(output_row_corr, V_uvw[u,v,w]/sqrt(n))
        output_row_norm = c(output_row_norm, p.val_chi2[u,v,w])
        output_row_ttes = c(output_row_ttes, p.val_t[u,v,w])
        output_row_wilc = c(output_row_wilc, p.val_Wilcoxon[u,v,w])
        if(n_perms > 0) output_row_perm = c(output_row_perm, p.val_perm[u,v,w])
        output_row_shap = c(output_row_shap, p.val_Shapiro[u,v,w])
      }
    }
    output_table[1,,w] = output_row_corr
    output_table[2,,w] = output_row_norm
    output_table[3,,w] = output_row_ttes
    output_table[4,,w] = output_row_wilc
    if(n_perms > 0){
      output_table[5,,w] = output_row_perm
      output_table[6,,w] = output_row_shap
    } else {
      output_table[5,,w] = output_row_shap
    }

  }

  output_col_names = NULL
  for (u in U){
    for (v in V){
      output_col_names = c(output_col_names,(paste0("V_",u,v)))
    }
  }
  dimnames(output_table)[[2]] = output_col_names
  if(n_perms > 0) {
    dimnames(output_table)[[1]] = c("correlations", "chi-squared", "t-test",
                                    "Wilcoxon", "Permutation", "Shapiro-Wilk")
  } else {
    dimnames(output_table)[[1]] = c("correlations", "chi-squared", "t-test", "Wilcoxon", "Shapiro-Wilk")
  }
  dimnames(output_table)[[3]] = paste0("w = ",0:max(W))



  ret = list(correlations = V_uvw/sqrt(n),V_uvw = V_uvw,
             treatment_name = treatment_name, response_name = response_name,
             strata_name = strata_name,
             p.val_chi2=p.val_chi2,p.val_t=p.val_t,p.val_Wilcoxon=p.val_Wilcoxon,
             p.val_Shapiro=p.val_Shapiro,output_table=output_table,
             stats_chi2=stats_chi2,stats_t=stats_t,stats_Wilcoxon=stats_Wilcoxon,
             rounding=rounding)
  #return(ret)
  new("gen_cor_test", ret)

}


