#' T_Mu statistic
#'
#' \code{T_Mu} returns the test statistic for an unconditional CMH MS equivalent
#' test of required \code{degree}. This function is used in the
#' \code{unconditional_CMH} function.
#'
#' @param treatment a vector of treatment values.
#' @param response a vector of response values.
#' @param strata a variable defining the strata.
#' @param degree the degree assessment required.
#'
#' @return
#' The function returns the test statistic for an unconditional CMH MS
#' equivalent test of required \code{degree}.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @seealso [unconditional_CMH()]
#'
#' @export
#' @examples
#' attach(job_satisfaction)
#' T_Mu(treatment = income, response = as.numeric(satisfaction),
#' strata = gender, degree = 1)
T_Mu = function(treatment, response, strata = NULL,degree){

  if(is.null(strata)) strata = factor(rep("1",length(treatment)))

  N_ihj = table(treatment,response,strata)
  n_i.j = table(treatment,strata)
  n_.hj = table(response,strata)
  n_..j = table(strata)

  u = degree

  dim_N_ihj = dim(N_ihj)
  a_uj = array(dim = c(dim_N_ihj))
  sqrt_n_i.j = array(dim = c(dim_N_ihj))

  for (j in 1:dim_N_ihj[3]){
    for (i in 1:dim_N_ihj[1]){
      a_uj[i,,j] = orthogonal_scores(response[strata==levels(strata)[j]],u)
    }
  }

  for (j in 1:dim_N_ihj[3]){
    for (h in 1:dim_N_ihj[2]){
      sqrt_n_i.j[,h,j] = sqrt(n_i.j[,j])
    }
  }

  V_uij = matrix(ncol = dim_N_ihj[3],nrow = dim_N_ihj[1])
  for (j in 1:dim_N_ihj[3]){
    V_uij[,j] = rowSums((N_ihj*a_uj/sqrt_n_i.j)[,,j])
  }

  V_ui. = rowSums(V_uij)

  f_j = sqrt(n_i.j)

  I_t = diag(rep(1),dim_N_ihj[1])

  Sigma = matrix(0,nrow = dim_N_ihj[1],ncol = dim_N_ihj[1])
  for (j in 1:dim_N_ihj[3]){
    Sigma = Sigma + (I_t - (f_j[,j]%*%t(f_j[,j]))/n_..j[j])
  }

  ret = V_ui. %*% MASS::ginv(Sigma) %*% V_ui.

  return(ret)

}


