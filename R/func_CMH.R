#' CMH Test
#'
#' \code{CMH} returns the test statistics and p-values for the four CMH tests.
#'
#' @details
#' Provided the required information is used in the function, the function will
#' return all four CMH test results.
#'
#' @param treatment a factor vector representing the treatment applied.
#' @param response a factor vector giving the response category for the
#' corresponding elements of \code{treatment}.
#' @param strata a factor vector giving the strata or block for the
#' corresponding elements of \code{treatment} and \code{response}.
#' @param a_ij a t x b matrix of treatment scores. The matrix allows for
#' different scores to be used over different strata. If a t x 1 vector of
#' scores is provided, it is assumed that the scores are the same across strata
#' and a warning provided.
#' @param b_hj a c x b matrix of response scores. The matrix allows for
#' different scores to be used over different strata. If a c x 1 vector of
#' scores is provided, it is assumed that the scores are the same across strata
#' and a warning provided.
#' @param test_OPA TRUE or FALSE flag to include the calculation of the OPA
#' test values.
#' @param test_GA TRUE or FALSE flag to include the calculation of the GA
#' test values.
#' @param test_MS TRUE or FALSE flag to include the calculation of the MS
#' test values. If response scores are not included, this test will not be
#' performed.
#' @param test_C TRUE or FALSE flag to include the calculation of the C
#' test values. If response scores and treatment scores are not included, this
#' test will not be performed.
#' @param cor_breakdown TRUE or FALSE flag to indicate if a correlation
#' breakdown over the strata is required.
#'
#' @return The CMH test results for the four tests assuming the required
#' information is supplied.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#'
#' @export
#' @examples
#' CMH(treatment = marriage$religion, response = marriage$opinion,
#' strata = marriage$education, test_OPA = FALSE, test_MS = FALSE)
CMH = function(treatment, response, strata = NULL, a_ij = NULL, b_hj = NULL,
               test_OPA = TRUE, test_GA = TRUE, test_MS = TRUE, test_C = TRUE,
               cor_breakdown = TRUE){

  # tests to perform

  if(is.null(a_ij) & is.null(b_hj) & test_MS & test_C) {
    test_MS = F
    test_C = F
    warning("Treatment and response scores not provided. Not performing Mean Score and Correlation tests.")
  } else {
    if(is.null(a_ij) & test_C){
      test_C = F
      warning("Treatment scores not provided. Not performing Correlation test.")
    }
    if(is.null(b_hj) & test_MS){
      stop("a_ij provided without b_hj")
    }
  }

  if(!test_OPA & !test_MS & !test_GA & !test_C) stop("You have asked to perform no tests, silly...")

  # factor errors
  factor_error = "treatment, response, and strata need to be factors"
  if(!is.factor(treatment) | !is.factor(response)) stop(factor_error)

  if(is.null(strata)){
    strata = factor(rep("strata",length(response)))
    if(is.vector(a_ij)) a_ij = matrix(a_ij,ncol = 1)
    if(is.vector(b_hj)) b_hj = matrix(b_hj,ncol = 1)
  } else {
    if(!is.factor(strata)) stop(factor_error)
  }

  levels_treatment = levels(treatment)
  levels_response = levels(response)
  levels_strata = levels(strata)
  t = length(levels_treatment)
  c = length(levels_response)
  b = length(levels_strata)

  if(b > 1){
    if(is.vector(a_ij)) {
      a_ij = matrix(rep(a_ij,b),ncol = b, byrow = F)
      warning("A vector of treatment scores was supplied. Therefore assuming equal treatment scores across strata",call. = F)
    }
    if(is.vector(b_hj)) {
      b_hj = matrix(rep(b_hj,b),ncol = b, byrow = F)
      warning("A vector of response scores was supplied. Therefore assuming equal response scores across strata",call. = F)
    }
  }

  if(test_MS){
    if(dim(b_hj)[1] != c | dim(b_hj)[2] != b){
      stop("Treatment score dimensions do not match the number of treatment or strata levels")
    }
  }

  if(test_C){
    if(dim(a_ij)[1] != t | dim(a_ij)[2] != b | dim(b_hj)[1] != c | dim(b_hj)[2] != b){
      stop("Treatment or repsonse score dimensions do not match the number of treatment or strata levels")
    }
  }

  # common calculations
  N_ihj = table(treatment,response,strata)
  n_i.j = table(treatment,strata)
  n_.hj = table(response,strata)
  n_..j = table(strata)

  p_i.j = n_i.j/matrix(rep(n_..j,t), nrow = t, byrow = TRUE)
  p_.hj = n_.hj/matrix(rep(n_..j,c), nrow = c, byrow = TRUE)

  V_Tj = V_Cj = list()
  for (j in 1:b){
    V_Tj[[j]] = diag(p_i.j[,j]) - p_i.j[,j]%*%t(p_i.j[,j])
    V_Cj[[j]] = diag(p_.hj[,j]) - p_.hj[,j]%*%t(p_.hj[,j])
  }

  S_OPA = S_GA = S_MS = S_C = df_OPA = df_GA = df_MS = df_C =
    p_val_OPA = p_val_GA = p_val_MS = p_val_C = NULL

  ################################################################################
  # OPA
  ################################################################################

  if(test_OPA){

    X2_sum = 0
    for (j in 1:b){
      for (i in 1:t){
        for (h in 1:c){
          X2_sum = X2_sum +
            ((n_..j-1)/n_..j)[j]*( (N_ihj[i,h,j] - n_i.j[i,j]*n_.hj[h,j]/n_..j[j])^2 ) /
            ( n_i.j[i,j]*n_.hj[h,j]/n_..j[j] )
        }
      }
    }

    if(is.nan(unname(X2_sum))){
      warning("S_OPA is not defined as there are response categories with zero counts. Not performing this test.",call. = F)
      test_OPA = FALSE
    } else {
      S_OPA = unname(X2_sum)
      df_OPA = b*(c-1)*(t-1)
      p_val_OPA = pchisq(q = S_OPA,df = df_OPA,lower.tail = F)
    }
  }


  ################################################################################
  # GA
  ################################################################################

  if(test_GA){
    U. = rowSums(matrix(as.vector(table(response,treatment,strata)),ncol = b))

    EU. = matrix(nrow = t*c, ncol=b)

    for (j in 1:b){
      temp_col = NULL
      for (i in 1:t){
        for (h in 1:c)

          temp_col = c(temp_col,n_i.j[i,j]*n_.hj[h,j]/n_..j[j])


      }
      EU.[,j] = temp_col
    }
    EU. = rowSums(EU.)

    CovU. = 0
    for (j in 1:b){
      CovU. = CovU. + n_..j[j]^2/(n_..j[j] - 1) * kronecker(V_Tj[[j]],V_Cj[[j]])
    }

    S_GA = t(U. - EU.)%*%(MASS::ginv(CovU.))%*%(U. - EU.)
    df_GA = (c-1)*(t-1)
    p_val_GA = pchisq(q = S_GA,df = df_GA,lower.tail = F)

  }

  ################################################################################
  # MS
  ################################################################################

  if(test_MS){
    # M
    M = matrix(ncol=b,nrow=t)
    for (j in 1:b){
      M[,j] = rowSums(matrix(b_hj[,j],nrow = t, ncol=c,byrow = T)*N_ihj[,,j])
    }
    M = rowSums(M)

    # EM
    EM = rowSums(n_i.j * matrix(colSums(b_hj * n_.hj/matrix(rep(n_..j,c), ncol = j, byrow = T)), ncol=j,nrow = t, byrow = TRUE))

    # CovM
    S2_j = ( n_..j * colSums(b_hj^2*n_.hj) - colSums(b_hj*n_.hj)^2 ) / (n_..j - 1)

    CovM = matrix(0,nrow=t,ncol=t)
    for (j in 1:b){
      CovM = CovM + S2_j[j] * V_Tj[[j]]
    }


    S_MS = t(M - EM) %*% (MASS::ginv(CovM)) %*% (M - EM)
    df_MS = t-1
    p_val_MS = pchisq(q = S_MS,df = df_MS,lower.tail = F)

  }


  ################################################################################
  # C
  ################################################################################

  output_table_cors = NULL

  if(test_C){

    S_XX = colSums(a_ij^2*n_i.j) - colSums(a_ij*n_i.j)^2/n_..j
    S_YY = colSums(b_hj^2*n_.hj) - colSums(b_hj*n_.hj)^2/n_..j

    quotient = colSums(a_ij*n_i.j)*colSums(b_hj*n_.hj)/n_..j
    S_XY = rep(NA,b)

    for (j in 1:b){
      sum_sum = 0
      for (i in 1:t){
        for (h in 1:c){
          sum_sum = sum_sum + a_ij[i,j]*b_hj[h,j]*sum(treatment==levels_treatment[i] &
                                                        response==levels_response[h] &
                                                        strata==levels_strata[j])
        }
      }
      S_XY[j] = sum_sum - quotient[j]

    }
    names(S_XY) = levels_strata

    if(b == 1){
      S_XX = unname(S_XX)
      S_YY = unname(S_YY)
      S_XY = unname(S_XY)
    }

    r_P = S_XY/sqrt(S_XX*S_YY)
    S_C = sum(S_XY)^2 / sum(S_XX*S_YY/(n_..j-1))
    S_C_per_strata = S_XY^2 / (S_XX*S_YY/(n_..j-1))

    df_C = 1
    p_val_C = pchisq(q = S_C,df = df_C,lower.tail = F)
    p_val_C_per_strata = pchisq(q = S_C_per_strata,df = df_C,lower.tail = F)

    # Create the table of correlation related values
    output_table_cors = array(dim = c(length(levels_strata),6))
    dimnames(output_table_cors)[[1]] = levels_strata
    dimnames(output_table_cors)[[2]] = c("S_C", "p-value", "r_P", "S_XX", "S_XY", "S_YY")

    output_table_cors[,1] = S_C_per_strata
    output_table_cors[,2] = p_val_C_per_strata
    output_table_cors[,3] = r_P
    output_table_cors[,4] = S_XX
    output_table_cors[,5] = S_XY
    output_table_cors[,6] = S_YY

  }


  # Create the main output table
  output_table_main = array(dim = c(test_OPA+test_GA+test_MS+test_C,3))

  dimnames(output_table_main)[[1]] = c("Overall Partial Association",
                                       "General Association","Mean Score","Correlation")[c(test_OPA,test_GA,test_MS,test_C)]
  dimnames(output_table_main)[[2]] = c("S", "df", "p-value")


  output_table_main[,1] = c(S_OPA,S_GA,S_MS,S_C)
  output_table_main[,2] = c(df_OPA,df_GA,df_MS,df_C)
  output_table_main[,3] = c(p_val_OPA,p_val_GA,p_val_MS,p_val_C)

  ret = list(test_OPA = test_OPA,test_GA = test_GA,test_MS = test_MS,test_C = test_C,
             output_table_main=output_table_main,levels_strata=levels_strata,
             output_table_cors=output_table_cors,cor_breakdown=cor_breakdown)

  new("CMH_test", ret)
}
