#' Unconditional CMH Test
#'
#' \code{unconditional_CMH} returns the test statistics and p-values for the
#' equivalent unconditional CMH tests.
#'
#' @param treatment a factor vector representing the treatment applied.
#' @param response a factor vector giving the response category for the
#' corresponding elements of \code{treatment}.
#' @param strata a factor vector giving the strata or block for the
#' corresponding elements of \code{treatment} and \code{response}.
#' @param U The degree of assessment relating to the treatment.
#' @param V The degree of assessment relating to the response.
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
#' @param test_M TRUE or FALSE flag to include the calculation of the MS
#' test values. If response scores are not included, this test will not be
#' performed.
#' @param test_C TRUE or FALSE flag to include the calculation of the C
#' test values. If response scores and treatment scores are not included, this
#' test will not be performed.
#'
#' @return
#' The unconditional CMH test results.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#'
#' @export
#' @examples
#' attach(job_satisfaction)
#' a_ij = matrix(rep(c(3,10,20,35),2),nrow=4)
#' b_hj = matrix(rep(c(1,3,4,5),2),nrow=4)
#' unconditional_CMH(treatment = income, response = satisfaction,
#' strata = gender, U = 2, V = 2, a_ij = a_ij, b_hj = b_hj)
unconditional_CMH = function(treatment, response, strata = NULL, U = NULL,
                             V = NULL, a_ij = NULL, b_hj = NULL,
                             test_OPA = TRUE, test_GA = TRUE, test_M = TRUE,
                             test_C = TRUE){

  # tests to perform

  if(is.null(U) & is.null(V) & test_M & test_C) {
    test_M = F
    test_C = F
    U = 0
    V = 0
    warning("Treatment and response degrees not provided. Not performing analogues of Mean Score and Correlation tests.")
  } else {
    if(is.null(V) & test_C){
      test_C = F
      V = 0
      warning("Treatment degree not provided. Not performing Correlation tests.")
    }
    if(is.null(V) & test_M){
      stop("U provided without V")
    }
  }


  if(is.null(a_ij) & is.null(b_hj) & test_M & test_C) {
    test_M = F
    test_C = F
    warning("Treatment and response scores not provided. Not performing Mean Score and Correlation tests.")
  } else {
    if(is.null(a_ij) & test_C){
      test_C = F
      warning("Treatment scores not provided. Not performing Correlation test.")
    }
    if(is.null(b_hj) & test_M){
      stop("a_ij provided without b_hj")
    }
  }

  if(!test_OPA & !test_M & !test_GA & !test_C) stop("You have asked to perform no tests, silly...")

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

  if(test_M){
    if(dim(b_hj)[1] != c | dim(b_hj)[2] != b){
      stop("Treatment score dimensions do not match the number of treatment or strata levels")
    }
  }

  if(test_C){
    if(dim(a_ij)[1] != t | dim(a_ij)[2] != b | dim(b_hj)[1] != c | dim(b_hj)[2] != b){
      stop("Treatment or repsonse score dimensions do not match the number of treatment or strata levels")
    }
  }


  # Redefine treatment and response variables based off provided scores
  if(test_C){
    treatment_numerical = as.numeric(treatment)
    for (j in 1:b){
      for (i in 1:t){
        treatment_numerical[ (treatment == levels(treatment)[i]) & (strata == levels(strata)[j]) ] = a_ij[i,j]
      }
    }

    response_numerical = as.numeric(response)
    for (j in 1:b){
      for (h in 1:c){
        response_numerical[ (response == levels(response)[h]) & (strata == levels(strata)[j]) ] = b_hj[h,j]
      }
    }

  } else {
    if(test_M){
      response_numerical = as.numeric(response)
      for (j in 1:b){
        for (h in 1:c){
          response_numerical[ (response == levels(response)[h]) & (strata == levels(strata)[j]) ] = b_hj[h,j]
        }
      }

    }
  }

  # common calculations
  N_ihj = table(treatment,response,strata)
  n_ij. = table(treatment,response)
  n_i.j = table(treatment,strata)
  n_.hj = table(response,strata)
  n_..j = table(strata)


  T_OPA = T_GA = T_M_stats = T_C_stats = df_OPA = df_GA = df_M = df_C =
    p_val_OPA = p_val_GA = p_vals_M = p_vals_C = NULL

  ################################################################################
  # OPA
  ################################################################################

  if(test_OPA){

    X2_sum = 0
    for (j in 1:b){
      for (i in 1:t){
        for (h in 1:c){
          X2_sum = X2_sum +
            ( (N_ihj[i,h,j] - n_i.j[i,j]*n_.hj[h,j]/n_..j[j])^2 ) /
            ( n_i.j[i,j]*n_.hj[h,j]/n_..j[j] )

        }
      }
    }

    if(is.nan(unname(X2_sum))){
      warning("T_OPA is not defined as there are response categories with zero counts. Not performing this test.",call. = F)
      test_OPA = FALSE
    } else {
      T_OPA = unname(X2_sum)
      df_OPA = b*(c-1)*(t-1)
      p_val_OPA = pchisq(q = T_OPA,df = df_OPA,lower.tail = F)
    }
  }


  ################################################################################
  # GA
  ################################################################################

  if(test_GA){

    expected_counts = matrix(rowSums(n_ij.), nrow = t, ncol=c)*matrix(colSums(n_ij.), nrow = t, ncol=c, byrow = T)/sum(n_ij.)
    T_GA = sum(((n_ij. - expected_counts)^2)/expected_counts)

    df_GA = (c-1)*(t-1)
    p_val_GA = pchisq(q = T_GA,df = df_GA,lower.tail = F)

  }

  ################################################################################
  # Mu
  ################################################################################

  if(test_M){
    # M

    # If there are blocks that are not informative, that is they all have the same
    # response category, the function will have problems. Perhaps add a warning for
    # this

    T_M_stats = NULL
    for (u in 1:U){

      T_M_stats[u] = T_Mu(treatment = treatment, response = response_numerical, strata = strata, degree = u)
    }
    df_M = t-1
    p_vals_M = pchisq(q = T_M_stats,df = df_M,lower.tail = F)

  }


  ################################################################################
  # C
  ################################################################################

  if(test_C){

    # If there are blocks that are not informative, that is they all have the same
    # response category, the function will have problems. Perhaps add a warning for
    # this

    split_response = split(x = response_numerical,f = strata)
    split_treatment = split(x = treatment_numerical,f = strata)

    T_C_stats = gen_cor(x = factor(split_response[[1]]),y = factor(split_treatment[[1]]), U = U,V = V,
                        x_scores = b_hj[,1], y_scores = a_ij[,1])$V_uvw

    if(b > 1){
      for (i in 2:b){
        T_C_stats = T_C_stats + gen_cor(x = factor(split_response[[i]]),
                                        y = factor(split_treatment[[i]]), U = U,V = V,
                                        x_scores = b_hj[,1], y_scores = a_ij[,1])$V_uvw
      }
    }

    T_C_stats = T_C_stats^2/b
    df_C = 1
    p_vals_C = pchisq(q = T_C_stats,df = df_C,lower.tail = F)

  }


  # Create the main output table
  output_table_main = array(dim = c(test_OPA+test_GA+sum(rep(test_M,U))+
                                      sum(rep(test_C,U*V)),3))
  cor_names = NULL
  if(test_C){
    for (u in 1:U){
      for (v in 1:V){
        cor_names = c(cor_names,(paste0("T_C",u,v,".")))
      }
    }
  }
  T_M_names = NULL
  if(test_M) T_M_names = paste0("T_M",1:U)


  dimnames(output_table_main)[[1]] = c("T_OPA","T_GA",T_M_names,cor_names)[c(test_OPA,test_GA,rep(test_M,U),rep(test_C,U*V))]
  dimnames(output_table_main)[[2]] = c("T", "df", "p-value")
  output_table_main[,1] = c(T_OPA,T_GA,as.vector(T_M_stats),as.vector(T_C_stats) )
  output_table_main[,2] = c(df_OPA,df_GA,rep(df_M,U),rep(df_C,U*V))
  output_table_main[,3] = c(p_val_OPA,p_val_GA,as.vector(p_vals_M),as.vector(p_vals_C))

  ret = list(test_OPA = test_OPA,test_GA = test_GA,test_M = test_M,test_C = test_C,
             output_table_main=output_table_main,levels_strata=levels_strata)

  new("unconditional_CMH_test", ret)
}
