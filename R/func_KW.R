#' KW
#'
#' \code{KW} returns the test statistic and p-value for the Kruskal-Wallis test
#' adjusted for ties.
#'
#' @details
#' The Kruskal-Wallis test is a non-parametric equivalent to the one way ANOVA.
#'
#' @param treatment a factor that fedines the group the response belongs to.
#' @param response a numeric variable measuring the response outcome.
#'
#' @return
#' The function returns the test result of the Kruskal Wallis test with
#' adjustment for ties and without.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#' @examples
#' attach(whiskey)
#' KW(treatment = maturity, response = grade)
KW = function(treatment, response){

  if(length(treatment) != length(response)){
    stop("Supplied vectors treatment, and response should be the same lengths.")
  }

  treatment_name = deparse(substitute(treatment))
  response_name = deparse(substitute(response))

  t = length(levels(treatment))

  mid_ranks = rank(response)
  R_i = tapply(X = rank(response),INDEX = treatment,FUN = sum)
  n_i = tapply(X = treatment,INDEX = treatment,FUN = length)
  n_i[is.na(n_i)] = 0
  n = sum(n_i)


  KW = (12/(n*(n+1)))*sum( (R_i^2)/n_i) - 3*(n+1)

  t_i = colSums(table(response,treatment))
  D = 1-sum(t_i^3-t_i)/(n^3-n)

  KW_A = KW/D
  df = t-1
  p_val = pchisq(q = KW,df = df,lower.tail = FALSE)
  p_val_A = pchisq(q = KW_A,df = df,lower.tail = FALSE)

  sig.digits = 4

  ret = list(KW_A = KW_A,p_val_A = p_val_A,p_val = p_val,D = D,t_i = t_i,KW = KW,R_i = R_i,
             response_name = response_name,treatment_name = treatment_name,df=df)
  new("KW_test", ret)

}




