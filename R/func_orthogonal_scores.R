#' orthogonal_scores
#'
#' \code{orthogonal_scores} returns orthogonal scores weighted by prevalence in
#' the data.
#'
#' @param x a vector of scores, either a factor or numeric.
#' @param degree the degree of orthogonal scores required.
#' @param n_strata  optional argument for indicating the number of strata to
#' apply the scores to.
#'
#' @return
#' Returns a matrix of orthogonal scores.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#' @examples
#' attach(jam)
#' orthogonal_scores(x = sweetness, degree = 2, n_strata = 8)

orthogonal_scores = function(x,degree,n_strata=1){
  o_s = matrix(poly(x = as.numeric(x),degree = degree)[,]*sqrt(length(x)),ncol = degree)
  scores = unname(colSums((table(round(o_s[,degree],10),x)>=1)*sort(unique(round(o_s[,degree],10)))))
  ret = matrix(rep(scores,n_strata),ncol=n_strata)
  return(ret)
}


