#' Poly
#'
#' \code{Poly} returns t-1 orthonormal scores weighted by a weights parameter.
#' The function uses Emerson Recursion.
#'
#' @param x a vector of numeric scores.
#' @param p a vector of weights corresponding to the elements of x.
#'
#' @return
#' Returns a matrix of orthomornal scores based on the weights provided.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#' Rayner, J.C.W., Thas, O. and De Boeck, B. (2008), A GENERALIZED EMERSON RECURRENCE RELATION. Australian & New Zealand Journal of Statistics, 50: 235-240.
#'
#' @export
#' @examples
#' x = 1:5
#' p = rep(0.2,5)
#' Poly(x = x, p = p)

Poly = function(x, p){

  # Errors and Warnings
  if(length(x) != length(p)) stop("The length of x not equal to length of p.")
  if(sum(p) != 1) {
    warning("Weights in p do not sum to 1. Using the ratio of the elements of p instead.")
    p = p/sum(p)
  }

  n = length(x)

  h_0 = rep(1,n)

  cm_1 = sum(x*p)
  cm_2 = sum((x-cm_1)*(x-cm_1)*p)
  h_1 = (x-cm_1)/sqrt(cm_2)

  K = matrix(nrow=n,ncol=n)
  K[1, ] = h_0
  K[2, ] = h_1

  for (i in 3:n){
    g_1 = sum(x * K[i-1, ] * K[i-1, ] * p)
    g_2 = sum(x * K[i-1, ] * K[i-2, ] * p)
    g_3 = (x - g_1) * K[i-1, ] - g_2 * K[i-2, ]
    K[i, ] = g_3/sqrt(sum(g_3 * g_3 * p))
  }
  return(K)
}


