#' mu_r
#'
#' \code{mu_r} returns the estimated rth moment of a vector.
#'
#' @param x a numeric vector.
#' @param r the degree moment requiring calculation
#'
#' @return
#' Returns the estimated rth moment of a vector.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#' @examples
#' x_vec = rnorm(10)
#' mu_r(x_vec,2)
mu_r = function(x,r){
  if(r==1){return(mean(x))}else{
    sum((x-mean(x))^r)/length(x)
  }

}
