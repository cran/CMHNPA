#' RL Test
#'
#' \code{RL} returns the test statistic and p-value for the RL test. This test
#' is applicable to Latin square designs. The test is not recommended though as
#' block effects contaminate the response variable leading to unacceptable test
#' size.
#'
#' @param y a numericc vector for the response variable.
#' @param treatment a vector giving the treatment type for the corresponding
#' elements of \code{y}.
#' @param block1 a vector giving the first blocking variable for the
#' corresponding elements of \code{y}.
#' @param block2 a vector giving the second blocking variable for the
#' corresponding elements of \code{y}.
#'
#' @return A list containing the RL test statistic adjusted for ties together with the
#' associated p-value using a chi-squared distribution with t-1 degrees of
#' freedom.
#'
#' @seealso [ARL()] [CARL()]
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#' @examples
#' attach(peanuts)
#' RL(y = yield, treatment = treatment, block1 = row, block2 = col)
RL = function(y,treatment,block1,block2){

  t = sqrt(length(y))

  # NOT aligned calculations
  ranks = rank(y)
  R_i.. = tapply(X = ranks, INDEX = treatment, sum)

  if(mean(diff(ranks)) == 0){
    statistic = 0
  } else {
    statistic = ( sum(R_i..^2) - (t^3*(t^2+1)^2)/4 ) / ( sum((ranks^2)/t) - t*(t^2+1)^2/4  )
  }

  pvalue = pchisq(q = statistic,df = t-1,lower.tail = F)

  # adjusted for ties
  return(list(statistic=statistic,pvalue=pvalue))

}



