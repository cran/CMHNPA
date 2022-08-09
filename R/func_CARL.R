#' CARL Test
#'
#' \code{CARL} returns the test statistic and p-value for the aligned RL test
#' with empirically fitted degrees of freedom.
#'
#' @details
#' This test is applicable to Latin square designs and is recommended over the
#' RL and ARL test. The test uses t+1 as the degrees of freedom of the
#' chi-squared null distribution and results in appropriate test sizes as well
#' as good power.
#'
#' @param y a numeric vector for the response variable.
#' @param treatment a vector giving the treatment type for the corresponding
#' elements of \code{y}.
#' @param block1 a vector giving the first blocking variable for the
#' corresponding elements of \code{y}.
#' @param block2 a vector giving the second blocking variable for the
#' corresponding elements of \code{y}.
#'
#' @return The \code{CARL} test statistic adjusted for ties together with the
#' associated p-value using a chi-squared distribution with t+1 degrees of
#' freedom.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @seealso [ARL()] [PARL()]
#'
#' @examples
#' attach(peanuts)
#' CARL(y = yield, treatment = treatment, block1 = row, block2 = col)
#'
#' @importFrom methods new
#'
#' @export
CARL = function(y,treatment,block1,block2){

  y_name = deparse(substitute(y))
  treatment_name = deparse(substitute(treatment))
  block1_name = deparse(substitute(block1))
  block2_name = deparse(substitute(block2))
  treatment_levels = levels(treatment)

  t = sqrt(length(y))
  row.col.effects = row_col_effect(to_align = y, rows = block1,
                                   cols = block2)
  aligned_response = y - row.col.effects

  ranks = rank(aligned_response)
  R_i.. = tapply(X = ranks, INDEX = treatment, sum)

  if(mean(diff(ranks)) == 0){
    statistic = 0
  } else {
    statistic = ( sum(R_i..^2) - (t^3*(t^2+1)^2)/4 ) / ( sum((ranks^2)/t) - t*(t^2+1)^2/4  )
  }

  pvalue = pchisq(q = statistic,df = t+1,lower.tail = F)

  # adjusted for ties
  ret = list(statistic=statistic,pvalue=pvalue,df=t+1,
             y_name = y_name, treatment_name = treatment_name,
             block1_name = block1_name, block2_name = block2_name,
             treatment_levels=treatment_levels,R_i..=R_i..
             )
  new("CARL_test", ret)

}



