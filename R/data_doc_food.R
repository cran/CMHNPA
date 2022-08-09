#' Food data
#'
#' Categorical responses made by 15 subjects for two different prices of the
#' same food product. The data is from a CMH design.
#'
#' @docType data
#'
#' @usage data(food)
#'
#' @format A data frame with 30 rows and three columns.
#' \describe{
#'   \item{price}{a factor showing two price points for the product}
#'   \item{decision}{a factor indicating the decision of either buy, undecided, or not_buy the product}
#'   \item{subject}{a factor showing the individual making a decison about the product}
#' }
#'
#' @keywords datasets food CMH
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @examples
#' attach(food)
#' b_hj = matrix(1:3,ncol=15,nrow=3)
#' CMH(treatment = price, response = decision, strata = subject, b_hj = b_hj,
#' test_OPA = FALSE, test_C = FALSE)
"food"
