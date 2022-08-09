#' Saltiness data
#'
#' Three products, A, B and C, were tasted by 107 consumers who gave responses
#' ‘not salty enough’, ‘just about right saltiness’ and ‘too salty’, which were
#' then scored as 1, 2 and 3. The design is randomised blocks.
#'
#'
#' @docType data
#'
#' @usage data(saltiness)
#'
#' @format A data frame with 321 rows and three columns.
#' \describe{
#'   \item{product}{the type of product being tested}
#'   \item{scores}{the saltiness score. Scores of 1, 2 and 3 correspond to responses
#' ‘not salty enough’, ‘just about right saltiness’ and ‘too salty’}
#'   \item{participant}{the participant providing the rating}
#' }
#'
#' @keywords datasets saltiness RBD
#'
#' @source Rayner, J. C. W. and Best, D. J. (2017)
#'
#' @references
#' Rayner, J. C. W. and Best, D. J. (2017). Unconditional analogues of Cochran-Mantel-Haenszel tests. Australian & New Zealand Journal of Statistics, 59(4):485–494.
#'
#' @examples
#' attach(saltiness)
#' CMH(treatment = product, response = scores, strata = participant,
#' test_OPA = FALSE, test_MS = FALSE, test_C = FALSE)
"saltiness"
