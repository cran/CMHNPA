#' Intelligence data
#'
#' Intelligence scores for individuals from different age groups.
#'
#' @docType data
#'
#' @usage data(intelligence)
#'
#' @format A data frame with 15 rows and two columns.
#' \describe{
#'   \item{age}{age of the respondents}
#'   \item{score}{intelligence score achieved}
#' }
#'
#' @keywords datasets intelligence
#'
#' @references
#' Rayner, J. C. W. and Best, D. J. (2001). A Contingency Table Approach to Nonparametric Testing. Boca Raton: Chapman & Hall/CRC.
#'
#' @source
#' Rayner and Best (2001, section 8.1)
#'
#' @examples
#' attach(intelligence)
#' gen_cor(x = rank(score), y = age, U = 2, V = 2)
"intelligence"
