#' Corn data
#'
#' Corn yields when grown by four different methods.
#'
#' @docType data
#'
#' @usage data(corn)
#'
#' @format A data frame with 34 rows and two columns.
#' \describe{
#'   \item{method}{one of four different methods used to grow corn}
#'   \item{outcome}{corn yield categorised into four levels}
#' }
#'
#' @keywords datasets corn
#'
#' @source Rayner and Best (2001)
#'
#' @references
#' Rayner, J.C.W. and Best, D.J. (2001). A Contingency Table Approach to Nonparametric Testing. Chapman & Hall/CRC: Boca Raton FL.
#'
#' @examples
#' attach(corn)
#' KW(treatment = method, response = outcome)
"corn"
