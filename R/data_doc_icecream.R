#' Ice Cream data
#'
#'
#' @details
#' The icecream data set comes from a Balanced Incomplete Block Design. There
#' are seven vanilla ice-creams that are the same except for increasing amounts
#' of vanilla flavouring. Seven judges each taste three varieties.

#'
#' @docType data
#'
#' @usage data(icecream)
#'
#' @format A data frame with 21 rows and three columns.
#' \describe{
#'   \item{rank}{the rank of the ice cream within each judging block}
#'   \item{judge}{the judge that was used}
#'   \item{variety}{the type of ice cream that was tested}
#' }
#'
#' @keywords datasets icecream BIBD
#'
#' @source Table 5.1 in Conover (1998, p. 391).
#'
#' @references
#' Conover, W. J. (1998). Practical nonparametric statistics (3rd ed.). New York: Wiley.
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @examples
#' attach(icecream)
#' durbin(y = rank, groups = variety, blocks = judge)
"icecream"
