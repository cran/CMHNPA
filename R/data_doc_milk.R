#' Milk data
#'
#' In each of four lactation periods each of four cows are fed a different diet.
#' There is a washout period so previous diet does not affect future results.
#' A 4 × 4 Latin square is used to assess how diet affects milk production.
#'
#' @docType data
#'
#' @usage data(milk)
#'
#' @format A data frame with 16 rows and four columns.
#' \describe{
#'   \item{production}{a numerical measure of the milk production for the given diet, cow, and period}
#'   \item{diet}{a factor showing which diet the cow was administered}
#'   \item{cow}{a factor indicating the cow}
#'   \item{period}{a factor showing the relevant period}
#' }
#'
#' @keywords datasets milk LSD
#'
#'
#' @source
#' https://www.stat.purdue.edu/~yuzhu/stat514fall05/Lecnot/latinsquarefall05.pdf
#'
#' @examples
#' attach(milk)
#' CARL(y = production, treatment = diet, block1 = cow, block2 = period)
"milk"
