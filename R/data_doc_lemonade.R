#' Lemonade data
#'
#' The Lemonade data set comes from a Randomised Block Design. There
#' are four types of lemonades which are all tasted by five tasters.
#'
#' @docType data
#'
#' @usage data(lemonade)
#'
#' @format A data frame with 20 rows and three columns.
#' \describe{
#'   \item{rank}{the rank of the lemonade within each judging block}
#'   \item{type}{the type of lemonade that was tested}
#'   \item{taster}{the judge that was used for tasting}
#' }
#'
#' @keywords datasets lemonade RBD
#'
#' @source Thas et al. (2012, section 4.2)
#'
#' @references Thas, O., Best, D.J. and Rayner, J.C.W. (2012). Using orthogonal trend contrasts for testing ranked data with ordered alternatives. Statisticia Neerlandica, 66(4), 452-471.
#'
#' @examples
#' attach(lemonade)
#' friedman(y = rank, groups = type, blocks = taster, components = TRUE)
"lemonade"
