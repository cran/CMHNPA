#' Dynamite data
#'
#' A data set for the explosiveness of five different formulations of explosive.
#'
#' @details
#' The effect of five different formulations of an explosive mixture are
#' assessed. A batch of raw material is large enough for only five
#' formulations. Each formulation is prepared by five operators. The response
#' here is the explosiveness of dynamite formulations. This is a Latin square
#' design.
#'
#' @docType data
#'
#' @usage data(dynamite)
#'
#' @format A data frame with 25 rows and four columns.
#' \describe{
#'   \item{response}{a numeric vector for the explosiveness of the formulation with corresponding \code{treatment}, \code{batch} and \code{operator}}
#'   \item{treatment}{a factor distinguishing the formulation for the
#'   explosive mixture}
#'   \item{batch}{a factor describing the batch the raw materials come from}
#'   \item{operator}{a factor describing the operator who prepares the
#'   explosive formulation}
#' }
#'
#' @keywords datasets dynamite LSD
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @source
#' https://www.fox.temple.edu/cms/wp-content/uploads/2016/05/Randomized-Block-Design.pdf
#'
#' @examples
#' attach(dynamite)
#' CARL(y = response, treatment = treatment, block1 = batch, block2 = operator)
"dynamite"
