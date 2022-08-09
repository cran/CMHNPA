#' Strawberry data
#'
#' Strawberry growth rates for different pesticides.
#'
#' @details Pesticides are applied to strawberry plants to inhibit the growth of
#' weeds. The question is, do they also inhibit the growth of the strawberries?
#' The design is a supplemented balanced design.
#'
#' @docType data
#'
#' @usage data(strawberry)
#'
#' @format A data frame with 28 rows and four columns.
#' \describe{
#'   \item{block}{the experimental block}
#'   \item{pesticide}{the pesticide applied to the plant. Pesticide O is a control.}
#'   \item{response}{a measure of the growth rate of strawberry plants with \code{pesticide} applied.}
#'   \item{rank}{rank of the response}
#' }
#'
#' @keywords datasets strawberry
#'
#' @source Pearce (1960)
#'
#' @references Pearce, S.C. (1960). Supplemented balanced. Biometrika, 47, 263-271.
#'
#' @examples
#' attach(strawberry)
#' lm_strawberry = lm(rank~pesticide+block)
#' anova(lm_strawberry)
"strawberry"
