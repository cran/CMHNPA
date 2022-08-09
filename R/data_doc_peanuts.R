#' Peanuts data
#'
#' A plant biologist conducted an experiment to compare the yields of four
#' varieties of peanuts, denoted as A, B, C, and D. A plot of land was divided
#' into 16 subplots (four rows and four columns).
#'
#' @docType data
#'
#' @usage data(peanuts)
#'
#' @format A data frame with 16 rows and four columns.
#' \describe{
#'   \item{yield}{the peanut yield}
#'   \item{treatment}{the variety of peanut plant}
#'   \item{row}{the row the plants were grown in of the plot}
#'   \item{col}{the column the plants were grown in of the plot}
#' }
#'
#' @keywords datasets peanuts LSD
#'
#' @source http://www.math.montana.edu/jobo/st541/sec3c.pdf
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @examples
#' attach(peanuts)
#' CARL(y = yield, treatment = treatment, block1 = row, block2 = col)
"peanuts"
