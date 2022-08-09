#' Lemonade Sugar data
#'
#' Five lemonades with increasing sugar content are ranked by each of ten
#' judges. They were not permitted to give tied outcomes. This is a randomised
#' block design.
#'
#' @docType data
#'
#' @usage data(lemonade_sugar)
#'
#' @format A data frame with 50 rows and three columns.
#' \describe{
#'   \item{ranks}{the rank each lemonade based on sugar content}
#'   \item{sugar_content}{type of lemonade ordered by sugar content}
#'   \item{judge}{the judge providing the ranking}
#' }
#'
#' @keywords datasets lemonade_sugar RBD
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @examples
#' attach(lemonade_sugar)
#' gen_cor(x = ranks, y = sugar_content, U = 3, V = 3, rounding = 3)
"lemonade_sugar"
