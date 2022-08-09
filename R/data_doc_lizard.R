#' Lizard data
#'
#' The data come from Manly (2007) and relate to the number of ants consumed by
#' two sizes of Eastern Horned Lizards over a four month period.
#'
#' @docType data
#'
#' @usage data(lizard)
#'
#' @format A data frame with 24 rows and three columns.
#' \describe{
#'   \item{month}{the month in which the measurements were taken}
#'   \item{size}{the size of the Eastern Horned Lizards}
#'   \item{ants}{the number of ants consumed}
#' }
#'
#' @keywords datasets lizard RBD
#'
#' @references
#' Manly, B. F. J. (2007). Randomization, Bootstrap and Monte Carlo Methods in Biology, Third Edition. Boca Raton: Chapman & Hall/CRC.
#'
#' @source Manly (2007)
#'
#' @examples
#' attach(lizard)
#' gen_cor(x = ants, y = month, z = size, U = 3, V = 3, W = 1)
"lizard"
