#' Cereal data
#'
#' Each of five breakfast cereals are ranked by ten judges, who each taste three
#' cereals. Each cereal is assessed six times.
#'
#' @details
#' The data comes from a Balanced Incomplete Block Design with the number of
#' treatments t = 5, number of blocks b = 10, with k = 3 and r = 6.
#'
#' @docType data
#'
#' @usage data(cereal)
#'
#' @format A data frame with 30 rows and three columns.
#' \describe{
#'   \item{rank}{the rank of the cereal within each judge block}
#'   \item{judge}{the judge that was used}
#'   \item{type}{the type of cereal}
#' }
#'
#' @keywords datasets cereal BIB
#'
#' @source Kutner et al. (2005, section 28.1)
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005). Applied linear statistical models (5th ed.). Boston: McGraw-Hill Irwin.
#'
#' @examples
#' attach(cereal)
#' durbin(y = rank, groups = type, blocks = judge)
"cereal"
