#' Row and column effects
#'
#' \code{row_col_effects} returns the values of the row and column effects. This
#' can be used to remove these effects from the response in a process call
#' aligning. This is particularly applicable in LSD data.
#'
#' @param to_align a numeric response vector.
#' @param rows a vector giving the row effects for \code{y}.
#' @param cols a vector giving the column effects for \code{y}.
#'
#' @return A vector of row and column effects. The response vector less this is
#' the aligned response variable.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#'
#' @examples
#' attach(peanuts)
#' row_col_effect(to_align = yield, rows = row, cols = col)
row_col_effect = function(to_align,rows,cols){
  temp_row = unname(tapply(X = to_align,INDEX = rows, mean))
  temp_col = unname(tapply(X = to_align,INDEX = cols, mean))

  return(temp_row[rows] + temp_col[cols] - mean(to_align))

}
